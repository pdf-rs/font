use super::*;
use std::cmp::{max, min};
use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum VariantGlyph {
    Replacement(u16),
    Constructable(Direction, Vec<GlyphInstruction>),
}

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Horizontal,
    Vertical
}

#[derive(Debug, Clone, Copy)]
pub struct GlyphInstruction {
    pub gid: u16,
    pub overlap: u16,
}

type FontUnit = u16;

impl MathVariants {
    pub fn vert_variant(&self, glyph: u16, size: u32) -> VariantGlyph {
        let construction = match self.vert_glyph_construction.get(&glyph) {
            Some(construction) => construction,
            None => return VariantGlyph::Replacement(glyph)
        };
        
        // Check if any replacement glyphs meet the requirement.
        for record in &construction.variants {
            if record.advance_measurement as u32 >= size {
                return VariantGlyph::Replacement(record.variant_glyph);
            }
        }

        // Otherwise we check for constructable glyphs.
        // In the scenario that none of the replacement glyphs match the desired
        // advance, and there is no constructable glyph, we return the largest
        // replacement glyph.
        let assembly = match construction.glyph_assembly {
            None => {
                trace!("no constructable glyphs found"); 
                return VariantGlyph::Replacement(construction.variants.last().unwrap().variant_glyph);
            },
            Some(ref assembly) => assembly,
        };

        // Calculate the metrics for a variant at least as large as size.
        let (repeats, diff_ratio) = self.smallest_upper_bound(&assembly.parts, size);
        let instructions = self.construct_glyphs(&assembly.parts, repeats, diff_ratio);
        VariantGlyph::Constructable(Direction::Vertical, instructions)
    }

    pub fn horz_variant(&self, glyph: u16, size: u32) -> VariantGlyph {
        let construction = match self.horiz_glyph_construction.get(&glyph) {
            Some(construction) => construction,
            None => return VariantGlyph::Replacement(glyph)
        };

        // if the first replacement is to big, use the current glyph
        if construction.variants[0].advance_measurement as u32 >= size {
            return VariantGlyph::Replacement(glyph);
        }

        // Check for replacement glyphs that meet the desired size first.
        // We want the largest variant that is _smaller_ than the given size.
        if let Some((replacement, _)) = construction.variants.iter().tuples().find(|(_, b)| b.advance_measurement as u32 >= size) {
            return VariantGlyph::Replacement(replacement.variant_glyph);
        }

        if let Some(ref assembly) = construction.glyph_assembly {
            // Calculate the metrics for a variant at least as large as size.
            if let Some((repeats, diff)) = self.greatest_lower_bound(&assembly.parts, size) {
                let instructions = self.construct_glyphs(&assembly.parts, repeats, diff);
                return VariantGlyph::Constructable(Direction::Horizontal, instructions);
            }
        }

        let backup = construction.variants.last().map(|r| r.variant_glyph).unwrap_or(glyph);
        trace!("constructable glyphs are too large");
        VariantGlyph::Replacement(backup)
   }

    /// This method will look for a successor of a given glyph if there
    /// exits one.  This is how operators like `\int` and `\sum` become
    /// larger while in Display mode.

    pub fn successor(&self, glyph: u16) -> u16 {
        // If there are no variant glyphs, return itself.
        let variants = match self.vert_glyph_construction.get(&glyph) {
            None => return glyph,
            Some(g) => g,
        };

        // First check to see if any of the replacement glyphs meet the requirement.
        // It is assumed that the glyphs are in increasing advance.
        match variants.variants.get(1) {
            Some(succ) => succ.variant_glyph,
            None => glyph,
        }
    }

    fn construct_glyphs(&self, parts: &[GlyphPartRecord], repeats: u16, diff_ratio: f64) -> Vec<GlyphInstruction> {
        // Construct the variant glyph
        let mut prev_connector = 0;
        let mut first = true;
        trace!("diff: {:?}, repeats: {}", diff_ratio, repeats);

        let mut variants = Vec::with_capacity(repeats as usize + 3);
        for glyph in parts {
            let repeat = if glyph.optional() { repeats } else { 1 };
            for _ in 0..repeat {
                let overlap = if first {
                    first = false;
                    0
                } else {
                    // linear interpolation
                    //  d * max_overlap + (1 - d) * MIN_CONNECTOR_OVERLAP
                    let max = self.max_overlap(prev_connector, glyph);
                    let overlap = (1.0 - diff_ratio) * max as f64
                        + diff_ratio * self.min_connector_overlap as f64;
                    overlap as u16
                };
                prev_connector = min(glyph.end_connector_length, glyph.full_advance / 2);

                variants.push(GlyphInstruction {
                    gid: glyph.glyph_id,
                    overlap: overlap
                });
            }
        }

        variants
    }

    /// Construct the smallest variant that is larger than the given size.
    /// With the number of glyphs required to construct the variant is larger
    /// than `ITERATION_LIMIT` we return `None`.
    fn smallest_upper_bound(&self, parts: &[GlyphPartRecord], size: u32) -> (u16, f64) {
        let (small, large) = self.advance_without_optional(parts);
        if small < size {
            trace!("using smallest variant glyph, {} <= smallest <= {}", small, large);
            return (0, 0.0)
        }

        // Otherwise, check the next largest variant with optional glyphs included.
        let (mut small, mut large, opt_small, opt_large) = self.advance_with_optional(parts);
        if large >= size {
            let diff_ratio = f64::from(size - small) / f64::from(large - small);
            trace!("Optional glyphs: 1, Difference ratio: {:2}", diff_ratio);
            return (1, diff_ratio);
        } 

        // We need to find the smallest integer k that satisfies:
        //     large + k * opt_large >= size
        // This is solved by:
        //     (size - large) / opt_large <= k
        // So take k = ceil[ (size - large) / opt_large ]
        let k = u32::from( (size - large) / opt_large ) + 1;
        trace!("k = ({} - {}) / {} = {}", size, large, opt_large, k);
        small += k * opt_small;
        large += k * opt_large;
        trace!("new size: {} <= advance <= {}", small, large);

        //  A---o---B, percentage: (o - A) / (B - A)
        // o  A-----B, percentage: 0 (smallest glyph).
        // Need small + diff_ratio * (opt_large - opt_small) = size
        if small >= size {
            return (k as u16 + 1, 0.into());
        }

        let difference_ratio = f64::from(size - small) / f64::from(large - small);
        trace!("Difference ratio: ({:?} - {:?}) / ({:?} - {:?}) = {:?}",
            size, small, large, small, difference_ratio);
        trace!("New size: {} + {} * {} * {}", small, k, difference_ratio, opt_large - opt_small);
        (k as u16 + 1, difference_ratio)
    }

    /// Measure the _largest_ a glyph construction _smaller_ than the given size. 
    /// If all constructions are larger than the given size, return `None`.
    /// Otherwise return the number of optional glyphs required and the difference
    /// ratio to obtain the desired size.
    fn greatest_lower_bound(&self,
        parts: &[GlyphPartRecord], 
        size: u32) 
    -> Option<(u16, f64)> {
        let (small, large) = self.advance_without_optional(parts);
        if small >= size {
            trace!("all constructable glyphs are too large, smallest: {}", small);
            return None;
        }

        // Otherwise calculate the size of including one set of optional glyphs.
        let (mut ssmall, mut llarge, opt_small, opt_large) = self.advance_with_optional(parts);

        // If the smallest constructable with optional glyphs is too large we
        // use no optional glyphs.
        // TODO: Do something better if `large == small`.
        if ssmall >= size {
            let diff_ratio = f64::from(size - small) / f64::from(large - small);
            let diff_ratio = diff_ratio.min(1.0);
            trace!("optional glyphs make construction too large, using none");
            trace!("diff_ratio = {:.2}", diff_ratio);
            return Some((0, diff_ratio));
        }

        // Determine the number of additional optional glyphs required to achieve size.
        // We need to find the smallest integer k such that:
        //     ssmall + k*opt_small >= size
        // This is solved by:
        //     (size - ssmall) / opt_small <= k
        // Which is solved by: k = floor[ (size - smmal) / opt_small ]
        // Since we round towards zero, floor is not necessary.
        let k = (size - ssmall) / opt_small;
        trace!("k = ({} - {})/ {} = {}", size, ssmall, opt_small, k);

        ssmall += k * opt_small;
        llarge += k * opt_large;
        let diff_ratio = f64::from(size - ssmall) / f64::from(llarge - ssmall);
        let diff_ratio = diff_ratio.min(1.0).max(0.0);

        trace!("{} <= advance <= {}", ssmall, llarge);
        trace!("Difference ratio: {}", diff_ratio);
        Some((k as u16 + 1, diff_ratio))
    }

    /// Calculate the advance of the smallest variant with exactly one set of optional
    /// connectors. This returns a tuple: the first element states the advance of a
    /// variant with one set of optional connectors, the second element states the
    /// increase in advance for each additional connector.
    fn advance_with_optional(&self, parts: &[GlyphPartRecord]) -> (u32, u32, u32, u32) {
        let mut advance_small = 0;
        let mut advance_large = self.min_connector_overlap as u32;
        let mut connector_small = 0;
        let mut connector_large = 0;
        let mut prev_connector = 0;

        // Calculate the legnth with exactly one connector
        for glyph in parts {
            let overlap = self.max_overlap(prev_connector, glyph);
            advance_small += (glyph.full_advance - overlap) as u32;
            advance_large += (glyph.full_advance - self.min_connector_overlap) as u32;
            prev_connector = min(glyph.end_connector_length, glyph.full_advance / 2);

            // Keep record of the advance each additional connector adds
            if glyph.optional() {
                let overlap = self.max_overlap(glyph.start_connector_length, glyph);
                connector_small += (glyph.full_advance - overlap) as u32;
                connector_large += (glyph.full_advance - self.min_connector_overlap) as u32;
            }
        }

        trace!("variant with optional glyphs: {} <= advance <= {}", advance_small, advance_large);
        trace!("advance from optional glyphs: {} <= advance <= {}",
            connector_small, connector_large);
        (advance_small, advance_large, connector_small, connector_large)
    }

    fn advance_without_optional(&self, parts: &[GlyphPartRecord]) -> (u32, u32) {
        let mut advance_small = 0;
        let mut advance_large = self.min_connector_overlap as u32;
        let mut prev_connector = 0;

        for glyph in parts.iter().filter(|glyph| glyph.optional()) {
            let overlap = self.max_overlap(prev_connector, glyph);
            advance_small += (glyph.full_advance - overlap) as u32;
            advance_large += (glyph.full_advance - self.min_connector_overlap) as u32;
            prev_connector = min(glyph.end_connector_length, glyph.full_advance / 2);
        }

        (advance_small, advance_large)
    }

    fn max_overlap(&self, left: FontUnit, right: &GlyphPartRecord) -> FontUnit {
        let overlap = min(left, right.start_connector_length);
        let overlap = min(overlap, right.full_advance / 2);
        max(overlap, self.min_connector_overlap)
    }
}
