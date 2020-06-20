use crate::{Font, GlyphId};
use pathfinder_builder::{Transform2F, Vector2F, Outline};
use pathfinder_renderer::{
    scene::{Scene, DrawPath},
    paint::Paint,
};
use pathfinder_color::ColorU;

#[cfg(feature="svg")]
use crate::SvgGlyph;

#[cfg(feature="svg")]
enum Glyph<'a> {
    Simple(Outline),
    Complex(&'a SvgGlyph)
}
#[cfg(not(feature="svg"))]
enum Glyph {
    Simple(Outline),
}

struct SuperFont {
    fonts: Vec<Box<dyn Font>>
}
struct Grapheme<'a> {
    font: &'a dyn Font,
    gid: u32,
    advance: Vector2F
}
/*
impl SuperFont {
    pub fn new(fonts: Vec<Box<dyn Font>>) -> SuperFont {
        SuperFont { fonts }
    }
    pub fn shape(&self, text: &str) {
        let mut current_font = &self.fonts[0]:
        let mut remaining = text;
        let mut done = Vec::new();
        loop {

            // keep going
            let mut chars = remaining.chars();
            for c in chars {
                if let Some(gid) = 
            }
        }
}*/

pub fn line(font: &dyn Font, font_size: f32, text: &str) -> Scene {
    let mut last_gid = None;
    let mut offset = Vector2F::default();

    let mut gids: Vec<GlyphId> = text.chars().map(|c| font.gid_for_unicode_codepoint(c as u32).unwrap_or(font.get_notdef_gid())).collect();
    if let Some(gsub) = font.get_gsub() {
        let mut substituted_gids = Vec::new();
        let mut pos = 0;
    'a: while let Some(&first) = gids.get(pos) {
            pos += 1;
            if let Some(subs) = gsub.substitutions(first) {
            for (sub, glyph) in subs {
                    if let Some(len) = sub.matches(gids[pos ..].iter().cloned()) {
                        substituted_gids.push(glyph);
                        pos += len;
                        continue 'a;
                    }
                }
            }

            substituted_gids.push(first);
        }

        gids = substituted_gids;
    }
    let glyphs: Vec<_> = gids.iter()
        .filter_map(|&gid| font.glyph(gid).map(|glyph| (gid, glyph)))
        .map(|(gid, glyph)| {
            if let Some(left) = last_gid.replace(gid) {
                offset = offset + Vector2F::new(font.kerning(left, gid), 0.0);
            }
            let p = offset;
            offset = offset + glyph.metrics.advance;

            #[cfg(feature="svg")]
            let glyph = match font.svg_glyph(gid) {
                None => Glyph::Simple(glyph.path),
                Some(svg) => Glyph::Complex(svg)
            };

            (glyph, p)
        })
        .collect();
    
    let bbox = font.bbox().expect("no bbox");
    let last_p = match glyphs.last() {
        Some((_, p)) => p,
        _ => return Scene::new()
    };
    let origin = Vector2F::new(0., -bbox.origin().y());
    let width = (last_p.x() + bbox.size().x()) * font.font_matrix().m11();
    let height = bbox.size().y() * font.font_matrix().m22();
    let mut scene = Scene::new();
    
    let tr = Transform2F::from_scale(Vector2F::splat(font_size))
        * Transform2F::from_translation(Vector2F::new(0., height))
        * Transform2F::from_scale(Vector2F::new(1.0, -1.0))
        * font.font_matrix();

    let paint = scene.push_paint(&Paint::from_color(ColorU::black()));
    for (glyph, p) in glyphs {
        let transform = tr * Transform2F::from_translation(p + origin);

        #[cfg(feature="svg")]
        {
            match glyph {
                Glyph::Simple(mut path) => {
                    path.transform(&transform);
                    let draw_path = DrawPath::new(path, paint);
                    scene.push_draw_path(draw_path);
                }
                Glyph::Complex(glyph) => {
                    glyph.draw_to(&mut scene, transform);
                }
            }
        }
        #[cfg(not(feature="svg"))]
        {
            let mut path = glyph.path;
            path.transform(&transform);
            let draw_path = DrawPath::new(path, paint);
            scene.push_draw_path(draw_path);
        }
    }
    
    scene
}

/*
1. split into paragraphs
2. split into runs
3. render each run

pub fn paragraphs(text: &str) {
    let info = BidiInfo::new(text, None);
    for paragraph in &info.paragraphs {
        for (level, run) in info.visual_runs(para, line: Range<usize>)
}
*/
