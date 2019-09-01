use vector::{Surface, PathStyle, Vector, Transform, PathBuilder, Outline};
use crate::{Font, GlyphId};


pub fn line<S: Surface>(font: &dyn Font<S::Outline>, font_size: f32, text: &str, style: PathStyle, baseline: Option<PathStyle>) -> S {
    let mut last_gid = None;
    let mut offset = Vector::default();

    let mut gids: Vec<GlyphId> = text.chars().map(|c| font.gid_for_unicode_codepoint(c as u32).unwrap_or(font.get_notdef_gid())).collect();
    if let Some(gsub) = font.get_gsub() {
        let mut substituted_gids = Vec::new();
        let mut pos = 0;
    'a: while let Some(&first) = gids.get(pos) {
            pos += 1;
            if let Some(subs) = gsub.substitutions(first) {
            'b: for (sub, glyph) in subs {
                    if let Some(len) = sub.matches(&gids[pos ..]) {
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
                offset = offset + Vector::new(font.kerning(left, gid), 0.0);
            }
            let p = offset;
            offset = offset + glyph.metrics.advance;
            (glyph, p)
        })
        .collect();
    
    let bbox = font.bbox().expect("no bbox");
    let last_p = match glyphs.last() {
        Some((_, p)) => p,
        _ => return S::new(Vector::default())
    };
    let origin = Vector::new(0., -bbox.origin().y());
    let width = (last_p.x() + bbox.size().x()) * font.font_matrix().m11();
    let height = bbox.size().y() * font.font_matrix().m22();
    let mut surface = S::new(Vector::new(width * font_size, font_size * height));
    
    let tr = Transform::from_scale(Vector::splat(font_size))
            * Transform::from_translation(Vector::new(0., height))
            * Transform::from_scale(Vector::new(1.0, -1.0))
            * font.font_matrix();
    
    if let Some(style) = baseline {
        let style = surface.build_style(style);
        let mut p = PathBuilder::new();
        p.move_to(origin);
        p.line_to(origin + offset);
        let o: S::Outline = p.into_outline();
        surface.draw_path(o.transform(tr), &style);
    }
    let style = surface.build_style(style);
    for (glyph, p) in glyphs {
        let transform = tr * Transform::from_translation(p + origin);
        surface.draw_path(glyph.path.transform(transform), &style);
    }
    
    surface
}
