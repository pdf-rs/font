use font::{Font, parse, GlyphId};
use std::env;
use std::fs::{self, File};
use std::io::BufWriter;
use std::error::Error;
use pathfinder_export::{Export, FileFormat};
use pathfinder_builder::{Transform2F, Vector2F, Outline};
use pathfinder_renderer::{
    scene::{Scene, DrawPath},
    paint::Paint,
};
use pathfinder_color::ColorU;
use font::SvgGlyph;
use svg_draw::draw_glyph;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    
    //let mut surface = Svg::new(BufWriter::new(File::create("glyph.svg")?), 200.0, 200.0);
    let file = args.get(1).expect("no filename given");
    let text = args.get(2).expect("no text given");
    
    let data = fs::read(file).expect("can't read specified file");
    let font = parse(&data);
    line(&*font, 20., &text)
        .export(&mut BufWriter::new(File::create("font.svg").unwrap()), FileFormat::SVG)
        .unwrap();
    
    Ok(())
}

enum Glyph<'a> {
    Simple(Outline),
    Complex(&'a SvgGlyph)
}

struct SuperFont {
    fonts: Vec<Box<dyn Font>>
}
struct Grapheme<'a> {
    font: &'a dyn Font,
    gid: u32,
    advance: Vector2F
}

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

        match glyph {
            Glyph::Simple(mut path) => {
                path.transform(&transform);
                let draw_path = DrawPath::new(path, paint);
                scene.push_draw_path(draw_path);
            }
            Glyph::Complex(glyph) => {
                draw_glyph(glyph, &mut scene, transform);
            }
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
