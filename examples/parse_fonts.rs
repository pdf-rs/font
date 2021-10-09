//use jemallocator::Jemalloc;

//#[global_allocator]
//static ALLOC: Jemalloc = Jemalloc;

use font::{parse, Font};
use std::fs;
use std::path::Path;
use std::panic::catch_unwind;
use pathfinder_content::outline::Outline;
use walkdir::WalkDir;
use env_logger;

fn main() {
    env_logger::init();
    
    let args: Vec<_> = std::env::args().collect();
    let loops: usize = args.get(2).map(|s| s.parse().expect("not a number")).unwrap_or(1);

    let font_dir = Path::new(&args[1]);
    let files: Vec<_> = WalkDir::new(font_dir).into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.metadata().map(|m| m.is_file()).unwrap_or(false))
        .collect();
    
    for _ in 0 .. loops {
        for entry in &files {
            eprint!("parsing {:100} .. ", entry.path().to_str().unwrap());
            let data = fs::read(entry.path()).expect("can't read file");
            
            match catch_unwind(|| parse(&data)) {
                Ok(Ok(font)) => {
                    eprintln!("OK");
                }
                Ok(Err(e)) => {
                    eprintln!("returned Err({:?})", e);
                }
                Err(e) => {
                    eprintln!("FAIL");
                }
            }
        }
    }
}
