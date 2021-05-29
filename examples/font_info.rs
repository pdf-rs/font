//use jemallocator::Jemalloc;

//#[global_allocator]
//static ALLOC: Jemalloc = Jemalloc;

use font::{font_info, Font};
use std::fs::File;
use std::path::Path;
use walkdir::WalkDir;
use env_logger;
use memmap::Mmap;

fn main() {
    env_logger::init();
    
    let args: Vec<_> = std::env::args().collect();
    let font_dir = Path::new(&args[1]);
    let files: Vec<_> = WalkDir::new(font_dir).into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.metadata().map(|m| m.is_file()).unwrap_or(false))
        .collect();

    for entry in &files {
        println!("{}", entry.path().to_str().unwrap());
        let file = File::open(entry.path()).unwrap();
        let mmap = unsafe {
            Mmap::map(&file).unwrap()
        };

        if let Some(info) = font_info(&mmap) {
            println!("{:#?}", info);
        }
    }
}
