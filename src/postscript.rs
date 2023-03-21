use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::borrow::{Cow};
use tuple::{TupleElements, Map};
use decorum::R32;
use indexmap::set::IndexSet;
use crate::{R, FontError};
use crate::parsers::{token, Token, comment, space, hex_string};


#[cfg(feature="unstable")]
use slotmap::SlotMap;

#[cfg(not(feature="unstable"))]
use slotmap::DenseSlotMap as SlotMap;

new_key_type! {
    pub struct DictKey;
    pub struct ArrayKey;
    pub struct StringKey;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct LitKey(usize);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Item {
    Null,
    Bool(bool),
    Int(i32),
    Real(R32),
    Dict(DictKey),
    Array(ArrayKey),
    String(StringKey),
    Name(LitKey),
    Literal(LitKey),
    Operator(Operator),
    Mark,
    File
}

#[cfg(feature="unstable")]
fn recursive_trap(ptr: usize, f: &mut fmt::Formatter<'_>, func: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result) -> fmt::Result {
    use std::cell::RefCell;
    
    #[thread_local]
    static STACK: RefCell<Vec<usize>> = RefCell::new(Vec::new());
    {
        let stack = &mut *STACK.borrow_mut();
        if stack.contains(&ptr) {
            return write!(f, "...");
        }
        stack.push(ptr);
    }
    
    func(f)?;
    
    let ptr2 = STACK.borrow_mut().pop().unwrap();
    assert_eq!(ptr, ptr2);
    Ok(())
}

#[derive(Copy, Clone)]
pub struct RefDict<'a> {
    vm: &'a Vm,
    dict: &'a Dictionary
}
impl<'a> RefDict<'a> {
    pub fn iter(&self) -> impl Iterator<Item=(RefItem<'a>, RefItem<'a>)> {
        let vm = self.vm;
        self.dict.iter().map(move |(k, v)| (RefItem::new(vm, *k), RefItem::new(vm, *v)))
    }
    pub fn get(&self, key: &str) -> Option<RefItem<'a>> {
        self.vm.literals.get_full(key.as_bytes())
            .and_then(|(index, _)| self.dict.get(&Item::Literal(LitKey(index))))
            .map(|&item| RefItem::new(self.vm, item))
    }
    pub fn get_int(&self, key: &str) -> Option<i32> {
        self.get(key).and_then(|i| i.as_int())
    }
    pub fn get_str(&self, key: &str) -> Option<&'a str> {
        self.get(key).and_then(|i| i.as_str())
    }
    pub fn get_dict(&self, key: &str) -> Option<RefDict<'a>> {
        self.get(key).and_then(|i| i.as_dict())
    }
    pub fn get_array(&self, key: &str) -> Option<RefArray<'a>> {
        self.get(key).and_then(|i| i.as_array())
    }
    pub fn len(&self) -> usize {
        self.dict.len()
    }
    pub fn string_entries(&self) -> impl Iterator<Item=(&'a str, RefItem<'a>)> {
        self.iter().filter_map(|(key, val)| {
            match key {
                RefItem::Literal(s) => std::str::from_utf8(s).ok().map(|s| (s, val)),
                _ => None
            }
        })
    }
}


#[cfg(feature="unstable")]
impl<'a> fmt::Debug for RefDict<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let vm = self.vm;
        recursive_trap(self.dict as *const _ as usize, f, move |f| {
            f.debug_map().entries(
                self.dict.iter()
                .map(move |(&k, &v)| (RefItem::new(vm, k), RefItem::new(vm, v)))
            )
            .finish()
        })
    }
}

#[derive(Copy, Clone)]
pub struct RefArray<'a> {
    vm: &'a Vm,
    array: &'a Array
}
impl<'a> RefArray<'a> {
    pub fn iter(&self) -> impl Iterator<Item=RefItem<'a>> {
        let vm = self.vm;
        self.array.iter()
            .map(move |&item| (RefItem::new(vm, item)))
    }
    pub fn get(&self, index: usize) -> Option<RefItem<'a>> {
        self.array.get(index)
            .map(|&item| RefItem::new(self.vm, item))
    }
    pub fn len(&self) -> usize {
        self.array.len()
    }
}

#[cfg(feature="unstable")]
impl<'a> fmt::Debug for RefArray<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let vm = self.vm;
        recursive_trap(self.array as *const _ as usize, f, move |f| {
            f.debug_list().entries(self.array.iter().map(move |&item| RefItem::new(vm, item)))
            .finish()
        })
    }
}

pub enum RefItem<'a> {
    Recursive,
    Null,
    Bool(bool),
    Int(i32),
    Real(f32),
    Dict(RefDict<'a>),
    Array(RefArray<'a>),
    String(&'a [u8]),
    Name(&'a [u8]),
    Literal(&'a [u8]),
    Operator(Operator),
    Mark,
    File
}
fn print_string(s: &[u8]) -> Cow<str> {
    String::from_utf8_lossy(&s[.. s.len().min(100)])
}
impl<'a> RefItem<'a> {
    fn new(vm: &'a Vm, item: Item) -> RefItem<'a> {
        match item {
            Item::Null => RefItem::Null,
            Item::Bool(b) => RefItem::Bool(b),
            Item::Int(i) => RefItem::Int(i),
            Item::Real(r) => RefItem::Real(r.into()),
            Item::Dict(key) => RefItem::Dict(RefDict { vm, dict: vm.get_dict(key) }),
            Item::Array(key) => RefItem::Array(RefArray { vm, array: vm.get_array(key) }),
            Item::String(key) => RefItem::String(vm.get_string(key)),
            Item::Name(key) => RefItem::Name(vm.get_lit(key)),
            Item::Literal(key) => RefItem::Literal(vm.get_lit(key)),
            Item::Operator(op) => RefItem::Operator(op),
            Item::Mark => RefItem::Mark,
            Item::File => RefItem::File
        }
    }
    pub fn as_dict(&self) -> Option<RefDict<'a>> {
        match *self {
            RefItem::Dict(dict) => Some(dict),
            _ => None
        }
    }
    pub fn as_array(&self) -> Option<RefArray<'a>> {
        match *self {
            RefItem::Array(array) => Some(array),
            _ => None
        }
    }
    pub fn as_bytes(&self) -> Option<&'a [u8]> {
        match *self {
            RefItem::String(bytes) |
            RefItem::Name(bytes) |
            RefItem::Literal(bytes) => Some(bytes),
            _ => None
        }
    }
    pub fn as_str(&self) -> Option<&'a str> {
        self.as_bytes().and_then(|b| std::str::from_utf8(b).ok())
    }
    pub fn as_f32(&self) -> Option<f32> {
        match *self {
            RefItem::Int(i) => Some(i as f32),
            RefItem::Real(r) => Some(r.into()),
            _ => None
        }
    }
    pub fn as_int(&self) -> Option<i32> {
        match *self {
            RefItem::Int(i) => Some(i),
            _ => None
        }
    }
    
}

impl<'a> fmt::Debug for RefItem<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RefItem::Recursive => write!(f, "..."),
            RefItem::Null => write!(f, "Null"),
            RefItem::Mark => write!(f, "Mark"),
            RefItem::File => write!(f, "File"),
            RefItem::Operator(op) => op.fmt(f),
            RefItem::Bool(b) => b.fmt(f),
            RefItem::Int(i) => i.fmt(f),
            RefItem::Real(r) => r.fmt(f),
            #[cfg(feature="unstable")]
            RefItem::Dict(dict) => dict.fmt(f),
            #[cfg(feature="unstable")]
            RefItem::Array(array) => array.fmt(f),
            RefItem::String(s) => write!(f, "({:?})", print_string(s)),
            RefItem::Literal(s) => write!(f, "/{:?}", print_string(s)),
            RefItem::Name(s) => write!(f, "{:?}", print_string(s)),
            _ => Ok(())
        }
    }
}

type Array = Vec<Item>;
type Dictionary = HashMap<Item, Item>;

#[derive(Debug)]
struct Mode {
    write: bool,
    execute: bool,
    read: bool
}
impl Mode {
    fn all() -> Mode {
        Mode {
            write: true,
            execute: true,
            read: true
        }
    }
    fn read_only(&mut self) {
        self.write = false;
    }
    fn execute_only(&mut self) {
        self.read = false;
        self.write = false;
    }
    fn noaccess(&mut self) {
        self.write = false;
        self.execute = false;
        self.read = false;
    }
}

macro_rules! operators {
    ($($key:expr => $name:ident),*; $($key2:expr => $name2:ident),*) => (
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub enum Operator {
            $($name),*
        }
        
        const OPERATOR_MAP: &[(&'static str, Operator)] = &[
            $( ($key, Operator::$name), )*
            $( ($key2, Operator::$name2), )*
        ];
    )
}

operators!{
    "abs"           => Abs,
    "add"           => Add,
    "array"         => Array,
    "begin"         => Begin,
    "currentdict"   => CurrentDict,
    "currentfile"   => CurrentFile,
    "cleartomark"   => ClearToMark,
    "closefile"     => CloseFile,
    "count"         => Count,
    "copy"          => Copy,
    "cvx"           => Cvx,
    "definefont"    => DefineFont,
    "for"           => For,
    "def"           => Def,
    "dict"          => Dict,
    "dup"           => Dup,
    "end"           => End,
    "exch"          => Exch,
    "executeonly"   => ExecuteOnly,
    "eexec"         => Eexec,
    "exec"          => Exec,
    "eq"            => Eq,
    "false"         => False,
    "get"           => Get,
    "if"            => If,
    "ifelse"        => IfElse,
    "index"         => Index,
    "internaldict"  => InternalDict,
    "known"         => Known,
    "length"        => Length,
    "maxlength"     => MaxLength,
    "mark"          => Mark,
    "mul"           => Mul,
    "noaccess"      => NoAccess,
    "not"           => Not,
    "pop"           => Pop,
    "put"           => Put,
    "readonly"      => ReadOnly,
    "readstring"    => ReadString,
    "string"        => String,
    "sub"           => Sub,
    "true"          => True,
    "]"             => EndArray;
    
    "["             => Mark
}

pub struct Input<'a> {
    data:   &'a [u8],
    open:   bool
}
impl<'a> Input<'a> {
    pub fn new(data: &'a [u8]) -> Input<'a> {
        Input { data, open: true }
    }
    pub fn len(&self) -> usize {
        self.data.len()
    }
    fn take(&mut self, n: usize) -> &'a [u8] {
        let (first, second) = self.data.split_at(n);
        self.data = second;
        first
    }
    fn advance(&mut self, n: usize) {
        self.data = self.data.get(n ..).unwrap_or(&[]);
    }
    // true if buf.len() bytes were read
    // false if EOF (buf will be truncated)
    fn read_to(&mut self, buf: &mut Vec<u8>) -> bool {
        if self.len() >= buf.len() {
            let len = buf.len();
            // normal case 
            buf.copy_from_slice(self.take(len));
            true
        } else {
            let len = self.len();
            buf.truncate(len);
            buf.copy_from_slice(self.take(len));
            false
        }
    }
    fn try_parse<T, E>(&mut self, parser: impl Fn(&'a [u8]) -> Result<(&'a [u8], T), E>) -> Option<T> {
        match parser(self.data) {
            Ok((i, t)) => {
                let n = self.data.len() - i.len();
                self.advance(n);
                Some(t)
            },
            Err(_) => {
                let slice = &self.data[.. self.data.len().min(20)];
                trace!("input: {:?} {:?}", String::from_utf8_lossy(slice), slice);
                None
            }
        }
    }
}
impl<'a> Deref for Input<'a> {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        self.data
    }
}
pub struct Vm {
    dicts:      SlotMap<DictKey, (Dictionary, Mode)>,
    arrays:     SlotMap<ArrayKey, (Array, Mode)>,
    strings:    SlotMap<StringKey, (Vec<u8>, Mode)>,
    literals:   IndexSet<Vec<u8>>,
    fonts:      HashMap<String, DictKey>,
    dict_stack: Vec<DictKey>,
    stack:      Vec<Item>,
    internal_dict: DictKey
}
impl Vm {
    pub fn new() -> Vm {
        let mut dicts = SlotMap::with_key();
        let error_dict_key = dicts.insert((Dictionary::new(), Mode { read: true, write: true, execute: false }));
        let internal_dict_key = dicts.insert((Dictionary::new(), Mode { read: true, write: true, execute: false }));
        
        let mut vm = Vm { 
            dicts,
            arrays: SlotMap::with_key(),
            strings: SlotMap::with_key(),
            literals: IndexSet::new(),
            fonts: HashMap::new(),
            dict_stack: Vec::new(),
            stack: Vec::new(),
            internal_dict: internal_dict_key
        };
        let mut system_dict: Dictionary = OPERATOR_MAP.iter()
            .map(|&(name, op)| (Item::Literal(vm.make_lit(name.as_bytes())), Item::Operator(op))) 
            .collect();
        
        let mut user_dict = Dictionary::new();
        {
            let font_dict = vm.make_dict(Dictionary::new(), Mode { write: true, execute: false, read: true });
            user_dict.insert(Item::Literal(vm.make_lit(b"FontDirectory")), Item::Dict(font_dict));
            
            // StandardEncoding …
            use crate::cff::{STANDARD_STRINGS, STANDARD_ENCODING};
            let arr = STANDARD_ENCODING.iter().map(|&sid|
                Item::Literal(vm.make_lit(STANDARD_STRINGS[sid as usize].as_bytes()))
            ).collect();
            let standard_encoding = vm.make_array(arr, Mode { write: false, execute: false, read: true });
            user_dict.insert(Item::Literal(vm.make_lit(b"StandardEncoding")), Item::Array(standard_encoding));
        }
        let user_dict_key = vm.make_dict(user_dict, Mode { write: true, execute: false, read: true });
        
        {
            let key = vm.make_lit(b"userdict");
            system_dict.insert(Item::Literal(key), Item::Dict(user_dict_key));
            
            let key = vm.make_lit(b"errordict");
            system_dict.insert(Item::Literal(key), Item::Dict(error_dict_key));
        }
        
        // systemdict fuckery
        {
            let system_dict_key = vm.make_dict(system_dict, Mode { write: false, execute: false, read: true });
            vm.push_dict(system_dict_key);
            let lit = vm.make_lit(b"systemdict");
            vm.dicts[system_dict_key].0.insert(Item::Literal(lit), Item::Dict(system_dict_key));
        }
        
        vm.push_dict(user_dict_key);
        
        vm
    }
    pub fn fonts<'a>(&'a self) -> impl Iterator<Item=(&'a str, RefDict<'a>)> {
        self.fonts.iter().map(move |(key, &dict)| (
            key.as_str(),
            RefDict { vm: self, dict: self.get_dict(dict) }
        ))
    }
    fn pop_tuple<T>(&mut self) -> Result<T, FontError> where
        T: TupleElements<Element=Item>
    {
        let range = self.stack.len() - T::N ..;
        Ok(expect!(T::from_iter(self.stack.drain(range)), "not enough data on the stack"))
    }
    fn pop(&mut self) -> Item {
        self.stack.pop().expect("empty stack")
    }
    fn push(&mut self, item: Item) {
        self.stack.push(item);
    }
    fn push_dict(&mut self, dict: DictKey) {
        self.dict_stack.push(dict);
    }
    fn make_lit(&mut self, lit: &[u8]) -> LitKey {
        if let Some((index, _)) = self.literals.get_full(lit) {
            return LitKey(index);
        }
        let (index, _) = self.literals.insert_full(lit.into());
        LitKey(index)
    }
    fn get_lit(&self, LitKey(index): LitKey) -> &[u8] {
        self.literals.get_index(index).expect("no such key").as_slice()
    }
    fn make_array(&mut self, array: Array, mode: Mode) -> ArrayKey {
        self.arrays.insert((array, mode))
    }
    fn make_string(&mut self, s: Vec<u8>) -> StringKey {
        self.strings.insert((s, Mode::all()))
    }
    fn make_dict(&mut self, dict: Dictionary, mode: Mode) -> DictKey {
        self.dicts.insert((dict, mode))
    }
    fn get_string(&self, key: StringKey) -> &[u8] {
        &self.strings.get(key).unwrap().0
    }
    fn get_string_mut(&mut self, key: StringKey) -> &mut Vec<u8> {
        &mut self.strings.get_mut(key).unwrap().0
    }
    fn get_array(&self, key: ArrayKey) -> &Array {
        match self.arrays.get(key).expect("no item for key") {
            (ref array, _) => array
        }
    }
    fn get_array_mut(&mut self, key: ArrayKey) -> &mut Array {
        match self.arrays.get_mut(key).expect("no item for key") {
            (ref mut array, Mode { write: true, .. }) => array,
            _ => panic!("array is locked")
        }
    }
    fn exec_array(&mut self, key: ArrayKey, input: &mut Input) {
        let array = match self.arrays.get(key).expect("no item for key") {
            (ref array, Mode { execute: true, .. } ) => array.clone(),
            _ => panic!("not executable")
        };
        
        for item in &array {
            self.exec(item.clone(), input);
        }
    }
    fn get_dict(&self, key: DictKey) -> &Dictionary {
        match self.dicts.get(key).expect("no item for key") {
            (ref dict, _) => dict
        }
    }
    fn get_dict_mut(&mut self, key: DictKey) -> &mut Dictionary {
        match self.dicts.get_mut(key).expect("no item for key") {
            (ref mut dict, Mode { write: true, .. }) => dict,
            _ => panic!("dict is locked")
        }
    }
    fn pop_dict(&mut self) {
        self.dict_stack.pop();
    }
    fn current_dict_mut(&mut self) -> &mut Dictionary {
        let &key = self.dict_stack.last().expect("no current dict");
        self.get_dict_mut(key)
    }
    pub fn stack(&self) -> &[Item] {
        &self.stack
    }
    
    // resolve name items. or keep them unchanged if unresolved
    fn resolve(&self, item: Item) -> Option<Item> {
        for &dict_key in self.dict_stack.iter().rev() {
            let dict = self.get_dict(dict_key);
            if let Some(&val) = dict.get(&item) {
                return Some(val.clone());
            }
        }
        None
    }
    
    fn transform_token(&mut self, token: Token) -> Item {
        match token {
            Token::Int(i) => Item::Int(i),
            Token::Real(r) => Item::Real(r),
            Token::Literal(name) => Item::Literal(self.make_lit(name)),
            Token::Name(name) => Item::Name(self.make_lit(name)),
            Token::String(vec) => Item::String(self.make_string(vec)),
            Token::Procedure(tokens) => {
                let array = tokens.into_iter().map(|t| self.transform_token(t)).collect();
                Item::Array(self.make_array(array, Mode::all()))
            }
        }
    }
    pub fn exec_token(&mut self, token: Token, input: &mut Input) -> Result<(), FontError> {
        let item = self.transform_token(token);
        trace!("exec_token {:?}", self.display(item));
        match item {
            Item::Operator(op) => self.exec_operator(op, input)?,
            Item::Name(key) => {
                let item = match self.resolve(Item::Literal(key)) {
                    Some(item) => item,
                    None => error!("unimplemented token {:?}", String::from_utf8_lossy(self.get_lit(key)))
                };
                self.exec_expand(item, input)?;
            }
            item => self.push(item)
        }
        Ok(())
    }
    
    fn exec_expand(&mut self, item: Item, input: &mut Input) -> Result<(), FontError> {
        trace!("exec_expand {:?}", self.display(item));
        match item {
            Item::Operator(op) => {
                self.exec_operator(op, input)?;
            }
            Item::Name(key) => {
                let item = expect!(self.resolve(Item::Literal(key)), "undefined");
                self.exec(item, input)?;
            }
            Item::Array(key) => {
                // check that the array is executable
                if !expect!(self.arrays.get(key), "no item for key").1.execute {
                    self.push(item);
                } else {
                    let mut pos = 0;
                    loop {
                        match self.arrays.get(key).expect("no item for key") {
                            (ref items, Mode { execute: true, .. }) => {
                                match items.get(pos) {
                                    Some(&item) => self.exec(item, input)?,
                                    None => break
                                }
                            },
                            _ => error!("exec: array is not executable")
                        }
                        pos += 1;
                    }
                }
            }
            item => {
                self.push(item);
            }
        }
        Ok(())
    }
    fn exec(&mut self, item: Item, input: &mut Input) -> Result<(), FontError> {
        trace!("exec {:?}", self.display(item));
        /*
        loop {
            let mut s = String::new();
            std::io::stdin().read_line(&mut s);
            let s = s.trim();
            match s {
                "s" => self.print_stack(),
                "d" => self.print_current_dict(),
                "" => break,
                _ => println!("unknown command. known are: 's' for print stack, 'd' for print current dict, empty to continue")
            }
        }
        */
        match item {
            Item::Operator(op) => self.exec_operator(op, input)?,
            Item::Name(key) => {
                let item = self.resolve(Item::Literal(key)).expect("undefined");
                self.exec_expand(item, input)?;
            }
            item => self.push(item)
        }
        Ok(())
    }
    
    #[deny(unreachable_patterns)]
    fn exec_operator(&mut self, op: Operator, input: &mut Input) -> Result<(), FontError> {
        match op {
            Operator::Array => {
                match self.pop() {
                    Item::Int(i) if i >= 0 => {
                        let key = self.make_array(vec![Item::Null; i as usize], Mode::all());
                        self.push(Item::Array(key));
                    }
                    i => error!("array: invalid count: {:?}", self.display(i))
                }
            }
            Operator::Begin => {
                match self.pop() {
                    Item::Dict(dict) => self.push_dict(dict),
                    item => error!("begin: unespected item {:?}", self.display(item))
                }
            }
            Operator::CurrentDict => {
                let &key = self.dict_stack.last().expect("no current dictionary");
                self.push(Item::Dict(key));
            }
            Operator::DefineFont => {
                match self.pop_tuple()? {
                    (Item::Literal(lit), Item::Dict(dict_key)) => {
                        let font_name = String::from_utf8(self.get_lit(lit).to_owned())
                            .expect("Font name is not valid UTF-8");
                        let (_, ref mut mode) = self.dicts.get_mut(dict_key).unwrap();
                        mode.read_only();
                        self.fonts.insert(font_name, dict_key);
                        self.push(Item::Dict(dict_key));
                    }
                    args => error!("definefont: invalid args {:?}", self.display_tuple(args))
                }
            }
            Operator::InternalDict => {
                match self.pop() {
                    Item::Int(1183615869) => {
                        let dict = Item::Dict(self.internal_dict);
                        self.push(dict);
                    }
                    i => error!("internaldict: invalid argument: {:?}", self.display(i))
                }
            }
            Operator::For => {
                match self.pop_tuple()? {
                    (Item::Int(initial), Item::Int(increment), Item::Int(limit), Item::Array(procedure)) => {
                        match increment {
                            i if i > 0 => require!(limit > initial),
                            i if i < 0 => require!(limit < initial),
                            _ => error!("zero increment")
                        }
                        // proc would be allowed to modify the procedure array…
                        let proc_array = self.get_array(procedure).clone();
                        let mut val = initial;
                        while val < limit {
                            self.push(Item::Int(val));
                            for item in &proc_array {
                                self.exec(item.clone(), input);
                            }
                            val += increment;
                        }
                    },
                    args => error!("for: invalid args {:?}", self.display_tuple(args))
                }
            }
            Operator::If => {
                match self.pop_tuple()? {
                    (Item::Bool(cond), Item::Array(proc)) => {
                        if cond {
                            self.exec_array(proc, input);
                        }
                    }
                    args => error!("if: invalid args {:?}", self.display_tuple(args))
                }
            }
            Operator::IfElse => {
                match self.pop_tuple()? {
                    (Item::Bool(cond), Item::Array(proc_a), Item::Array(proc_b)) => {
                        let proc = if cond {
                            proc_a
                        } else {
                            proc_b
                        };
                        
                        self.exec_array(proc, input);
                    }
                    args => error!("ifelse: invalid args {:?}", self.display_tuple(args))
                }
            }
            Operator::Exec => {
                let item = self.pop();
                self.exec(item, input);
            }
            Operator::Eq => {
                let (a, b) = self.pop_tuple()?;
                self.push(Item::Bool(a == b));
            }
            Operator::Cvx => {
                let item = self.pop();
                let item = self.resolve(item).unwrap_or(item);
                self.push(item);
            }
            Operator::Def => {
                let (key, val) = self.pop_tuple()?;
                self.current_dict_mut().insert(key, val);
            }
            Operator::Dict => {
                match self.pop() {
                    Item::Int(n) if n >= 0 => {
                        let dict = self.make_dict(Dictionary::with_capacity(n as usize), Mode::all());
                        self.push(Item::Dict(dict));
                    }
                    arg => error!("dict: unsupported {:?}", self.display(arg))
                }
            }
            Operator::Known => {
                match self.pop_tuple()? {
                    (Item::Dict(dict), key) => {
                        let dict = self.get_dict(dict);
                        let known = dict.contains_key(&key);
                        self.push(Item::Bool(known))
                    },
                    args => error!("known: invalid args {:?}", self.display_tuple(args))
                }
            }
            Operator::String => {
                match self.pop() {
                    Item::Int(n) if n >= 0 => {
                        let string = self.make_string(vec![0; n as usize]);
                        self.push(Item::String(string));
                    },
                    len => error!("string: unsupported {:?}", self.display(len))
                }
            },
            Operator::ReadString => {
                match self.pop_tuple()? {
                    (Item::File, Item::String(key)) => {
                        let string = self.get_string_mut(key);
                        let flag = input.read_to(string);
                        expect!(input.try_parse(space), "Failed to parse space");
                        
                        self.push(Item::String(key));
                        self.push(Item::Bool(flag));
                    },
                    args => error!("readstring: invalid arguments {:?}", self.display_tuple(args))
                }
            }
            Operator::Dup => {
                let v = self.pop();
                self.push(v.clone());
                self.push(v);
            },
            Operator::Copy => {
                let last = self.pop();
                match last {
                    Item::Int(i) if i >= 0 => {
                        let n = i as usize;
                        let len = self.stack.len();
                        let start = self.stack.len() - n;
                        for i in start .. len {
                            let item = self.stack[i];
                            self.push(item);
                        }
                    },
                    _ => {
                        let first = self.pop();
                        match (first, last) {
                            (Item::Array(a), Item::Array(b)) => {
                                // ugly, but avoids RefCells
                                let a = self.get_array(a).clone();
                                self.get_array_mut(b)[.. a.len()].copy_from_slice(&a);
                                self.push(last);
                            }
                            (Item::Dict(a), Item::Dict(b)) => {
                                let a = self.get_dict(a).clone();
                                self.get_dict_mut(b).extend(a);
                                self.push(last);
                            }
                            (Item::String(a), Item::String(b)) => {
                                let a = self.get_string(a).to_owned();
                                expect!(self.get_string_mut(b).get_mut(.. a.len()), "out of bounds").copy_from_slice(&a);
                                self.push(last);
                            }
                            args => error!("copy: invalid arguments {:?}", self.display_tuple(args))
                        }
                    }
                }
            }
            Operator::Pop => {
                self.pop();
            }
            Operator::End => self.pop_dict(),
            Operator::Exch => {
                let (a, b) = self.pop_tuple()?;
                self.push(b);
                self.push(a);
            }
            Operator::False => self.push(Item::Bool(false)),
            Operator::True => self.push(Item::Bool(true)),
            Operator::Not => {
                match self.pop() {
                    Item::Bool(b) => self.push(Item::Bool(!b)),
                    Item::Int(i) => self.push(Item::Int(!i)),
                    arg => error!("not: invalid argument {:?}", self.display(arg))
                }
            }
            Operator::Index => match self.pop() {
                Item::Int(idx) if idx >= 0 => {
                    let n = self.stack.len();
                    let item = self.stack.get(n - idx as usize - 1).expect("out of bounds").clone();
                    self.push(item);
                },
                arg => error!("index: invalid argument {:?}", self.display(arg))
            }
            Operator::Get => match self.pop_tuple()? {
                (Item::Array(key), Item::Int(index)) if index >= 0 => {
                    let &item = self.get_array(key).get(index as usize).expect("out of bounds");
                    self.push(item);
                }
                (Item::String(key), Item::Int(index)) if index >= 0 => {
                    let &byte = self.get_string(key).get(index as usize).expect("out of bounds");
                    self.push(Item::Int(byte as i32));
                }
                (Item::Dict(dict_key), key) => {
                    let &item = self.get_dict(dict_key).get(&key).expect("no such entry");
                    self.push(item);
                }
                args => error!("get: invalid arguments {:?}", self.display_tuple(args))
            }
            Operator::Put => {
                let (a, b, c) = self.pop_tuple()?;
                let a = self.resolve(a).unwrap_or(a);
                match (a, b, c) {
                    (Item::Array(array), Item::Int(idx), any) => {
                        *expect!(self.get_array_mut(array).get_mut(idx as usize), "out of bounds") = any;
                    }
                    (Item::Dict(dict), key, any) => {
                        self.get_dict_mut(dict).insert(key, any);
                    }
                    args => error!("put: unsupported args {:?})", self.display_tuple(args))
                }
            }
            Operator::Count => {
                let n = self.stack.len();
                self.push(Item::Int(n as i32));
            }
            Operator::Length => {
                let len = match self.pop() {
                    Item::Array(key) => self.get_array(key).len(),
                    Item::Dict(key) => self.get_dict(key).len(),
                    Item::String(key) => self.get_string(key).len(),
                    Item::Name(lit) => self.get_lit(lit).len(),
                    arg => error!("length: invalid argument {:?}", self.display(arg))
                };
                self.push(Item::Int(len as i32));
            }
            Operator::MaxLength => {
                match self.pop() {
                    Item::Dict(key) => {
                        let cap = self.get_dict(key).capacity();
                        self.push(Item::Int(cap as i32));
                    }
                    arg => error!("maxlength: invalid argument {:?}", self.display(arg))
                }
            }
            Operator::ReadOnly => {
                let item = self.pop();
                match item {
                    Item::Array(key) => self.arrays[key].1.read_only(),
                    Item::Dict(key) => self.dicts[key].1.read_only(),
                    Item::String(key) => self.strings[key].1.read_only(),
                    i => error!("can't make {:?} readonly", self.display(i))
                }
                self.push(item);
            },
            Operator::ExecuteOnly => {
                let item = self.pop();
                match item {
                    Item::Array(key) => self.arrays[key].1.execute_only(),
                    Item::Dict(key) => self.dicts[key].1.execute_only(),
                    Item::String(key) => self.strings[key].1.execute_only(),
                    i => error!("can't make {:?} executeonly", self.display(i))
                }
                self.push(item);
            },
            Operator::NoAccess => {
                let item = self.pop();
                match item {
                    Item::Array(key) => self.arrays[key].1.noaccess(),
                    Item::Dict(key) => self.dicts[key].1.noaccess(),
                    Item::String(key) => self.strings[key].1.noaccess(),
                    i => error!("can't make {:?} executeonly", self.display(i))
                }
                self.push(item);
            }
            Operator::EndArray => {
                let start = expect!(
                    self.stack.iter().rposition(|item| *item == Item::Mark),
                    "unmatched ]"
                );
                let array = self.stack.drain(start ..).skip(1).collect(); // skip the Mark
                let key = self.make_array(array, Mode::all());
                self.push(Item::Array(key));
            },
            Operator::Mark => self.push(Item::Mark),
            Operator::ClearToMark => {
                let start = expect!(
                    self.stack.iter().rposition(|item| *item == Item::Mark),
                    "unmatched mark"
                );
                self.stack.drain(start ..);
            }
            Operator::CurrentFile => self.push(Item::File),
            Operator::CloseFile => {
                match self.pop() {
                    Item::File => {
                        input.open = false;
                    },
                    arg => error!("closefile: invalid arg {:?})", self.display(arg))
                }
            }
            Operator::Eexec => {
                match self.pop() {
                    Item::File => {
                        use crate::eexec::Decoder;
                        match input.try_parse(hex_string) {
                            Some(mut data) if data.len() > 4 => {
                                Decoder::file().decode_inline(&mut data);
                                debug!("data: {}", String::from_utf8_lossy(&data));
                                self.parse_and_exec(slice!(data, 4..));
                            }
                            _ => {
                                let decoded = Decoder::file().decode(input.data, 4);
                                let skip = self.parse_and_exec(&decoded)? + 4;
                                input.advance(skip);
                            }
                        };
                    },
                    Item::String(_) => {
                        unimplemented!()
                        // let mut input = Input::new(self.get_string(key));
                        // self.parse_and_exec(&mut input);
                    },
                    arg => error!("eexec: unsupported arg {:?})", self.display(arg))
                }
            }

            Operator::Abs => {
                let out = match self.pop() {
                    Item::Real(r) => Item::Real(R32::from(r.into_inner().abs())),
                    Item::Int(i32::MIN) => Item::Real(-R32::from(i32::MIN as f32)),
                    Item::Int(i) => Item::Int(i.abs()),
                    arg => error!("abs: unsupported arg {:?})", self.display(arg))
                };
                self.push(out);
            }
            Operator::Add => {
                let out = match self.pop_tuple()? {
                    (Item::Int(a), Item::Int(b)) => match a.checked_add(b) {
                        Some(c) => Item::Int(c),
                        None => Item::Real(R32::from(a as f32) + R32::from(b as f32))
                    },
                    (Item::Real(a), Item::Real(b)) => Item::Real(a + b),
                    (Item::Int(a), Item::Real(b)) |
                    (Item::Real(b), Item::Int(a)) =>
                        Item::Real(R32::from(a as f32) + b),
                    (arg1, arg2) => error!("add: unsupported args {:?} {:?})", self.display(arg1), self.display(arg2))
                };
                self.push(out);
            }
            Operator::Sub => {
                let out = match self.pop_tuple()? {
                    (Item::Int(a), Item::Int(b)) => match a.checked_sub(b) {
                        Some(c) => Item::Int(c),
                        None => Item::Real(R32::from(a as f32) - R32::from(b as f32))
                    },
                    (Item::Real(a), Item::Real(b)) => Item::Real(a - b),
                    (Item::Int(a), Item::Real(b)) => Item::Real(R32::from(a as f32) - b),
                    (Item::Real(a), Item::Int(b)) => Item::Real(a - R32::from(b as f32)),
                    (arg1, arg2) => error!("sub: unsupported args {:?} {:?})", self.display(arg1), self.display(arg2))
                };
                self.push(out);
            }
            Operator::Mul => {
                let out = match self.pop_tuple()? {
                    (Item::Int(a), Item::Int(b)) => match a.checked_mul(b) {
                        Some(c) => Item::Int(c),
                        None => Item::Real(R32::from(a as f32) * R32::from(b as f32))
                    },
                    (Item::Real(a), Item::Real(b)) => Item::Real(a * b),
                    (Item::Int(a), Item::Real(b)) |
                    (Item::Real(b), Item::Int(a)) =>
                        Item::Real(R32::from(a as f32) * b),
                    (arg1, arg2) => error!("mul: unsupported args {:?} {:?})", self.display(arg1), self.display(arg2))
                };
                self.push(out);
            }
        }
        Ok(())
    }
    pub fn display(&self, item: Item) -> RefItem {
        RefItem::new(self, item)
    }
    pub fn display_tuple<'a, T>(&'a self, tuple: T) -> T::Output where
        T: TupleElements<Element=Item>,
        T: Map<RefItem<'a>>
    {
        tuple.map(|item| RefItem::new(self, item))
    }
    pub fn print_stack(&self) {
        for (i, &item) in self.stack.iter().rev().enumerate().rev() {
            println!("stack[{}]: {:?}", i, self.display(item));
        }
    }
    pub fn step(&mut self, input: &mut Input) -> Result<(), FontError> {
        expect!(input.try_parse(space), "Failed to parse space");
        if let Some(_) = input.try_parse(comment) {
            return Ok(());
        }
        if input.len() == 0 {
            return Ok(());
        }
        let tk = expect!(input.try_parse(token), "Failed to parse token");
        
        trace!("token: {:?}", tk);
        self.exec_token(tk, input)
    }
    // returns the number of bytes processed
    pub fn parse_and_exec(&mut self, data: &[u8]) -> Result<usize, FontError> {
        let input_size = data.len();
        let mut input = Input::new(data);
        // skip leading whitespace
        
        while input.len() > 0 && input.open {
            self.step(&mut input)?;
        }
        Ok(input_size - input.len())
    }
}
