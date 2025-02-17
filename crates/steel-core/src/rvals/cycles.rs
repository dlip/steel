use std::{cell::Cell, collections::VecDeque};

use num::BigInt;

use crate::steel_vm::{builtin::get_function_name, engine::Engine};

use super::*;

thread_local! {
    // Use this to print values, in lieu of a bespoke printer
    static PRINTING_KERNEL: RefCell<Engine> = {

        let mut engine = Engine::new_printer();

        engine.run(include_str!("../scheme/print.scm")).unwrap();

        RefCell::new(engine)
    };
}

pub fn install_printer() {
    PRINTING_KERNEL.with(|x| {
        x.borrow().globals();
    });
}

#[steel_derive::function(name = "print-in-engine")]
pub fn print_in_engine(value: SteelVal) {
    PRINTING_KERNEL
        .with(|x| {
            x.borrow_mut()
                .call_function_by_name_with_args("print", vec![value])
        })
        .unwrap();
}

#[derive(Default)]
// Keep track of any reference counted values that are visited, in a pointer
pub(super) struct CycleDetector {
    // Recording things that have already been seen
    cycles: fxhash::FxHashMap<usize, usize>,

    // Values captured in cycles
    values: Vec<SteelVal>,

    depth: usize,
}

impl CycleDetector {
    pub(super) fn detect_and_display_cycles(val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        // Consider using one shared queue here
        let mut queue = VecDeque::new();

        let mut bfs_detector = CycleCollector {
            visited: fxhash::FxHashSet::default(),
            cycles: fxhash::FxHashMap::default(),
            values: Vec::new(),
            queue: &mut queue,
            found_mutable: false,
        };

        bfs_detector.push_back(val.clone());

        bfs_detector.visit();

        CycleDetector {
            cycles: bfs_detector.cycles,
            values: bfs_detector.values,
            depth: 0,
        }
        .start_format(val, f)
    }

    fn start_format(mut self, val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        for node in std::mem::take(&mut self.values) {
            let id = match &node {
                SteelVal::CustomStruct(c) => {
                    let ptr_addr = c.as_ptr() as usize;
                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HeapAllocated(b) => {
                    // Get the object that THIS points to
                    let ptr_addr = b.get().as_ptr_usize().unwrap();
                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::ListV(l) => {
                    let ptr_addr = l.as_ptr_usize();

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::VectorV(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HashMapV(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::HashSetV(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::Custom(l) => {
                    let ptr_addr = l.0.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::Boxed(b) => {
                    let ptr_addr = b.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                SteelVal::SyntaxObject(s) => {
                    let ptr_addr = s.as_ptr() as usize;

                    self.cycles.get(&ptr_addr).unwrap()
                }
                _ => {
                    unreachable!()
                }
            };

            write!(f, "#{id}=")?;
            self.top_level_format_with_cycles(&node, f)?;
            writeln!(f)?;
        }

        if !self.values.contains(val) {
            self.format_with_cycles(val, f)?;
        }

        Ok(())
    }

    fn top_level_format_with_cycles(
        &mut self,
        val: &SteelVal,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        self.depth += 1;

        if self.depth > 128 {
            return write!(f, "...");
        }

        let res = match val {
            BoolV(b) => write!(f, "#{b}"),
            NumV(x) => write!(f, "{x:?}"),
            IntV(x) => write!(f, "{x}"),
            StringV(s) => write!(f, "{s:?}"),
            BigNum(b) => write!(f, "{}", b.as_ref()),
            CharV(c) => write!(f, "#\\{c}"),
            FuncV(func) => {
                if let Some(name) = get_function_name(*func) {
                    write!(f, "#<function:{}>", name.name)
                } else {
                    write!(f, "#<function>")
                }
            }
            Void => write!(f, "#<void>"),
            SymbolV(s) => write!(f, "{s}"),
            VectorV(lst) => {
                let mut iter = lst.iter();
                write!(f, "'#(")?;
                if let Some(last) = iter.next_back() {
                    for item in iter {
                        self.format_with_cycles(item, f)?;
                        write!(f, " ")?;
                    }
                    self.format_with_cycles(last, f)?;
                }
                write!(f, ")")
            }
            Custom(x) => write!(f, "#<{}>", x.borrow().display()?),
            CustomStruct(s) => {
                let guard = s;

                {
                    if guard
                        .get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
                        .and_then(|x| x.as_bool())
                        .unwrap_or_default()
                    {
                        write!(f, "({}", guard.name())?;

                        for i in guard.fields.iter() {
                            write!(f, " ")?;
                            self.format_with_cycles(i, f)?;
                        }

                        write!(f, ")")
                    } else {
                        write!(f, "({})", guard.name())
                    }
                }
            }

            PortV(_) => write!(f, "#<port>"),
            Closure(_) => write!(f, "#<bytecode-closure>"),
            HashMapV(hm) => write!(f, "#<hashmap {:#?}>", hm.as_ref()),
            IterV(_) => write!(f, "#<iterator>"),
            HashSetV(hs) => write!(f, "#<hashset {:?}>", hs.0),
            FutureFunc(_) => write!(f, "#<future-func>"),
            FutureV(_) => write!(f, "#<future>"),
            StreamV(_) => write!(f, "#<stream>"),
            BoxedFunction(b) => {
                if let Some(name) = b.name() {
                    write!(f, "#<function:{}>", name)
                } else {
                    write!(f, "#<function>")
                }
            }
            ContinuationFunction(c) => write!(f, "#<continuation: {:?}>", c.stack),
            // #[cfg(feature = "jit")]
            // CompiledFunction(_) => write!(f, "#<compiled-function>"),
            ListV(l) => {
                write!(f, "(")?;

                let mut iter = l.iter().peekable();

                while let Some(item) = iter.next() {
                    self.format_with_cycles(item, f)?;
                    if iter.peek().is_some() {
                        write!(f, " ")?
                    }
                }

                write!(f, ")")
            }
            MutFunc(_) => write!(f, "#<function>"),
            BuiltIn(_) => write!(f, "#<function>"),
            ReducerV(_) => write!(f, "#<reducer>"),
            MutableVector(v) => write!(f, "{:?}", v.get()),
            SyntaxObject(s) => {
                if let Some(raw) = &s.raw {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, raw)
                } else {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, s.syntax)
                }
            }
            BoxedIterator(_) => write!(f, "#<iterator>"),
            Boxed(b) => write!(f, "'#&{}", b.borrow()),
            Reference(x) => write!(f, "{}", x.format()?),
            HeapAllocated(b) => write!(f, "'#&{}", b.get()),
        };

        self.depth -= 1;

        res
    }

    fn format_with_cycles(&mut self, val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
        self.depth += 1;

        if self.depth > 128 {
            return write!(f, "...");
        }

        let res = match val {
            BoolV(b) => write!(f, "#{b}"),
            NumV(x) => write!(f, "{x:?}"),
            IntV(x) => write!(f, "{x}"),
            StringV(s) => write!(f, "{s:?}"),
            CharV(c) => write!(f, "#\\{c}"),
            FuncV(func) => {
                if let Some(name) = get_function_name(*func) {
                    write!(f, "#<function:{}>", name.name)
                } else {
                    write!(f, "#<function>")
                }
            }
            Void => write!(f, "#<void>"),
            SymbolV(s) => write!(f, "{s}"),
            VectorV(lst) => {
                let mut iter = lst.iter();
                write!(f, "(")?;
                if let Some(last) = iter.next_back() {
                    for item in iter {
                        self.format_with_cycles(item, f)?;
                        write!(f, " ")?;
                    }
                    self.format_with_cycles(last, f)?;
                }
                write!(f, ")")
            }
            Custom(x) => write!(f, "{}", x.borrow().display()?),
            CustomStruct(s) => {
                if let Some(id) = self.cycles.get(&(s.as_ptr() as usize)) {
                    write!(f, "#{id}#")
                } else {
                    let guard = s;

                    {
                        if s.get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
                            .and_then(|x| x.as_bool())
                            .unwrap_or_default()
                        {
                            write!(f, "({}", guard.name())?;

                            for i in guard.fields.iter() {
                                write!(f, " ")?;
                                self.format_with_cycles(i, f)?;
                            }

                            write!(f, ")")
                        } else {
                            write!(f, "({})", guard.name())
                        }
                    }
                }
            }

            PortV(_) => write!(f, "#<port>"),
            Closure(_) => write!(f, "#<bytecode-closure>"),
            HashMapV(hm) => write!(f, "#<hashmap {:#?}>", hm.as_ref()),
            IterV(_) => write!(f, "#<iterator>"),
            HashSetV(hs) => write!(f, "#<hashset {:?}>", hs.0),
            FutureFunc(_) => write!(f, "#<future-func>"),
            FutureV(_) => write!(f, "#<future>"),
            // Promise(_) => write!(f, "#<promise>"),
            StreamV(_) => write!(f, "#<stream>"),
            BoxedFunction(b) => {
                if let Some(name) = b.name() {
                    write!(f, "#<function:{}>", name)
                } else {
                    write!(f, "#<function>")
                }
            }
            ContinuationFunction(c) => write!(f, "#<continuation: {:?}>", c.stack),
            // #[cfg(feature = "jit")]
            // CompiledFunction(_) => write!(f, "#<compiled-function>"),
            ListV(l) => {
                write!(f, "(")?;

                let mut iter = l.iter().peekable();

                while let Some(item) = iter.next() {
                    self.format_with_cycles(item, f)?;
                    if iter.peek().is_some() {
                        write!(f, " ")?
                    }
                }

                // for item in l.iter().pe

                // for item in l {
                //     display_helper(item, f)?;
                //     write!(f, " ")?;
                // }
                write!(f, ")")
            }
            // write!(f, "#<list {:?}>", l),
            MutFunc(_) => write!(f, "#<function>"),
            BuiltIn(_) => write!(f, "#<function>"),
            ReducerV(_) => write!(f, "#<reducer>"),
            MutableVector(v) => write!(f, "{:?}", v.get()),
            SyntaxObject(s) => {
                if let Some(raw) = &s.raw {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, raw)
                } else {
                    write!(f, "#<syntax:{:?} {:?}>", s.span, s.syntax)
                }
            }
            BoxedIterator(_) => write!(f, "#<iterator>"),
            Boxed(b) => write!(f, "'#&{}", b.borrow()),
            Reference(x) => write!(f, "{}", x.format()?),
            BigNum(b) => write!(f, "{}", b.as_ref()),
            HeapAllocated(b) => {
                if let Some(id) = b.get().as_ptr_usize().and_then(|x| self.cycles.get(&x)) {
                    write!(f, "#{id}#")
                } else {
                    write!(f, "'#&{}", b.get())
                }
            }
        };

        self.depth -= 1;

        res
    }
}

fn replace_with_void(value: &mut SteelVal) -> SteelVal {
    std::mem::replace(value, SteelVal::Void)
}

impl SteelVal {
    fn make_void(&mut self) -> SteelVal {
        std::mem::replace(self, SteelVal::Void)
    }
}

pub(crate) struct SteelCycleCollector {
    cycles: fxhash::FxHashMap<usize, usize>,
    values: List<SteelVal>,
}

impl Custom for SteelCycleCollector {}

impl SteelCycleCollector {
    pub fn from_root(value: SteelVal) -> Self {
        let mut queue = VecDeque::new();

        let mut collector = CycleCollector {
            visited: fxhash::FxHashSet::default(),
            cycles: fxhash::FxHashMap::default(),
            values: Vec::new(),
            queue: &mut queue,
            found_mutable: false,
        };

        collector.push_back(value);

        collector.visit();

        if collector.found_mutable {
            SteelCycleCollector {
                cycles: collector.cycles,
                values: collector.values.into(),
            }
        } else {
            SteelCycleCollector {
                cycles: fxhash::FxHashMap::default(),
                values: List::new(),
            }
        }
    }

    // Get the value
    pub fn get(&self, node: SteelVal) -> Option<usize> {
        match node {
            SteelVal::CustomStruct(c) => {
                let ptr_addr = c.as_ptr() as usize;
                self.cycles.get(&ptr_addr)
            }
            SteelVal::HeapAllocated(b) => {
                // Get the object that THIS points to
                let ptr_addr = b.get().as_ptr_usize().unwrap();
                self.cycles.get(&ptr_addr)
            }
            SteelVal::ListV(l) => {
                let ptr_addr = l.as_ptr_usize();

                self.cycles.get(&ptr_addr)
            }
            SteelVal::VectorV(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::HashMapV(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::HashSetV(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::Custom(l) => {
                let ptr_addr = l.0.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::Boxed(b) => {
                let ptr_addr = b.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            SteelVal::SyntaxObject(s) => {
                let ptr_addr = s.as_ptr() as usize;

                self.cycles.get(&ptr_addr)
            }
            _ => None,
        }
        .copied()
    }

    pub fn values(&self) -> List<SteelVal> {
        self.values.clone()
    }
}

struct CycleCollector<'a> {
    // Keep a mapping of the pointer -> gensym
    visited: fxhash::FxHashSet<usize>,

    // Recording things that have already been seen
    cycles: fxhash::FxHashMap<usize, usize>,

    // Values captured in cycles
    values: Vec<SteelVal>,

    // Queue of items to check
    queue: &'a mut VecDeque<SteelVal>,

    // Whether we found something mutable - if we haven't, then a cycle
    // isn't even possible
    found_mutable: bool,
}

impl<'a> CycleCollector<'a> {
    fn add(&mut self, val: usize, steelval: &SteelVal) -> bool {
        if self.visited.contains(&val) {
            let id = self.cycles.len();

            // If we've already seen this, its fine, we can just move on
            if let std::collections::hash_map::Entry::Vacant(e) = self.cycles.entry(val) {
                e.insert(id);
                // Keep track of the actual values that are being captured
                self.values.push(steelval.clone());
            } else {
                return true;
            }

            return true;
        }

        self.visited.insert(val);
        false
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for CycleCollector<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop_front()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.queue.push_back(value)
    }

    fn visit_closure(&mut self, _closure: Gc<ByteCodeLambda>) -> Self::Output {}
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_float(&mut self, _float: f64) -> Self::Output {}
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_char(&mut self, _c: char) -> Self::Output {}

    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        if !self.add(
            vector.0.as_ptr() as usize,
            &SteelVal::VectorV(vector.clone()),
        ) {
            for value in vector.0.iter() {
                self.push_back(value.clone());
            }
        }
    }

    fn visit_void(&mut self) -> Self::Output {}
    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}
    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}

    // If we have cycles here, it is game over - we probably don't want to be
    // able to render to these easily?
    fn visit_custom_type(
        &mut self,
        _custom_type: Gc<RefCell<Box<dyn CustomType>>>,
    ) -> Self::Output {
    }

    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output {
        if !self.add(
            hashmap.0.as_ptr() as usize,
            &SteelVal::HashMapV(hashmap.clone()),
        ) {
            for (key, value) in hashmap.0.iter() {
                self.push_back(key.clone());
                self.push_back(value.clone());
            }
        }
    }

    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output {
        if !self.add(
            hashset.0.as_ptr() as usize,
            &SteelVal::HashSetV(hashset.clone()),
        ) {
            for key in hashset.0.iter() {
                self.push_back(key.clone())
            }
        }
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        if !self.add(
            steel_struct.as_ptr() as usize,
            &SteelVal::CustomStruct(steel_struct.clone()),
        ) {
            for value in steel_struct.fields.iter() {
                self.push_back(value.clone())
            }
        }
    }

    fn visit_port(&mut self, _port: Gc<SteelPort>) -> Self::Output {}
    fn visit_transducer(&mut self, _transducer: Gc<Transducer>) -> Self::Output {}
    fn visit_reducer(&mut self, _reducer: Gc<Reducer>) -> Self::Output {}
    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}
    fn visit_stream(&mut self, _stream: Gc<LazyStream>) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Rc<BoxedDynFunction>) -> Self::Output {}
    fn visit_continuation(&mut self, _continuation: Gc<Continuation>) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        if !self.add(list.as_ptr_usize(), &SteelVal::ListV(list.clone())) {
            for value in list {
                self.push_back(value);
            }
        }
    }

    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}

    // TODO: Figure out the mutable vector first
    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        if !self.add(
            vector.as_ptr_usize(),
            &SteelVal::MutableVector(vector.clone()),
        ) {
            for value in vector.get().iter() {
                self.push_back(value.clone());
            }
        }
    }

    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}

    fn visit_boxed_iterator(&mut self, _iterator: Gc<RefCell<OpaqueIterator>>) -> Self::Output {}

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        if !self.add(
            syntax_object.as_ptr() as usize,
            &SteelVal::SyntaxObject(syntax_object.clone()),
        ) {
            if let Some(raw) = syntax_object.raw.clone() {
                self.push_back(raw);
            }

            self.push_back(syntax_object.syntax.clone());
        }
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output {
        if !self.add(
            boxed_value.as_ptr() as usize,
            &SteelVal::Boxed(boxed_value.clone()),
        ) {
            self.push_back(boxed_value.borrow().clone());
        }
    }

    fn visit_reference_value(&mut self, _reference: Rc<OpaqueReference<'static>>) -> Self::Output {}
    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) -> Self::Output {}

    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        self.found_mutable = true;

        if !self.add(
            heap_ref.as_ptr_usize(),
            &SteelVal::HeapAllocated(heap_ref.clone()),
        ) {
            self.push_back(heap_ref.get());
        }
    }
}

#[cfg(not(feature = "without-drop-protection"))]
pub(crate) mod drop_impls {

    use super::*;

    thread_local! {
        pub static DROP_BUFFER: RefCell<VecDeque<SteelVal>> = RefCell::new(VecDeque::with_capacity(128));
        pub static FORMAT_BUFFER: RefCell<VecDeque<SteelVal>> = RefCell::new(VecDeque::with_capacity(128));
    }

    impl Drop for SteelVector {
        fn drop(&mut self) {
            if self.0.is_empty() {
                return;
            }

            if let Some(inner) = self.0.get_mut() {
                DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            for value in std::mem::take(inner) {
                                drop_buffer.push_back(value);
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        }
                    })
                    .ok();
            }
        }
    }

    impl Drop for SteelHashMap {
        fn drop(&mut self) {
            if self.0.is_empty() {
                return;
            }

            if let Some(inner) = self.0.get_mut() {
                DROP_BUFFER
                    .try_with(|drop_buffer| {
                        if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                            for (key, value) in std::mem::take(inner) {
                                drop_buffer.push_back(key);
                                drop_buffer.push_back(value);
                            }

                            IterativeDropHandler::bfs(&mut drop_buffer);
                        }
                    })
                    .ok();
            }
        }
    }

    impl Drop for UserDefinedStruct {
        fn drop(&mut self) {
            if self.fields.is_empty() {
                return;
            }

            if DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        // for value in std::mem::take(&mut self.fields) {
                        //     drop_buffer.push_back(value);
                        // }

                        drop_buffer.extend(Vec::from(std::mem::take(&mut self.fields)));

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .is_err()
            {
                let mut buffer = Vec::from(std::mem::take(&mut self.fields)).into();

                IterativeDropHandler::bfs(&mut buffer);
            }
        }
    }

    impl Drop for LazyStream {
        fn drop(&mut self) {
            if self.initial_value == SteelVal::Void && self.stream_thunk == SteelVal::Void {
                return;
            }

            DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        drop_buffer.push_back(self.initial_value.make_void());
                        drop_buffer.push_back(self.stream_thunk.make_void());

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .ok();
        }
    }

    impl Drop for ByteCodeLambda {
        fn drop(&mut self) {
            if self.captures.is_empty() {
                return;
            }

            DROP_BUFFER
                .try_with(|drop_buffer| {
                    if let Ok(mut drop_buffer) = drop_buffer.try_borrow_mut() {
                        for value in std::mem::take(&mut self.captures) {
                            drop_buffer.push_back(value);
                        }

                        IterativeDropHandler::bfs(&mut drop_buffer);
                    }
                })
                .ok();
        }
    }
}

pub struct IterativeDropHandler<'a> {
    drop_buffer: &'a mut VecDeque<SteelVal>,
}

impl<'a> IterativeDropHandler<'a> {
    pub fn bfs(drop_buffer: &'a mut VecDeque<SteelVal>) {
        // println!("Current depth: {}", DEPTH.with(|x| x.get()));

        // DEPTH.with(|x| x.set(x.get() + 1));
        IterativeDropHandler { drop_buffer }.visit();
        // DEPTH.with(|x| x.set(x.get() - 1));
    }
}

impl<'a> BreadthFirstSearchSteelValVisitor for IterativeDropHandler<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {
        ()
    }

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.drop_buffer.pop_front()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.drop_buffer.push_back(value)
    }

    fn visit_bool(&mut self, _boolean: bool) {}
    fn visit_float(&mut self, _float: f64) {}
    fn visit_int(&mut self, _int: isize) {}
    fn visit_char(&mut self, _c: char) {}
    fn visit_void(&mut self) {}
    fn visit_string(&mut self, _string: SteelString) {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) {}
    fn visit_symbol(&mut self, _symbol: SteelString) {}
    fn visit_port(&mut self, _port: Gc<SteelPort>) {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) {}
    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) {}
    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) {}
    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) {}
    fn visit_builtin_function(&mut self, _function: BuiltInSignature) {}
    fn visit_boxed_function(&mut self, _function: Rc<BoxedDynFunction>) {}

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) {
        if let Ok(mut inner) = closure.try_unwrap() {
            for value in std::mem::take(&mut inner.captures) {
                self.push_back(value);
            }
        }
    }

    fn visit_immutable_vector(&mut self, mut vector: SteelVector) {
        if let Some(inner) = vector.0.get_mut() {
            for value in std::mem::take(inner) {
                self.push_back(value);
            }
        }
    }

    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) {
        if let Ok(inner) = custom_type.try_unwrap() {
            let mut inner = inner.into_inner();

            // let this decide if we're doing anything with this custom type
            inner.drop_mut(self);
        }
    }

    fn visit_hash_map(&mut self, mut hashmap: SteelHashMap) {
        if let Some(inner) = hashmap.0.get_mut() {
            for (key, value) in std::mem::take(inner) {
                self.push_back(key);
                self.push_back(value);
            }
        }
    }

    fn visit_hash_set(&mut self, mut hashset: SteelHashSet) {
        if let Some(inner) = hashset.0.get_mut() {
            for key in std::mem::take(inner) {
                self.push_back(key);
            }
        }
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) {
        if let Ok(mut inner) = steel_struct.try_unwrap() {
            for value in Vec::from(std::mem::take(&mut inner.fields)) {
                self.push_back(value);
            }
        }
    }

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) {
        if let Ok(inner) = transducer.try_unwrap() {
            for transducer in inner.ops {
                match transducer {
                    crate::values::transducers::Transducers::Map(m) => self.push_back(m),
                    crate::values::transducers::Transducers::Filter(v) => self.push_back(v),
                    crate::values::transducers::Transducers::Take(t) => self.push_back(t),
                    crate::values::transducers::Transducers::Drop(d) => self.push_back(d),
                    crate::values::transducers::Transducers::FlatMap(fm) => self.push_back(fm),
                    crate::values::transducers::Transducers::Flatten => {}
                    crate::values::transducers::Transducers::Window(w) => self.push_back(w),
                    crate::values::transducers::Transducers::TakeWhile(tw) => self.push_back(tw),
                    crate::values::transducers::Transducers::DropWhile(dw) => self.push_back(dw),
                    crate::values::transducers::Transducers::Extend(e) => self.push_back(e),
                    crate::values::transducers::Transducers::Cycle => {}
                    crate::values::transducers::Transducers::Enumerating => {}
                    crate::values::transducers::Transducers::Zipping(z) => self.push_back(z),
                    crate::values::transducers::Transducers::Interleaving(i) => self.push_back(i),
                }
            }
        }
    }

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) {
        if let Ok(inner) = reducer.try_unwrap() {
            match inner {
                Reducer::ForEach(f) => self.push_back(f),
                Reducer::Generic(rf) => {
                    self.push_back(rf.initial_value);
                    self.push_back(rf.function);
                }
                _ => {}
            }
        }
    }

    fn visit_stream(&mut self, stream: Gc<LazyStream>) {
        if let Ok(mut inner) = stream.try_unwrap() {
            self.push_back(replace_with_void(&mut inner.initial_value));
            self.push_back(replace_with_void(&mut inner.stream_thunk));
        }
    }

    // Walk the whole thing! This includes the stack and all the stack frames
    fn visit_continuation(&mut self, continuation: Gc<Continuation>) {
        if let Ok(mut inner) = continuation.try_unwrap() {
            for value in std::mem::take(&mut inner.stack) {
                self.push_back(value);
            }

            if let Some(inner) = inner.current_frame.function.get_mut() {
                for value in std::mem::take(&mut inner.captures) {
                    self.push_back(value);
                }
            }

            for mut frame in std::mem::take(&mut inner.stack_frames) {
                if let Some(inner) = frame.function.get_mut() {
                    for value in std::mem::take(&mut inner.captures) {
                        self.push_back(value);
                    }
                }
            }
        }
    }

    fn visit_list(&mut self, list: List<SteelVal>) {
        // println!("VISITING LIST: {}", list.strong_count());
        // println!("list: {:?}", list);

        if list.strong_count() == 1 {
            for value in list.draining_iterator() {
                // println!(
                // "PUSHING BACK VALUE - queue size: {}",
                // self.drop_buffer.len()
                // );

                // println!("enqueueing: {}", value);

                self.push_back(value);
            }
        }

        // if list.strong_count() == 1 {
        //     for value in list {
        //         self.push_back(value);
        //     }
        // }
    }

    // TODO: When this gets replaced with heap storage, then we can do this more
    // effectively!
    fn visit_mutable_vector(&mut self, _vector: HeapRef<Vec<SteelVal>>) {}

    // TODO: Once the root is added back to this, bring it back
    fn visit_boxed_iterator(&mut self, iterator: Gc<RefCell<OpaqueIterator>>) {
        if let Ok(inner) = iterator.try_unwrap() {
            self.push_back(inner.into_inner().root)
        }
    }

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) {
        if let Ok(inner) = syntax_object.try_unwrap() {
            if let Some(raw) = inner.raw {
                self.push_back(raw);
            }

            self.push_back(inner.syntax);
        }
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) {
        if let Ok(inner) = boxed_value.try_unwrap() {
            self.push_back(inner.into_inner());
        }
    }

    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) {
        if let Ok(mut inner) = Rc::try_unwrap(reference) {
            inner.drop_mut(self);
        }
    }

    fn visit_heap_allocated(&mut self, _heap_ref: HeapRef<SteelVal>) -> Self::Output {}

    fn visit(&mut self) -> Self::Output {
        let mut ret = self.default_output();

        while let Some(value) = self.pop_front() {
            ret = match value {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(b),
                NumV(n) => self.visit_float(n),
                IntV(i) => self.visit_int(i),
                CharV(c) => self.visit_char(c),
                VectorV(v) => self.visit_immutable_vector(v),
                Void => self.visit_void(),
                StringV(s) => self.visit_string(s),
                FuncV(f) => self.visit_function_pointer(f),
                SymbolV(s) => self.visit_symbol(s),
                SteelVal::Custom(c) => self.visit_custom_type(c),
                HashMapV(h) => self.visit_hash_map(h),
                HashSetV(s) => self.visit_hash_set(s),
                CustomStruct(c) => self.visit_steel_struct(c),
                PortV(p) => self.visit_port(p),
                IterV(t) => self.visit_transducer(t),
                ReducerV(r) => self.visit_reducer(r),
                FutureFunc(f) => self.visit_future_function(f),
                FutureV(f) => self.visit_future(f),
                StreamV(s) => self.visit_stream(s),
                BoxedFunction(b) => self.visit_boxed_function(b),
                ContinuationFunction(c) => self.visit_continuation(c),
                ListV(l) => self.visit_list(l),
                MutFunc(m) => self.visit_mutable_function(m),
                BuiltIn(b) => self.visit_builtin_function(b),
                MutableVector(b) => self.visit_mutable_vector(b),
                BoxedIterator(b) => self.visit_boxed_iterator(b),
                SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                Boxed(b) => self.visit_boxed_value(b),
                Reference(r) => self.visit_reference_value(r),
                BigNum(b) => self.visit_bignum(b),
                HeapAllocated(b) => self.visit_heap_allocated(b),
            };
        }

        // println!("--- finished draining drop queue ----");

        ret
    }
}

pub trait BreadthFirstSearchSteelValVisitor {
    type Output;

    fn default_output(&mut self) -> Self::Output;

    fn pop_front(&mut self) -> Option<SteelVal>;

    fn push_back(&mut self, value: SteelVal);

    fn visit(&mut self) -> Self::Output {
        let mut ret = self.default_output();

        while let Some(value) = self.pop_front() {
            ret = match value {
                Closure(c) => self.visit_closure(c),
                BoolV(b) => self.visit_bool(b),
                NumV(n) => self.visit_float(n),
                IntV(i) => self.visit_int(i),
                CharV(c) => self.visit_char(c),
                VectorV(v) => self.visit_immutable_vector(v),
                Void => self.visit_void(),
                StringV(s) => self.visit_string(s),
                FuncV(f) => self.visit_function_pointer(f),
                SymbolV(s) => self.visit_symbol(s),
                SteelVal::Custom(c) => self.visit_custom_type(c),
                HashMapV(h) => self.visit_hash_map(h),
                HashSetV(s) => self.visit_hash_set(s),
                CustomStruct(c) => self.visit_steel_struct(c),
                PortV(p) => self.visit_port(p),
                IterV(t) => self.visit_transducer(t),
                ReducerV(r) => self.visit_reducer(r),
                FutureFunc(f) => self.visit_future_function(f),
                FutureV(f) => self.visit_future(f),
                StreamV(s) => self.visit_stream(s),
                BoxedFunction(b) => self.visit_boxed_function(b),
                ContinuationFunction(c) => self.visit_continuation(c),
                ListV(l) => self.visit_list(l),
                MutFunc(m) => self.visit_mutable_function(m),
                BuiltIn(b) => self.visit_builtin_function(b),
                MutableVector(b) => self.visit_mutable_vector(b),
                BoxedIterator(b) => self.visit_boxed_iterator(b),
                SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                Boxed(b) => self.visit_boxed_value(b),
                Reference(r) => self.visit_reference_value(r),
                BigNum(b) => self.visit_bignum(b),
                HeapAllocated(b) => self.visit_heap_allocated(b),
            };
        }

        ret
    }

    fn visit_closure(&mut self, closure: Gc<ByteCodeLambda>) -> Self::Output;
    fn visit_bool(&mut self, boolean: bool) -> Self::Output;
    fn visit_float(&mut self, float: f64) -> Self::Output;
    fn visit_int(&mut self, int: isize) -> Self::Output;
    fn visit_char(&mut self, c: char) -> Self::Output;
    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output;
    fn visit_void(&mut self) -> Self::Output;
    fn visit_string(&mut self, string: SteelString) -> Self::Output;
    fn visit_function_pointer(&mut self, ptr: FunctionSignature) -> Self::Output;
    fn visit_symbol(&mut self, symbol: SteelString) -> Self::Output;
    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output;
    fn visit_hash_map(&mut self, hashmap: SteelHashMap) -> Self::Output;
    fn visit_hash_set(&mut self, hashset: SteelHashSet) -> Self::Output;
    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output;
    fn visit_port(&mut self, port: Gc<SteelPort>) -> Self::Output;
    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output;
    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output;
    fn visit_future_function(&mut self, function: BoxedAsyncFunctionSignature) -> Self::Output;
    fn visit_future(&mut self, future: Gc<FutureResult>) -> Self::Output;
    fn visit_stream(&mut self, stream: Gc<LazyStream>) -> Self::Output;
    fn visit_boxed_function(&mut self, function: Rc<BoxedDynFunction>) -> Self::Output;
    fn visit_continuation(&mut self, continuation: Gc<Continuation>) -> Self::Output;
    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output;
    fn visit_mutable_function(&mut self, function: MutFunctionSignature) -> Self::Output;
    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output;
    fn visit_builtin_function(&mut self, function: BuiltInSignature) -> Self::Output;
    fn visit_boxed_iterator(&mut self, iterator: Gc<RefCell<OpaqueIterator>>) -> Self::Output;
    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output;
    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output;
    fn visit_reference_value(&mut self, reference: Rc<OpaqueReference<'static>>) -> Self::Output;
    fn visit_bignum(&mut self, bignum: Gc<BigInt>) -> Self::Output;
    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output;
}

thread_local! {
    static LEFT_QUEUE: RefCell<VecDeque<SteelVal>> = RefCell::new(VecDeque::with_capacity(128));
    static RIGHT_QUEUE: RefCell<VecDeque<SteelVal>> = RefCell::new(VecDeque::with_capacity(128));
    static VISITED_SET: RefCell<fxhash::FxHashSet<usize>> = RefCell::new(fxhash::FxHashSet::default());
    static EQ_DEPTH: Cell<usize> = Cell::new(0);
}

fn increment_eq_depth() {
    #[cfg(feature = "sandbox")]
    EQ_DEPTH.with(|x| x.set(x.get() + 1));
}

fn decrement_eq_depth() {
    #[cfg(feature = "sandbox")]
    EQ_DEPTH.with(|x| x.set(x.get() - 1));
}

fn reset_eq_depth() {
    #[cfg(feature = "sandbox")]
    EQ_DEPTH.with(|x| x.set(0));
}

fn eq_depth() -> usize {
    #[cfg(feature = "sandbox")]
    return EQ_DEPTH.with(|x| x.get());

    #[cfg(not(feature = "sandbox"))]
    0
}

struct RecursiveEqualityHandler<'a> {
    left: EqualityVisitor<'a>,
    right: EqualityVisitor<'a>,
    visited: &'a mut fxhash::FxHashSet<usize>,
    found_mutable_object: bool,
}

impl<'a> RecursiveEqualityHandler<'a> {
    pub fn compare_equality(&mut self, left: SteelVal, right: SteelVal) -> bool {
        self.left.push_back(left);
        self.right.push_back(right);

        self.visit()
    }

    fn should_visit(&mut self, value: usize) -> bool {
        if self.found_mutable_object && self.visited.insert(value) {
            return true;
        }

        if !self.found_mutable_object {
            return true;
        }

        return false;
    }

    fn visit(&mut self) -> bool {
        loop {
            let (left, right) = match (self.left.pop_front(), self.right.pop_front()) {
                (Some(l), Some(r)) => (l, r),
                (None, None) => return true,
                _ => return false,
            };

            match (left, right) {
                (Closure(l), Closure(r)) => {
                    if l != r {
                        return false;
                    }

                    self.left.visit_closure(l);

                    continue;
                }
                (BoolV(l), BoolV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (NumV(l), NumV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (IntV(l), IntV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (CharV(l), CharV(r)) => {
                    if l != r {
                        return false;
                    }

                    continue;
                }
                (VectorV(l), VectorV(r)) => {
                    if l.len() != r.len() {
                        return false;
                    }

                    // If these point to the same object, break early
                    if Gc::ptr_eq(&l.0, &r.0) {
                        continue;
                    }

                    // Should we visit these?
                    if self.should_visit(l.0.as_ptr() as usize)
                        && self.should_visit(r.0.as_ptr() as usize)
                    {
                        self.left.visit_immutable_vector(l);
                        self.right.visit_immutable_vector(r);
                    } else {
                        return false;
                    }

                    continue;
                }
                (Void, Void) => {
                    continue;
                }
                (StringV(l), StringV(r)) => {
                    if l != r {
                        return false;
                    }
                    continue;
                }
                (FuncV(l), FuncV(r)) => {
                    if l as usize != r as usize {
                        return false;
                    }
                    continue;
                }
                (SymbolV(l), SymbolV(r)) => {
                    if l != r {
                        return false;
                    }
                    continue;
                }
                (SteelVal::Custom(l), SteelVal::Custom(r)) => {
                    if l.borrow().inner_type_id() != r.borrow().inner_type_id() {
                        return false;
                    }

                    if l.borrow().check_equality_hint(r.borrow().as_ref()) {
                        // Go down to the next level
                        self.left.visit_custom_type(l);
                        self.right.visit_custom_type(r);
                        continue;
                    } else {
                        return false;
                    }
                }
                (SteelVal::HashMapV(l), SteelVal::HashMapV(r)) => {
                    if Gc::ptr_eq(&l.0, &r.0) {
                        println!("Found ptr equality");

                        continue;
                    }

                    if self.should_visit(l.0.as_ptr() as usize)
                        && self.should_visit(r.0.as_ptr() as usize)
                    {
                        if l.len() != r.len() {
                            return false;
                        }

                        // TODO: Implicitly here we are assuming that this key was even hashable
                        // to begin with, since it ended up in the right spot, and didn't blow
                        // the stack on a recursive structure.
                        //
                        // This still does not handle the pathological edge case of something like
                        // (hash (hash (hash ...) value) value)
                        //
                        // In this case, we'll get a stack overflow, when trying to compare equality
                        // with these maps if they're sufficiently deep.
                        //
                        // The issue is that if the two maps are equivalent, we need to check the
                        // existence of each key in the left map with each key in the right map.
                        // Doing so invokes an equality check, where we'll then invoke this logic
                        // again. We could solve this by disallowing hashmaps as keys - then
                        // we would not the same issue where putting a hashmap into the map
                        // causes the equality checks to go off the rails.

                        if eq_depth() > 512 {
                            log::error!("Aborting eq checks before the stack overflows");

                            return false;
                        }

                        for (key, value) in l.0.iter() {
                            if let Some(right_value) = r.0.get(key) {
                                self.left.push_back(value.clone());
                                self.right.push_back(right_value.clone());
                            } else {
                                // We know that these are not equal, because the
                                // key in the left map does not exist in the right
                                return false;
                            }
                        }
                    }

                    continue;
                }
                (HashSetV(l), HashSetV(r)) => {
                    if Gc::ptr_eq(&l.0, &r.0) {
                        continue;
                    }

                    if self.should_visit(l.0.as_ptr() as usize)
                        && self.should_visit(r.0.as_ptr() as usize)
                    {
                        if l.len() != r.len() {
                            return false;
                        }
                        if eq_depth() > 512 {
                            log::error!("Aborting eq checks before the stack overflows");

                            return false;
                        }

                        for key in l.0.iter() {
                            if !l.0.contains(key) {
                                return false;
                            }
                        }
                    }

                    continue;
                }
                (CustomStruct(l), CustomStruct(r)) => {
                    // If these are the same object, just continue
                    if Gc::ptr_eq(&l, &r) {
                        continue;
                    }

                    if self.should_visit(l.as_ptr() as usize)
                        && self.should_visit(r.as_ptr() as usize)
                    {
                        // Check the top level equality indicators to make sure
                        // that these two types are the same
                        if !(l.type_descriptor == r.type_descriptor && l.name() == r.name()) {
                            return false;
                        }

                        self.left.visit_steel_struct(l);
                        self.right.visit_steel_struct(r);
                    }

                    continue;
                }
                // (PortV(_), PortV(_)) => {
                // return
                // }
                (IterV(l), IterV(r)) => {
                    self.left.visit_transducer(l);
                    self.right.visit_transducer(r);

                    continue;
                }
                (ReducerV(l), ReducerV(r)) => {
                    self.left.visit_reducer(l);
                    self.right.visit_reducer(r);

                    continue;
                }
                // FutureV(f) => self.visit_future(f),
                (ContinuationFunction(l), ContinuationFunction(r)) => {
                    if !Gc::ptr_eq(&l, &r) {
                        return false;
                    }

                    continue;
                }
                (ListV(l), ListV(r)) => {
                    // If we've reached the same object, we're good
                    if l.ptr_eq(&r) {
                        continue;
                    }

                    if self.should_visit(l.as_ptr_usize()) && self.should_visit(r.as_ptr_usize()) {
                        if l.len() != r.len() {
                            return false;
                        }

                        self.left.visit_list(l);
                        self.right.visit_list(r);
                    }

                    continue;
                }
                // MutFunc(m) => self.visit_mutable_function(m),
                (BuiltIn(l), BuiltIn(r)) => {
                    if l as usize != r as usize {
                        return false;
                    }
                    continue;
                }
                // MutableVector(b) => self.visit_mutable_vector(b),
                // BoxedIterator(b) => self.visit_boxed_iterator(b),
                // SteelVal::SyntaxObject(s) => self.visit_syntax_object(s),
                // Boxed(b) => self.visit_boxed_value(b),
                // Reference(r) => self.visit_reference_value(r),
                (BigNum(l), BigNum(r)) => {
                    if l != r {
                        return false;
                    }
                    continue;
                }
                (SyntaxObject(l), SyntaxObject(r)) => {
                    if Gc::ptr_eq(&l, &r) {
                        continue;
                    }

                    self.left.visit_syntax_object(l);
                    self.right.visit_syntax_object(r);
                }
                (HeapAllocated(l), HeapAllocated(r)) => {
                    self.left.visit_heap_allocated(l);
                    self.right.visit_heap_allocated(r);

                    continue;
                }
                (_, _) => {
                    return false;
                }
            }

            // unreachable!();
        }
    }
}

pub struct EqualityVisitor<'a> {
    // Mark each node that we've visited, if we encounter any mutable objects
    // on the way, then we'll start using the visited set. But we'll optimistically
    // assume that there are no mutable objects, and we won't start using this
    // until we absolutely have to.
    // found_mutable_object: bool,
    queue: &'a mut VecDeque<SteelVal>,
}

impl<'a> BreadthFirstSearchSteelValVisitor for EqualityVisitor<'a> {
    type Output = ();

    fn default_output(&mut self) -> Self::Output {}

    fn pop_front(&mut self) -> Option<SteelVal> {
        self.queue.pop_front()
    }

    fn push_back(&mut self, value: SteelVal) {
        self.queue.push_back(value)
    }

    fn visit_closure(&mut self, _closure: Gc<ByteCodeLambda>) -> Self::Output {}

    // Leaf nodes, we don't need to do anything here
    fn visit_bool(&mut self, _boolean: bool) -> Self::Output {}
    fn visit_float(&mut self, _float: f64) -> Self::Output {}
    fn visit_int(&mut self, _int: isize) -> Self::Output {}
    fn visit_char(&mut self, _c: char) -> Self::Output {}
    fn visit_void(&mut self) -> Self::Output {}
    fn visit_string(&mut self, _string: SteelString) -> Self::Output {}
    fn visit_function_pointer(&mut self, _ptr: FunctionSignature) -> Self::Output {}
    fn visit_symbol(&mut self, _symbol: SteelString) -> Self::Output {}
    fn visit_port(&mut self, _port: Gc<SteelPort>) -> Self::Output {}
    fn visit_boxed_function(&mut self, _function: Rc<BoxedDynFunction>) -> Self::Output {}
    fn visit_mutable_function(&mut self, _function: MutFunctionSignature) -> Self::Output {}
    fn visit_builtin_function(&mut self, _function: BuiltInSignature) -> Self::Output {}

    //
    fn visit_immutable_vector(&mut self, vector: SteelVector) -> Self::Output {
        // If we've found the mutable object, mark that this has been visited. Only
        // if self.should_visit(vector.0.as_ptr() as usize) {
        for value in vector.iter() {
            self.push_back(value.clone());
        }
        // }
    }

    // SHOULD SET MUTABLE HERE
    fn visit_custom_type(&mut self, custom_type: Gc<RefCell<Box<dyn CustomType>>>) -> Self::Output {
        custom_type.borrow().visit_children_for_equality(self);
    }

    fn visit_hash_map(&mut self, _hashmap: SteelHashMap) -> Self::Output {
        // TODO: See comment above
    }

    fn visit_hash_set(&mut self, _hashset: SteelHashSet) -> Self::Output {
        // TODO: See comment above
    }

    fn visit_steel_struct(&mut self, steel_struct: Gc<UserDefinedStruct>) -> Self::Output {
        // if self.should_visit(steel_struct.as_ptr() as usize) {
        for value in steel_struct.fields.iter() {
            self.push_back(value.clone());
        }
        // }
    }

    fn visit_transducer(&mut self, transducer: Gc<Transducer>) -> Self::Output {
        for transducer in transducer.ops.iter() {
            match transducer.clone() {
                crate::values::transducers::Transducers::Map(m) => self.push_back(m),
                crate::values::transducers::Transducers::Filter(v) => self.push_back(v),
                crate::values::transducers::Transducers::Take(t) => self.push_back(t),
                crate::values::transducers::Transducers::Drop(d) => self.push_back(d),
                crate::values::transducers::Transducers::FlatMap(fm) => self.push_back(fm),
                crate::values::transducers::Transducers::Flatten => {}
                crate::values::transducers::Transducers::Window(w) => self.push_back(w),
                crate::values::transducers::Transducers::TakeWhile(tw) => self.push_back(tw),
                crate::values::transducers::Transducers::DropWhile(dw) => self.push_back(dw),
                crate::values::transducers::Transducers::Extend(e) => self.push_back(e),
                crate::values::transducers::Transducers::Cycle => {}
                crate::values::transducers::Transducers::Enumerating => {}
                crate::values::transducers::Transducers::Zipping(z) => self.push_back(z),
                crate::values::transducers::Transducers::Interleaving(i) => self.push_back(i),
            }
        }
    }

    fn visit_reducer(&mut self, reducer: Gc<Reducer>) -> Self::Output {
        match reducer.as_ref().clone() {
            Reducer::ForEach(f) => self.push_back(f),
            Reducer::Generic(rf) => {
                self.push_back(rf.initial_value);
                self.push_back(rf.function);
            }
            _ => {}
        }
    }

    fn visit_future_function(&mut self, _function: BoxedAsyncFunctionSignature) -> Self::Output {}
    fn visit_future(&mut self, _future: Gc<FutureResult>) -> Self::Output {}
    fn visit_bignum(&mut self, _bignum: Gc<BigInt>) -> Self::Output {}

    fn visit_stream(&mut self, _stream: Gc<LazyStream>) -> Self::Output {}

    fn visit_continuation(&mut self, _continuation: Gc<Continuation>) -> Self::Output {}

    fn visit_list(&mut self, list: List<SteelVal>) -> Self::Output {
        for value in list {
            self.push_back(value);
        }
    }

    fn visit_mutable_vector(&mut self, vector: HeapRef<Vec<SteelVal>>) -> Self::Output {
        for value in vector.get().iter() {
            self.push_back(value.clone());
        }
    }

    fn visit_boxed_iterator(&mut self, _iterator: Gc<RefCell<OpaqueIterator>>) -> Self::Output {}

    fn visit_syntax_object(&mut self, syntax_object: Gc<Syntax>) -> Self::Output {
        if let Some(raw) = syntax_object.raw.clone() {
            self.push_back(raw);
        }

        self.push_back(syntax_object.syntax.clone());
    }

    fn visit_boxed_value(&mut self, boxed_value: Gc<RefCell<SteelVal>>) -> Self::Output {
        self.push_back(boxed_value.borrow().clone());
    }

    fn visit_reference_value(&mut self, _reference: Rc<OpaqueReference<'static>>) -> Self::Output {}

    // Should set mutable here
    fn visit_heap_allocated(&mut self, heap_ref: HeapRef<SteelVal>) -> Self::Output {
        // self.found_mutable_object = true;

        // if self.should_visit(heap_ref.as_ptr_usize()) {
        self.push_back(heap_ref.get());
        // }
    }
}

impl PartialEq for SteelVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Void, Void) => true,
            (BoolV(l), BoolV(r)) => l == r,
            (BigNum(l), BigNum(r)) => l == r,
            (IntV(l), IntV(r)) => l == r,

            // Floats shouls also be considered equal
            (NumV(l), NumV(r)) => l == r,

            (StringV(l), StringV(r)) => l == r,
            // (VectorV(l), VectorV(r)) => l == r,
            (SymbolV(l), SymbolV(r)) => l == r,
            (CharV(l), CharV(r)) => l == r,
            // (HashSetV(l), HashSetV(r)) => l == r,
            // (HashMapV(l), HashMapV(r)) => l == r,
            // (Closure(l), Closure(r)) => l == r,
            // (IterV(l), IterV(r)) => l == r,
            // (ListV(l), ListV(r)) => l == r,
            // (CustomStruct(l), CustomStruct(r)) => l == r,
            (FuncV(l), FuncV(r)) => *l as usize == *r as usize,
            // (Custom(l), Custom(r)) => Gc::ptr_eq(l, r),
            // (HeapAllocated(l), HeapAllocated(r)) => l.get() == r.get(),
            (left, right) => LEFT_QUEUE.with(|left_queue| {
                RIGHT_QUEUE.with(|right_queue| {
                    VISITED_SET.with(|visited_set| {
                        match (
                            left_queue.try_borrow_mut(),
                            right_queue.try_borrow_mut(),
                            visited_set.try_borrow_mut(),
                        ) {
                            (Ok(mut left_queue), Ok(mut right_queue), Ok(mut visited_set)) => {
                                let mut equality_handler = RecursiveEqualityHandler {
                                    left: EqualityVisitor {
                                        queue: &mut left_queue,
                                    },
                                    right: EqualityVisitor {
                                        queue: &mut right_queue,
                                    },
                                    visited: &mut visited_set,
                                    found_mutable_object: false,
                                };

                                let res =
                                    equality_handler.compare_equality(left.clone(), right.clone());

                                // EQ_DEPTH.with(|x| x.set(0));

                                reset_eq_depth();

                                // Clean up!
                                equality_handler.left.queue.clear();
                                equality_handler.right.queue.clear();
                                equality_handler.visited.clear();

                                res
                            }
                            _ => {
                                let mut left_queue = VecDeque::new();
                                let mut right_queue = VecDeque::new();

                                let mut visited_set = fxhash::FxHashSet::default();

                                // EQ_DEPTH.with(|x| x.set(x.get() + 1));

                                increment_eq_depth();

                                // println!("{}", EQ_DEPTH.with(|x| x.get()));

                                let mut equality_handler = RecursiveEqualityHandler {
                                    left: EqualityVisitor {
                                        queue: &mut left_queue,
                                    },
                                    right: EqualityVisitor {
                                        queue: &mut right_queue,
                                    },
                                    visited: &mut visited_set,
                                    found_mutable_object: false,
                                };

                                let res =
                                    equality_handler.compare_equality(left.clone(), right.clone());

                                // EQ_DEPTH.with(|x| x.set(x.get() - 1));

                                decrement_eq_depth();

                                res
                            }
                        }
                    })
                })
            }),
        }
    }
}
