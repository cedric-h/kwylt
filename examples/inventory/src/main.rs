const SCRIPT: &'static str = r#"
!:global INIT = { ${ count = 0 } };
!:global UPDATE = { _ + _1 };
!:global VIEW = {||
    box $[ on_click { 1 } ] $[]
};
"#;

fn main() {
    use wlambda::EvalContext;
    let sc = qwlt::qwlt_scope();
    let mut ctx = EvalContext::new(sc);

    ctx.eval(SCRIPT).expect("couldn't eval SCRIPT");

    let init = ctx.get_global_var("INIT").expect("no INIT in script");
    let update = ctx.get_global_var("UPDATE").expect("no UPDATE in script");
    let view = ctx.get_global_var("VIEW").expect("no VIEW in script");

    // make initial model and view
    let model = ctx.call(&init, &[]).expect("INIT call err");
    let view = ctx
        .call(&view, &[model.clone()])
        .expect("VIEW inital call err");

    println!("view: {}", view.s());
}

mod qwlt {
    use std::cell::RefCell;
    use std::rc::Rc;
    use wlambda::vval::VValUserData;
    use wlambda::VVal;

    #[derive(Debug, Clone)]
    enum ElemKind {
        Box,
    }
    #[derive(Debug, Clone)]
    struct Elem {
        kind: ElemKind,
        attr: Vec<Attr>,
        kids: Vec<ElemRef>,
    }
    impl Elem {
        fn empty_kind(kind: ElemKind) -> Elem {
            Elem {
                kind,
                attr: Vec::new(),
                kids: Vec::new(),
            }
        }
    }
    #[derive(Clone, Debug)]
    struct ElemRef(Rc<RefCell<Elem>>);
    impl ElemRef {
        fn new(e: Elem) -> Self {
            ElemRef(Rc::new(RefCell::new(e)))
        }
    }
    impl VValUserData for ElemRef {
        fn s(&self) -> String {
            let e = self.0.borrow();
            format!(
                "$<{:?}([{}],[{}])>",
                e.kind,
                e.attr.iter().map(|x| x.s()).collect::<Vec<String>>().join(","),
                e.kids.iter().map(|x| x.s()).collect::<Vec<String>>().join(",")
            )
        }
        fn call(&self, args: &[VVal]) -> Result<VVal, wlambda::StackAction> {
            if args.len() != 2 && args.len() != 0 {
                return Err(wlambda::StackAction::panic_msg(format!(
                    "{} called with too few arguments: {:?}",
                    self.s(),
                    args
                )));
            }
            if let Some(VVal::Lst(attr)) = args.get(0) {
                self.0.borrow_mut().attr.append(
                    &mut attr
                        .borrow_mut()
                        .iter_mut()
                        .filter_map(|v| {
                            if let VVal::Usr(ref mut u) = v {
                                if let Some(attr) = u.as_any().downcast_ref::<Attr>() {
                                    return Some(attr.clone());
                                }
                            }
                            None
                        })
                        .collect(),
                );
            }
            if let Some(VVal::Lst(kids)) = args.get(1) {
                self.0.borrow_mut().kids.append(
                    &mut kids
                        .borrow_mut()
                        .iter_mut()
                        .filter_map(|v| {
                            if let VVal::Usr(ref mut u) = v {
                                if let Some(kid) = u.as_any().downcast_ref::<ElemRef>() {
                                    return Some(kid.clone());
                                }
                            }
                            None
                        })
                        .collect(),
                );
            }
            Ok(VVal::Nul)
        }
        fn as_any(&mut self) -> &mut dyn std::any::Any {
            self
        }
        fn clone_ud(&self) -> Box<dyn VValUserData> {
            Box::new(ElemRef::new(self.0.borrow().clone()))
        }
    }

    #[derive(Debug, Clone)]
    enum Attr {
        OnClick(VVal),
    }
    impl VValUserData for Attr {
        fn s(&self) -> String {
            format!(
                "$<Attr({})>",
                match self {
                    Attr::OnClick(f) => format!("OnClick({})", f.s())
                }
            )
        }
        fn as_any(&mut self) -> &mut dyn std::any::Any {
            self
        }
        fn clone_ud(&self) -> Box<dyn VValUserData> {
            Box::new(self.clone())
        }
    }

    pub fn qwlt_scope() -> Rc<RefCell<wlambda::compiler::GlobalEnv>> {
        use wlambda::compiler::GlobalEnv;
        use wlambda::vval::Env;

        let g = GlobalEnv::new();
        g.borrow_mut().add_func(
            "box",
            |env: &mut Env, argc: usize| {
                let b = ElemRef::new(Elem::empty_kind(ElemKind::Box));
                if 2 == argc {
                    b.call(&[env.arg(0), env.arg(1)])?;
                }
                Ok(VVal::Usr(Box::new(b)))
            },
            None,
            Some(2),
        );
        g.borrow_mut().add_func(
            "on_click",
            |env: &mut Env, _argc: usize| Ok(VVal::Usr(Box::new(Attr::OnClick(env.arg(0))))),
            Some(1),
            Some(1),
        );

        g
    }
}
