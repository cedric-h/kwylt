const SCRIPT: &'static str = r#"
!:global INIT = {
    ${
        down = $f,
        pos = $[0, 0],
        last_mouse = $n,
    }
};
!:global UPDATE = {!(model, msg)=@;
    (is_some msg.down) {
        (not msg.down) {
            model.last_mouse = $n;
        };
        model.down = msg.down;
    };
    (is_some msg.mouse_pos) {!(x, y)=msg.mouse_pos;
        (is_some model.last_mouse) {
            model.pos.0 = model.pos.0 + (x - model.last_mouse.0);
            model.pos.1 = model.pos.1 + (y - model.last_mouse.1);
        };
        model.last_mouse = msg.mouse_pos;
    };
    model
};
!:global VIEW = {!model=_;
    box $[
            color "black",
            size 800 600,
            model.down { on_mouse_move { ${ mouse_pos = @ } } },
            on_mouse_up { ${ down = $f } },
        ] $[
            box $[
                    on_mouse_down { ${ down = $t } },
                    size 100,
                    pos model.pos.0 model.pos.1,
                    color ~ model.down "gray" "white",
                ] $[
                ]
        ]
};
"#;

fn main() {
    quicksilver::lifecycle::run(
        quicksilver::lifecycle::Settings {
            size: quicksilver::geom::Vector::new(800.0, 600.0).into(),
            title: "Square Example",
            ..quicksilver::lifecycle::Settings::default()
        },
        app,
    );
}
async fn app(
    window: quicksilver::lifecycle::Window,
    mut gfx: quicksilver::graphics::Graphics,
    mut events: quicksilver::lifecycle::EventStream,
) -> quicksilver::Result<()> {
    let sc = wlambda::compiler::GlobalEnv::new_default();
    wl::add_qwlt(&sc);
    let mut ctx = wlambda::EvalContext::new(sc);

    ctx.eval(SCRIPT).expect("couldn't eval SCRIPT");

    let mut qwl =
        state::Qwl::from_ctx(&mut ctx).unwrap_or_else(|e| panic!("Couldn't create qwl: {}", e));

    loop {
        while let Some(ev) = events.next_event().await {
            qwl.capture_event(&mut ctx, &ev);
            /* use quicksilver::lifecycle::Event;
            match ev {} */
        }

        gfx.clear(quicksilver::graphics::Color::BLACK);
        qwl.render(&mut ctx, &mut gfx);
        gfx.present(&window)?;
    }
}

mod state {
    use std::fmt;
    use wlambda::{EvalContext, VVal};

    pub enum RequiredFn {
        Init,
        Update,
        View,
    }
    impl fmt::Display for RequiredFn {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use RequiredFn::*;
            write!(
                f,
                "{}",
                match self {
                    Init => "INIT",
                    Update => "UPDATE",
                    View => "VIEW",
                }
            )
        }
    }
    pub enum CallError {
        InvalidVVal(VVal, String),
        UnexpectedStackAction(wlambda::StackAction),
    }
    pub enum QwlCreationError {
        MissingFn(RequiredFn),
        InitialCall(RequiredFn, CallError),
    }
    impl fmt::Display for QwlCreationError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use CallError::*;
            use QwlCreationError::*;
            match self {
                MissingFn(fun) => write!(
                    f,
                    "No {} variable could be found in the provided EvalContext's global variables.",
                    fun,
                ),
                InitialCall(fun, result) => write!(
                    f,
                    "Initial call to {} yielded {}",
                    fun,
                    match result {
                        InvalidVVal(v, expect) => format!(
                            "an invalid VVal, \"{}\" expected VVal of type {}",
                            v.s(),
                            expect
                        ),
                        UnexpectedStackAction(sa) => format!(
                            "an unexpected stack action({}): {}",
                            sa, "these functions should be top level global variables"
                        ),
                    }
                ),
            }
        }
    }

    pub struct View {
        pub(crate) fun: VVal,
        pub(crate) needs_compile: bool,
        pub(crate) compiled: Vec<super::shapes::Shape>,
    }

    pub struct Qwl {
        pub(crate) model: VVal,
        pub(crate) update: VVal,
        pub(crate) view: View,
        pub(crate) input: super::input::Input,
    }
    impl Qwl {
        pub fn from_ctx(ctx: &mut EvalContext) -> Result<Self, QwlCreationError> {
            let init = ctx
                .get_global_var("INIT")
                .ok_or_else(|| QwlCreationError::MissingFn(RequiredFn::Init))?;
            let update = ctx
                .get_global_var("UPDATE")
                .ok_or_else(|| QwlCreationError::MissingFn(RequiredFn::Update))?;
            let view = ctx
                .get_global_var("VIEW")
                .ok_or_else(|| QwlCreationError::MissingFn(RequiredFn::View))?;

            // make initial model
            let model = ctx.call(&init, &[]).map_err(|e| {
                QwlCreationError::InitialCall(RequiredFn::Init, CallError::UnexpectedStackAction(e))
            })?;

            Ok(Self {
                model,
                update,
                view: View {
                    fun: view,
                    needs_compile: true,
                    compiled: Vec::new(),
                },
                input: Default::default(),
            })
        }
    }
}

mod input {
    use super::shapes::{Shape, ShapeKind};
    use quicksilver::geom::{Shape as _, Vector};
    use quicksilver::lifecycle::{Event, MouseButton};
    use wlambda::{EvalContext, VVal};

    /// Keeps a bunch of Input across frames,
    /// for telling when things like mouse releases and clicks happen.
    /// quicksilver::event_cache could also be used here, but we need
    /// so little data that that feels like overkill.
    pub struct Input {
        mouse_down: bool,
        mouse_loc: Vector,
    }
    impl Default for Input {
        fn default() -> Self {
            Input {
                mouse_down: false,
                mouse_loc: Vector::ONE,
            }
        }
    }

    #[inline]
    #[must_use]
    fn contains(s: &Shape, v: Vector) -> bool {
        match s.kind {
            ShapeKind::Box => super::shapes::BOX.contains(s.transform.inverse() * v),
        }
    }

    impl super::state::Qwl {
        /// Returns a bool indicating whether or not Qwl wants to "capture" the event,
        /// i.e. your application shouldn't use it because it was over some Qwl GUI.
        pub fn capture_event(&mut self, ctx: &mut EvalContext, ev: &Event) -> bool {
            match ev {
                Event::PointerMoved(p) => {
                    let loc = p.location();
                    self.input.mouse_loc = loc.into();
                    // shapes are sorted by z-index so this should be fine
                    if let Some(msg) = self
                        .view
                        .compiled
                        .iter()
                        .find_map(|s| {
                            s.handlers
                                .on_mouse_move
                                .as_ref()
                                .filter(|_| contains(s, self.input.mouse_loc))
                        })
                        .and_then(|h| ctx.call(h, &[VVal::Flt(loc.x as f64), VVal::Flt(loc.y as f64)]).ok())
                    {
                        self.call_update(ctx, msg);
                        return true;
                    }
                },
                Event::PointerInput(p) => {
                    // if the old isn't the new, we know something changed.
                    // (and this event isn't just a repeat)
                    let mouse_change = p.is_down() != self.input.mouse_down;
                    self.input.mouse_down = p.is_down();

                    if let (MouseButton::Left, true) = (p.button(), mouse_change) {
                        if p.is_down() {
                            // shapes are sorted by z-index so this should be fine
                            if let Some(msg) = self
                                .view
                                .compiled
                                .iter()
                                .find_map(|s| {
                                    s.handlers
                                        .on_mouse_down
                                        .as_ref()
                                        .filter(|_| contains(s, self.input.mouse_loc))
                                })
                                .and_then(|h| ctx.call(h, &[]).ok())
                            {
                                self.call_update(ctx, msg);
                                return true;
                            }
                        } else {
                            // shapes are sorted by z-index so this should be fine
                            if let Some(msg) = self
                                .view
                                .compiled
                                .iter()
                                .find_map(|s| {
                                    s.handlers
                                        .on_mouse_up
                                        .as_ref()
                                        .filter(|_| contains(s, self.input.mouse_loc))
                                })
                                .and_then(|h| ctx.call(h, &[]).ok())
                            {
                                self.call_update(ctx, msg);
                                return true;
                            }
                        }
                    }
                }
                _ => {}
            }
            false
        }

        /// Calls update with the provided message and the current model,
        /// replacing the model with the new one UPDATE returns
        /// and flags for the view to be regenerated accordingly.
        pub fn call_update(&mut self, ctx: &mut EvalContext, msg: VVal) {
            if let Ok(model) = ctx.call(&self.update, &[self.model.clone(), msg]) {
                println!("new model: {}", model.s());
                self.model = model;
                self.view.needs_compile = true;
            }
        }
    }
}

mod shapes {
    use super::wl::{Attr, Elem, ElemKind};
    use quicksilver::geom::{Transform, Vector};
    use wlambda::VVal;

    /// This "unit box" is then scaled with transforms to make the boxes you see on the screen.
    pub const BOX: quicksilver::geom::Rectangle = quicksilver::geom::Rectangle {
        pos: Vector { x: 0.0, y: 0.0 },
        size: Vector { x: 1.0, y: 1.0 },
    };

    #[derive(Debug, Clone)]
    pub enum ShapeKind {
        Box,
    }
    pub struct Shape {
        pub(crate) kind: ShapeKind,
        pub(crate) transform: Transform,
        pub(crate) color: quicksilver::graphics::Color,
        pub(crate) z_index: i64,
        pub(crate) handlers: Handlers,
    }
    #[derive(Default)]
    pub struct Handlers {
        //pub(crate) on_click: Option<VVal>,
        pub(crate) on_mouse_move: Option<VVal>,
        pub(crate) on_mouse_down: Option<VVal>,
        pub(crate) on_mouse_up: Option<VVal>,
    }
    impl Handlers {
        fn empty() -> Self {
            Default::default()
        }
    }

    /// Compiles the view Elem into a list of shapes.
    pub fn compile_view(view: Elem, shapes: &mut Vec<Shape>) {
        shapes.clear();
        compile_elem(view, Transform::IDENTITY, 0, shapes);
        shapes.sort_by_key(|s| s.z_index);
    }

    fn compile_elem(e: Elem, t: Transform, mut z: i64, shapes: &mut Vec<Shape>) {
        let mut local = Transform::IDENTITY;
        let mut kids = Transform::IDENTITY;
        let mut color = quicksilver::graphics::Color::WHITE;
        let mut handlers = Handlers::empty();

        for attr in e.attr.into_iter() {
            #[rustfmt::skip]
            match attr {
                //
                // -- HANDLERS --
                // 
                //Attr::OnClick(v)     => handlers.on_click      = Some(v),
                Attr::OnMouseUp(v)   => handlers.on_mouse_up   = Some(v),
                Attr::OnMouseDown(v) => handlers.on_mouse_down = Some(v),
                Attr::OnMouseMove(v) => handlers.on_mouse_move = Some(v),
                //
                // -- TRANSFORM --
                // 
                // another dimension
                Attr::Z(z_offset) => z += z_offset,
                // kids too
                Attr::Pos(v)  => kids *= Transform::translate(v),
                Attr::Rot(a)  => kids *= Transform::rotate(a),
                Attr::Zoom(v) => kids *= Transform::scale(v),
                // just local
                Attr::Offset(v) => local *= Transform::translate(v),
                Attr::Angle(a)  => local *= Transform::rotate(a),
                Attr::Size(v)   => local *= Transform::scale(v),
                //
                // -- STYLE --
                //
                Attr::Color(c) => color = c,
            };
        }

        shapes.push(Shape {
            kind: match e.kind {
                ElemKind::Box => ShapeKind::Box,
            },
            transform: t * kids * local,
            z_index: z,
            color,
            handlers,
        });

        for kid in e.kids.into_iter() {
            compile_elem(kid, t * kids, z + 1, shapes);
        }
    }

    impl super::state::Qwl {
        pub fn render(
            &mut self,
            ctx: &mut wlambda::EvalContext,
            gfx: &mut quicksilver::graphics::Graphics,
        ) {
            if self.view.needs_compile {
                println!("compile");
                super::shapes::compile_view(
                    ctx.call(&self.view.fun, &[self.model.clone()])
                        .ok()
                        .and_then(|v| super::wl::Elem::from_vval(v).ok())
                        .expect("view function crash"),
                    /*
                        .map_err(|e| CallError::UnexpectedStackAction(e))
                        .and_then(|v| {
                            super::wl::Elem::from_vval(v)
                                .map_err(|e| CallError::InvalidVVal(e, "Elem type".to_string()))
                        })
                        .map_err(|e| QwlCreationError::InitialCall(RequiredFn::View, e))?,
                    */
                    &mut self.view.compiled,
                );
                self.view.needs_compile = false;
            }

            for shape in self.view.compiled.iter() {
                gfx.set_transform(shape.transform);
                match shape.kind {
                    ShapeKind::Box => {
                        gfx.fill_rect(&BOX, shape.color);
                    }
                }
            }
        }
    }
}

mod wl {
    use quicksilver::geom::Vector;
    use std::cell::RefCell;
    use std::rc::Rc;
    use wlambda::vval::VValUserData;
    use wlambda::VVal;
    use quicksilver::graphics::Color;

    #[derive(Debug, Clone)]
    pub enum ElemKind {
        Box,
    }
    #[derive(Debug, Clone)]
    pub struct Elem {
        pub kind: ElemKind,
        pub attr: Vec<Attr>,
        pub kids: Vec<Elem>,
    }
    impl Elem {
        #[inline]
        /// Returns an empty Elem of the given kind.
        fn empty_kind(kind: ElemKind) -> Elem {
            Elem {
                kind,
                attr: Vec::new(),
                kids: Vec::new(),
            }
        }

        #[inline]
        /// Turn a `VVal` into an `Elem`.
        /// Returns the `VVal` as the `Result`'s Err if it wasn't an `Elem`.
        pub fn from_vval(mut v: VVal) -> Result<Self, VVal> {
            if let VVal::Usr(ref mut u) = v {
                if let Some(el) = u.as_any().downcast_ref::<ElemRef>() {
                    return Ok(el.0.borrow().clone());
                }
            }
            Err(v)
        }

        #[inline]
        /// Turn a `&mut VVal` into an `Elem`.
        /// Returns `None` if the VVal wasn't an `Elem`.
        pub fn from_vval_ref(v: &mut VVal) -> Option<Self> {
            if let VVal::Usr(u) = v {
                if let Some(el) = u.as_any().downcast_ref::<ElemRef>() {
                    return Some(el.0.borrow().clone());
                }
            }
            None
        }

        /// For use by ElemRef as a VValUserData
        fn s(&self) -> String {
            format!(
                "$<{:?}([{}],[{}])>",
                self.kind,
                self.attr
                    .iter()
                    .map(|x| x.s())
                    .collect::<Vec<String>>()
                    .join(","),
                self.kids
                    .iter()
                    .map(|x| x.s())
                    .collect::<Vec<String>>()
                    .join(",")
            )
        }
    }
    #[derive(Clone, Debug)]
    pub struct ElemRef(Rc<RefCell<Elem>>);
    impl ElemRef {
        #[inline]
        pub fn new(e: Elem) -> Self {
            ElemRef(Rc::new(RefCell::new(e)))
        }
    }
    impl VValUserData for ElemRef {
        fn s(&self) -> String {
            self.0.borrow().s()
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
                        .filter_map(|v| Attr::from_vval_ref(v))
                        .collect(),
                );
            }
            if let Some(VVal::Lst(kids)) = args.get(1) {
                self.0.borrow_mut().kids.append(
                    &mut kids
                        .borrow_mut()
                        .iter_mut()
                        .filter_map(|v| Elem::from_vval_ref(v))
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
    pub enum Attr {
        //
        // -- LISTENERS --
        //
        //OnClick(VVal),
        OnMouseMove(VVal),
        OnMouseDown(VVal),
        OnMouseUp(VVal),
        //
        // -- TRANSFORM --
        //
        // kids too
        Pos(quicksilver::geom::Vector),
        Rot(f32),
        Zoom(quicksilver::geom::Vector),
        // jus' local
        Offset(quicksilver::geom::Vector),
        Angle(f32),
        Size(quicksilver::geom::Vector),
        // another dimension
        Z(i64),
        //
        // -- STYLE --
        //
        Color(Color)
    }
    impl Attr {
        /// Turn a VVal into an Attr.
        /// Returns the VVal as the Result's Err if it wasn't an Attr.
        #[inline]
        fn from_vval_ref(v: &mut VVal) -> Option<Self> {
            if let VVal::Usr(u) = v {
                if let Some(attr) = u.as_any().downcast_ref::<Attr>() {
                    return Some(attr.clone());
                }
            }
            None
        }
    }
    impl VValUserData for Attr {
        fn s(&self) -> String {
            format!(
                "$<Attr({})>",
                match self {
                    //Attr::OnClick(f) => format!("OnClick({})", f.s()),
                    o => format!("{:?}", o),
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

    pub fn add_qwlt(g: &Rc<RefCell<wlambda::compiler::GlobalEnv>>) {
        use wlambda::vval::Env;

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

        const COLORS: &'static [(&'static str, Color)] = &[
            ("white", Color::WHITE),
            ("black", Color::BLACK),
            ("red", Color::RED),
            ("orange", Color::ORANGE),
            ("yellow", Color::YELLOW),
            ("green", Color::GREEN),
            ("cyan", Color::CYAN),
            ("blue", Color::BLUE),
            ("magenta", Color::MAGENTA),
            ("purple", Color::PURPLE),
            ("indigo", Color::INDIGO),
        ];
        let colors: fxhash::FxHashMap<String, Color> = COLORS
            .iter()
            .copied()
            .map(|(s, c)| (s.to_string(), c))
            .collect();
        g.borrow_mut().add_func(
            "color",
            move |env: &mut Env, argc: usize| {
                Ok(VVal::Usr(Box::new(Attr::Color(match argc {
                    1 => colors.get(&env.arg(0).s_raw()).cloned().unwrap_or(Color::MAGENTA),
                    4 => Color {
                        r: env.arg(0).f() as f32,
                        g: env.arg(1).f() as f32,
                        b: env.arg(2).f() as f32,
                        a: env.arg(3).f() as f32,
                    },
                    _ => return Err(wlambda::StackAction::panic_msg(format!(
                            "\"color\" called with too few arguments: {:?} (expected 1 or 4)",
                            env.argv()
                        )))
                }))))
            },
            Some(1),
            Some(4),
        );

        g.borrow_mut().add_func(
            "z",
            |env: &mut Env, _argc: usize| Ok(VVal::Usr(Box::new(Attr::Z(env.arg(0).i())))),
            Some(1),
            Some(1),
        );

        macro_rules! take_a_vval {
            ( $( $name:literal: $attr:ident; )* ) => {
                $(
                    g.borrow_mut().add_func(
                        $name,
                        |env: &mut Env, _argc: usize| Ok(VVal::Usr(Box::new(Attr::$attr(env.arg(0))))),
                        Some(1),
                        Some(1),
                    );
                )*
            }
        }
        take_a_vval!(
            "on_click"      : OnMouseDown ;
            "on_mouse_move" : OnMouseMove ;
            "on_mouse_up"   : OnMouseUp   ;
            "on_mouse_down" : OnMouseDown ;
        );

        macro_rules! vec_translators {
            ( $( $name:literal: $attr:ident; )* ) => {
                $(
                g.borrow_mut().add_func(
                    $name,
                    |env: &mut Env, argc: usize| {
                        Ok(VVal::Usr(Box::new(Attr::$attr(match argc {
                            1 => Vector::ONE * (env.arg(0).f() as f32),
                            _ => Vector::new(env.arg(0).f() as f32, env.arg(1).f() as f32),
                        }))))
                    },
                    Some(1),
                    Some(2),
                );
                )*
            }
        }
        vec_translators!(
            "size"   : Size   ;
            "zoom"   : Zoom   ;
            "pos"    : Pos    ;
            "offset" : Offset ;
        );
        g.borrow_mut().add_func(
            "rot",
            |env: &mut Env, _: usize| Ok(VVal::Usr(Box::new(Attr::Rot(env.arg(0).f() as f32)))),
            Some(1),
            Some(1),
        );
        g.borrow_mut().add_func(
            "angle",
            |env: &mut Env, _: usize| Ok(VVal::Usr(Box::new(Attr::Angle(env.arg(0).f() as f32)))),
            Some(1),
            Some(1),
        );
    }
}
