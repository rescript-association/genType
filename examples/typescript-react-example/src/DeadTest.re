let _ = Js.log(ImmutableArray.fromArray);
let fortytwo = 42;

[@genType]
let fortyTwoButExported = 42;

let thisIsUsedOnce = 34;
ignore(thisIsUsedOnce);

let thisIsUsedTwice = 34;
ignore(thisIsUsedTwice);
ignore(thisIsUsedTwice);

[@dead]
let thisIsMarkedDead = 99;

let thisIsKeptAlive = 42;

[@live]
let thisIsMarkedLive = thisIsKeptAlive;

module Inner = {
  [@dead]
  let thisIsAlsoMarkedDead = 99;
};

module M: {
  [@dead]
  let thisSignatureItemIsDead: int;
} = {
  let thisSignatureItemIsDead = 34;
};

module VariantUsedOnlyInImplementation: {
  type t =
    | A; // TODO: discovered this automatically
  let a: t;
} = {
  type t =
    | A;
  let a = A;
};

let _ = (x => x)(VariantUsedOnlyInImplementation.a);

let _ = DeadTypeTest.OnlyInInterface;
let _ = DeadTypeTest.InBoth;

type record = {
  xxx: int,
  yyy: int,
};

let _ = r => r.xxx;
let _ = ({yyy}) => yyy;

module UnderscoreInside = {
  let _ = 13;
};

module MM: {
  let x: int;
  let y: int;
} = {
  let y = 55;
  let x = {
    y;
  };
  let valueOnlyInImplementation = 7;
};

let _ = {
  Js.log(MM.x);
  44;
};

let () = Js.log(DeadValueTest.valueAlive);

let rec unusedRec = () => unusedRec();

let rec split_map = l => {
  let _ = split_map(l);
  [];
};

let rec rec1 = () => rec2()
and rec2 = () => rec1();

let rec recWithCallback = () => {
  let cb = () => recWithCallback();
  cb();
};

let rec foo = () => {
  let cb = () => bar();
  cb();
}
and bar = () => foo();

let withDefaultValue = (~paramWithDefault=3, y) => paramWithDefault + y;

external unsafe_string1: (bytes, int, int) => Digest.t = "caml_md5_string";

module Ext_buffer: {
  external unsafe_string2: (bytes, int, int) => Digest.t = "caml_md5_string";
} = {
  external unsafe_string2: (bytes, int, int) => Digest.t = "caml_md5_string";
};

let () = Js.log(DeadRT.Root("xzz"));

module LazyDynamicallyLoadedComponent = [%lazyLoadComponent
  DynamicallyLoadedComponent
];

module type LocalDynamicallyLoadedComponent2 = (module type of DynamicallyLoadedComponent);

module LazyDynamicallyLoadedComponent2 = {
  let reasonResource: JSResource.t(module LocalDynamicallyLoadedComponent2) =
    JSResource.jSResource("DynamicallyLoadedComponent.bs");
  let makeProps = DynamicallyLoadedComponent.makeProps;
  let make = props =>
    React.createElement(
      {
        module Comp = (val BootloaderResource.read(reasonResource));
        Comp.make;
      },
      props,
    );
};

let cmp = <LazyDynamicallyLoadedComponent s="hello" />;

let cmp2 = () => <LazyDynamicallyLoadedComponent2 s="hello" />;

let () = Js.log(cmp);

module Chat = {};

module ComponentSwitch = (
  val [%requireCond
        (
          `gk,
          "chat",
          {"false": DynamicallyLoadedComponent, "true": ExportWithRename},
        )
      ]
);

let zzz = {
  let a1 = 1;
  let a2 = 2;
  let a3 = 3;
  ();
};

let () = Js.log(<DynamicallyLoadedComponent s="" />);

let second = 1L;
let minute = Int64.mul(60L, second);

let deadRef = ref(12);

let makeSwitch = ComponentSwitch.make;

[@react.component]
let make = s => React.string(s);

let () = Js.log(make);

let theSideEffectIsLogging = Js.log(123);

let stringLengthNoSideEffects = String.length("sdkdl");