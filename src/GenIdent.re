/** Generate fresh identifiers */

type generator = {mutable propsType: int};

let create = () => {propsType: 0};

let jsTypeNameForAnonymousTypeID = id => "T" ++ string_of_int(id);

let propsTypeName = (~generator) => {
  generator.propsType = generator.propsType + 1;
  "Props" ++ (generator.propsType == 1 ? "" : string_of_int(generator.propsType));
};