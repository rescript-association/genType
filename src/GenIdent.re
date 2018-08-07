/** Generate fresh identifiers */

type generator = {mutable propsType: int};

let initial = {propsType: 0};

let resetPerFile = () => initial.propsType = 0;

let jsTypeNameForAnonymousTypeID = id => "T" ++ string_of_int(id);

let propsTypeName = () => {
  initial.propsType = initial.propsType + 1;
  "Props" ++ (initial.propsType == 1 ? "" : string_of_int(initial.propsType));
};