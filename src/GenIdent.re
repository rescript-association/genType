/** Generate fresh identifiers */

let propsTypeNameCount = {contents: 0};

let resetPerFile = () => propsTypeNameCount.contents = 0;

let propsTypeName = () => {
  propsTypeNameCount.contents = propsTypeNameCount.contents + 1;
  "Props"
  ++ (
    propsTypeNameCount.contents == 1 ?
      "" : string_of_int(propsTypeNameCount.contents)
  );
};