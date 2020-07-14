// TODO: rename metodd back once remmt bug is fixed
@genType.import("./") @react.component
external make: (~method: @bs.string [ | #push | #replace ]=?) => React.element = "default"
