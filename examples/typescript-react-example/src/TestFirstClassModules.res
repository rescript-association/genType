export convert = (x: FirstClassModules.firstClassModule) => x

export convertInterface = (x: FirstClassModulesInterface.firstClassModule) => x

export convertRecord = (x: FirstClassModulesInterface.record) => x

module type MT = {
  type outer
  let out: outer => outer

  module Inner: {
    type inner
    let inn: inner => inner
  }
}

export type firstClassModuleWithTypeEquations<'i, 'o> = module(MT with
  type Inner.inner = 'i
  and type outer = 'o
)

export convertFirstClassModuleWithTypeEquations = (
  type o i,
  x: module(MT with type Inner.inner = i and type outer = o),
) => x
