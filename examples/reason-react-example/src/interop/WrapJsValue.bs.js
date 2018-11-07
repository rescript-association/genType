import * as WrapJsValueRe from "./WrapJsValue.re";

export const lazy = () => ({
  roundedNumber: WrapJsValueRe.round(1.8)
});
