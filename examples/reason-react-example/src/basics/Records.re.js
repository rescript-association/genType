/** 
 * @flow strict
 * @generated 
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
const RecordsBS = require("./Records.bs");

// No need to import locally visible type coord. Make sure it is also marked with @genType

// $FlowExpectedError: Reason checked type sufficiently
export type coord = {|x:number, y:number, z?:number|};
export const origin: coord = {x:RecordsBS.origin[0], y:RecordsBS.origin[1], z:RecordsBS.origin[2]};
export const computeArea: (coord) => number = function _(Arg1) { const result = RecordsBS.computeArea([Arg1.x, Arg1.y, (Arg1.z == null ? undefined : Arg1.z)]); return result };
export const coord2d: (number, number) => coord = function _(Arg1, Arg2) { const result = RecordsBS.coord2d(Arg1, Arg2); return {x:result[0], y:result[1], z:result[2]} };