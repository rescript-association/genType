/* @flow strict */

import * as References from './References.gen';

const r : [number] = [34];

r[0] = 42;

export const n : number = References.access(r);


const ar : References.t<number> = References.make(34);

References.set(ar, 42);

export const an : number = References.get(ar);
