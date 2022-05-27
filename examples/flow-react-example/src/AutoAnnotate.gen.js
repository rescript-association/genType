/** 
 * @flow strict
 * @generated from AutoAnnotate.res
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as AutoAnnotateBS from './AutoAnnotate.bs';

export type variant = {| tag: "R", value: number |};

export type record = {| +variant: variant |};

export type r2 = {| +r2: number |};

export type r3 = {| +r3: number |};

export type r4 = {| +r4: number |};

export type annotatedVariant = 
    {| tag: "R2", value: [r2, r3] |}
  | {| tag: "R4", value: r4 |};

export type r5 = {| +r5: number |};

export const useR5: (r5) => r5 = AutoAnnotateBS.useR5;
