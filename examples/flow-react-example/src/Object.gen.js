/** 
 * @flow strict
 * @generated from Object.res
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently
import * as ObjectBS from './Object.bs';

export type openObject<a> = { +foo: number, ... };

export const foo: (openObject<{| +foo: number |}>) => openObject<{| +foo: number |}> = ObjectBS.foo;

export const useProps: ({ +foo: number, ... }) => number = ObjectBS.useProps;

export const usePropsClosed: ({| +fooClosed: number |}) => number = ObjectBS.usePropsClosed;
