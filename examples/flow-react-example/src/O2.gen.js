/** 
 * @flow strict
 * @generated from O2.res
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as O2BS from './O2.bs';

export type openObject<a> = { +foo: number, ... };

export type t1<a> = {||};

export type t2 = {||};

export const foo: (openObject<{| +foo: number |}>) => openObject<{| +foo: number |}> = O2BS.foo;

export const useProps: ({ +foo: number, ... }) => number = O2BS.useProps;

export const usePropsClosed: ({| +fooClosed: number |}) => number = O2BS.usePropsClosed;
