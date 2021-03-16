/** 
 * @flow strict
 * @generated from Object2.res
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as Object2BS from './Object2.bs';

export type openObject<a> = { +foo: number, ... };

export type t1<a> = {||};

export type t2 = {||};

export const foo: (openObject<{| +foo: number |}>) => openObject<{| +foo: number |}> = Object2BS.foo;

export const useProps: ({ +foo: number, ... }) => number = Object2BS.useProps;

export const usePropsClosed: ({| +fooClosed: number |}) => number = Object2BS.usePropsClosed;
