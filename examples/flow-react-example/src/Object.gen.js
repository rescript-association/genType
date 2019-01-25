/** 
 * @flow strict
 * @generated
 * @nolint
 */
/* eslint-disable */

// $FlowExpectedError: Reason checked type sufficiently
import * as ObjectBS from './Object.bs';

export type openObject<T1> = {| +foo: number |};

export const foo: (openObject<mixed>) => openObject<mixed> = ObjectBS.foo;

export const useProps: ({| +foo: number |}) => number = ObjectBS.useProps;
