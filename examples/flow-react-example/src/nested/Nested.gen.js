/** 
 * @flow strict
 * @generated
 * @nolint
 */

// $FlowExpectedError: Reason checked type sufficiently
import * as NestedBS from './Nested.bs';

// flowlint-next-line nonstrict-import:off
import type {variant as Component2_variant} from '../../src/Component2.gen';

export type variant = "VariantA" | "VariantB" | "VariantC";

export const consumeVariant: (Component2_variant) => number = NestedBS.consumeVariant;
