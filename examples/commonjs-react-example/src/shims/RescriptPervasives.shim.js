/**
 * @flow strict
 * @nolint
 */

export opaque type SafeReasonValueNotExportedToFlow = mixed;
'use strict';

export opaque type EmptyList = mixed;

export opaque type Cons<t> = mixed;

export type list<t> = | Cons<t> | EmptyList;