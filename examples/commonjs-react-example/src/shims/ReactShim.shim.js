/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @flow strict
 * @nolint
 */

import * as React from 'react';

export type reactElement = React.Node;
export type element = React.Node;

export type component<t> = React.ComponentType<t>;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently.
export opaque type componentSpec = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently.
export opaque type noRetainedProps = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently.
export opaque type actionless = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently.
export opaque type stateless = any;

// $FlowExpectedError[untyped-import]: Reason checked type sufficiently.
export opaque type reactRef = any;
