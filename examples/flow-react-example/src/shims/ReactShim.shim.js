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

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type componentSpec = any;

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type noRetainedProps = any;

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type actionless = any;

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type stateless = any;

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type reactRef = any;

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type ref = any;
