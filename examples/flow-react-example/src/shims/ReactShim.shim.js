/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @flow strict
 * @nolint
 */

// The following FlowExpectedErrors are safe - they are kind of like abstract
// external types. They may be replaced with a .flow.js interface file.

// $FlowExpectedError: Reason checked type sufficiently.
export type reactElement = React$Node;

// $FlowExpectedError: Reason checked type sufficiently.
export opaque type component = any;

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
