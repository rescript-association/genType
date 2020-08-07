/* TypeScript file generated from Opaque.re by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const OpaqueBS = require('./Opaque.bs');

import {business as Records_business} from './Records.gen';

// tslint:disable-next-line:max-classes-per-file 
// tslint:disable-next-line:class-name
export abstract class opaqueFromRecords { protected opaque!: any }; /* simulate opaque types */

// tslint:disable-next-line:interface-over-type-literal
export type pair = [opaqueFromRecords, opaqueFromRecords];

export const noConversion: (x:opaqueFromRecords) => opaqueFromRecords = OpaqueBS.noConversion;

export const testConvertNestedRecordFromOtherFile: (x:Records_business) => Records_business = OpaqueBS.testConvertNestedRecordFromOtherFile;
