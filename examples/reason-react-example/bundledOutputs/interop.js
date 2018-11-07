/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./src/interop/InteropRoot.js");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./src/interop/InteropRoot.js":
/*!************************************!*\
  !*** ./src/interop/InteropRoot.js ***!
  \************************************/
/*! no exports provided */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony import */ var _WrapJsValue_re__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./WrapJsValue.re */ \"./src/interop/WrapJsValue.re.js\");\n\nconsole.log(\"interopRoot.js roundedNumber:\", _WrapJsValue_re__WEBPACK_IMPORTED_MODULE_0__[\"roundedNumber\"]);\n\n//# sourceURL=webpack:///./src/interop/InteropRoot.js?");

/***/ }),

/***/ "./src/interop/MyMath.js":
/*!*******************************!*\
  !*** ./src/interop/MyMath.js ***!
  \*******************************/
/*! exports provided: round */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, \"round\", function() { return round; });\nvar round = Math.round;\n\n//# sourceURL=webpack:///./src/interop/MyMath.js?");

/***/ }),

/***/ "./src/interop/WrapJsValue.bs.js":
/*!***************************************!*\
  !*** ./src/interop/WrapJsValue.bs.js ***!
  \***************************************/
/*! exports provided: roundedNumber */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, \"roundedNumber\", function() { return roundedNumber; });\n/* harmony import */ var _WrapJsValue_re__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./WrapJsValue.re */ \"./src/interop/WrapJsValue.re.js\");\n\nvar roundedNumber = _WrapJsValue_re__WEBPACK_IMPORTED_MODULE_0__[\"round\"](1.8);\n\n//# sourceURL=webpack:///./src/interop/WrapJsValue.bs.js?");

/***/ }),

/***/ "./src/interop/WrapJsValue.re.js":
/*!***************************************!*\
  !*** ./src/interop/WrapJsValue.re.js ***!
  \***************************************/
/*! exports provided: round, roundedNumber */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, \"round\", function() { return round; });\n/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, \"roundedNumber\", function() { return roundedNumber; });\n/* harmony import */ var _MyMath__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./MyMath */ \"./src/interop/MyMath.js\");\n/* harmony import */ var _WrapJsValue_bs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./WrapJsValue.bs */ \"./src/interop/WrapJsValue.bs.js\");\n // Export 'round' early to allow circular import from the '.bs.js' file.\n\nvar round = _MyMath__WEBPACK_IMPORTED_MODULE_0__[\"round\"];\n //const WrapJsValueBS = require('./WrapJsValue.bs');\n\nvar roundedNumber = _WrapJsValue_bs__WEBPACK_IMPORTED_MODULE_1__[\"roundedNumber\"];\n\n//# sourceURL=webpack:///./src/interop/WrapJsValue.re.js?");

/***/ })

/******/ });