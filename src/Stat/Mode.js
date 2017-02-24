'use strict';

exports.toOctalStringImpl = function toOctalStringImpl(mode) {
	return ('0000' + (mode & 4095).toString(8)).slice(-4);
};
