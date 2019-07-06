// -*-javascript-*-

//import * as t from '@babel/types';
import { strict as assert } from 'assert'

export function expects (exp)
{
    return function (target, key, descriptor) {
	const meth = descriptor.value;
	descriptor.value = function (...args) {
	    return meth.apply (this, args);
	}
	return descriptor
    }
}

export function ensures (exp)
{
    return function (target, key, descriptor) {
	const meth = descriptor.value;
	descriptor.value = function (...args) {
	    return meth.apply (this, args);
	}
	return descriptor
    }
}

export function update (exp)
{
    return function (target, key, descriptor) {
	const meth = descriptor.value;
	descriptor.value = function (...args) {
	    return meth.apply (this, args);
	}
	return descriptor
    }
}
