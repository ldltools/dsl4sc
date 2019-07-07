// import * as pp from './out/ping_pong_guarded'
// import { pint, pong } from './out/ping_pong_guarded'
// ** jest (v24.8) does not comply with ESM, fall back to CommonJS
const pp = require ('./out/ping_pong_guarded')

test ('test01', () => {
    pp._reset ();
    expect ((() => { pp.ping (); pp.pong (); pp.quit (); })()).toBeUndefined ();
})

test ('test02', () => {
    pp._reset ();
    expect ((() => {
	try { pp.ping (); pp.ping (); pp.quit (); } catch (e) { return false; }
    })()).toBe (false);
})

test ('test03', () => {
    pp._reset ();
    expect ((() => {
	try { pp.ping (); pp.pong (); pp.ping (); pp.quit (); } catch (e) { return false; }
    })()).toBe (false);
})
