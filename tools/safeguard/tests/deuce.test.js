const Deuce = require ("./out/deuce_guarded")
const d = new Deuce ()

test ('test01', () => {
    d._reset ();
    expect ((() => { d.sharapova (); d.sharapova (); d.game (); })()).toBeUndefined ();
})

test ('test02', () => {
    d._reset ();
    expect ((() => {
	try { d.sharapova (); d.williams (); d.game (); } catch (e) { return false; }
    })()).toBe (false);
})
