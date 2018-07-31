# features

## adjust\_labels (called in mso2dfa)
- label="01X.." -> label="!0 1 .."
- add "accepting" / "rejecting" attributes to "state" elements

## subst\_labels (**deprecated)

## decode\_events (spec2dfa)  
- recover event names

## elim\_rejecting (dfa2scxml)  
- eliminate sink nodes with "rejecting=true"

## merge\_transitions (dfa2dot)

## add\_rules (rules2dfa)  
- add eca rules (<rules>...</rules>) into dfa.xml

## assoc\_eca (dfa2scxml)
- add conditions and actions to transitions with events

