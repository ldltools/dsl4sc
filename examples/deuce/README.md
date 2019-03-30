# deuce

![statechart](deuce.svg)

## [deuce](deuce.dsl)

### protocol

```
protocol  
(sharapova + williams)*; game ;;
```

### state variable and its initial condition

```
variable  
state : nat(4); // nat(4) = {0, 1, 2, 3}  
  // 0: deuce  
  // 1: advantage with sharapova  
  // 2: advantage with williams  
  // 3: game set  
property  
state = 0;
```

### rules


## Formal verification
