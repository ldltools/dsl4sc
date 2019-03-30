# bounded buffer

A buffer that can carry up to 3 items.
It works like a stack.

- put (item): add an item to the buffer
- get (item): get the most-recently added item (without keeping it in the buffer)


<details>
  <summary>bounded buffer (capacity of 3)</summary>
  <div><img alt="statechart" src="bounded_buffer.svg?sanitize=true"/></div>
</details>


## [bounded\_buffer](bounded_buffer.rules)

### protocol

```
protocol  
(put + get);;
```

### variable _n_ & its initial value

```
variable  
n : nat (4);  // num of items in the buffer. 0 <= n <= 3  
property  
n = 0;  // initially set to 0, indicating the buffer is empty.  
```

### rules

```
rule  
on put when n = 0 ensure n = 1;  
on put when n = 1 ensure n = 2;  
on put when n = 2 ensure n = 3;  
on put when n = 3 ensure false;  
on get when n = 0 ensure false;  
on get when n = 1 ensure n = 0;  
on get when n = 2 ensure n = 1;  
on get when n = 3 ensure n = 2;  
```

Remark  
dsl4sc itself provides no means to define the above rules more compactly.  
One way to mitigate this inconvenience is to use a preprocessor such as cpp
to generate these rules automatically.


## Formal verification

### claim: "put; put; put" is acceptable, but "put; put; put; put" is not

### claim: buffer never carries 4 items

### claim: if the capacity is restricted to 2 or less, buffer does not accept "put; put; put"

## Statechart generation and its execution

```
$ rules2scxml bounded_buffer.rules -o bounded_buffer.scxml
```


```
$ shelltest bounded_buffer.conf
```
