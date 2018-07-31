# [ping pong](ping\_pong.rules)

```
protocol
 (ping; pong)*;;
rule
 on ping do raise pong { console.log ("ping"); };
 on pong do { console.log ("pong"); };
```

