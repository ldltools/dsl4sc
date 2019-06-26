# composite transaction

For atomicity of a composite transaction (tx1; tx2),
only one of the following 2 cases can occur in the end

- both tx1 and tx2 succeed
- neither succeeds (rollbacked)
