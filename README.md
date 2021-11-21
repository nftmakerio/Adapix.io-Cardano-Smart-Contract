# Adapix contracts
The smart contracts powering adapix.io

## How it works

### Deployed continously
This contract is deployed on every _operation_. What we call _operation_ is the entire process of a sale (listing -> editing price -> buying -> done).
This is different than other contract architecture patterns, slightly more in an ethereum-like style, where a single big contract handles all the logic. There 
are some big benefits of doing this:

- If there is a vulnerability on the contract, it can be fixed in future contracts and does not spread
- Can help with some contention issues
- Easier to scale, as it does so horizontally
- No need to keep track of other trasactions
- Sometimes easier to debug

This architecture only makes sense when there is no shared state or logic among the operations. Unlike SpaceBudz, where the contract needs to know which
bud is being sold/bought, this contract only cares about the operation in question. That said, there are some downsides to the continous deployment model

- It's harder for users to audit: This can be fixed by calling a blockchain explorer API with the bytecode every time a contract is created and deployed.
- No shared logic: It is impossible to share logic between contracts (unless some kind of oracle is involved) as they have different datums and states.
- Complex in the deployment: A whole system needs to be put in place to continously build and deploy these contracts

### Note
We excluded the tests in the repo, since this is what it is actually deployed on the server
