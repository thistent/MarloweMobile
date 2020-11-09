# Marlowe Mobile
A way to view and edit Marlowe smart contracts for mobile devices.

Work in progress...

See the current implementation [here](https://thistent.github.io/MarloweMobile/).

## Things to figure out:
* Finish code to transform the AST into an alternative form with unique identifiers so that subsections of the tree can be selected and replaced.
* Get the keyboard working so that it displays possible sub-nodes of the tree for replacement.
* Switch from `Int` to `BigInt`.
* The current text-based AST in the Marlowe Playground doesn't directly represent the AST in the PureScript code, such as the representation of different token types. 


Eventually this should work as a drop-in replacement for *Blockly in the Marlowe Playground, or maybe even an alternative to the playground altogether.

*Blockly* poorly utilizes horizontal space, which makes it difficult to work with on a mobile devices.
As my goal is to teach programming in Malawi, and due to the fact that many more people have access to smart phones than full-sized computers,
it's important to me that I have tools that give students the ability to learn and work with the technology that they already have.

The text-based AST on Marlowe Playground is actually valid Elm code, due to Elm's close relationship with Haskell.
This made it very easy to drop in the sample contracts from the playground under Contract definitions in Elm.

One of the goals here is to make syntactically imposible states irrepresentable..
I'd also like to work on checking semantic correctness, but that's a much more ambitious goal.

Work here should be repurposeable for visualizing Plutus contracts, lambda calculus, and combinatory logic,
which I also want to teach as a bottom-up approach along with working from the top down, with languages like Elm.
