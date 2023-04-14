
Ideas:
1. User specifies syntax and the function modifies the JAGs file
	* advantage: the order of variable versus latent doesn't matter (more flexible)
2. Use specifies placeholders in lavaan syntax
3. User creates lavaan syntax, function finds nonlienar elements, sends rest to blavaan
	* problem: lavaan uses `latent =~ variable` syntax, but we'd need `variable ~ latent` (usually)