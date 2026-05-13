# AdamsPokeR

## Project Definition

AdamsPokeR is a bounded-complexity poker simulation framework written in R for testing simplified, human-usable strategies in Texas Hold'em poker. 

## The Reason
One of the first things I teach my friends to do so they can play more interesting poker is "fold more often, and especially at the preflop because its cheaper". 
The usual response is "how would I know which hole cards to fold on? If I don't see the flop, I might miss out on good hands." I tell them to add the values of their cards together (on a 2-14 scale), add 5 if they are suited or add 10 if they are a pair. If the result is anything above 30, raise and call if someone raises back. Anything between 25-30, raise if no-one else has, but call if someone else already has. There are rules for 20-25, 15-20, and <15 too. 

People don't usually have the time to memorize every hole card situation and how strong it is, so this means they only need to remember the equation and the numbers to compare it to, and then the decision is made for them. No more "what if I get quads on the board" or other nonsense, just simple and objective decision-making. 

But honestly, I didn't know how effective that one rule was, so I started building AdamsPokeR in 2022 to see if it significantly improved play versus going to flop on every hand. Turns out, it made a huge difference. So then I started thinking "what if I came up with rules for flop, turn, and river as well?" 

And here we are. 

## Goals

The goal of the project is NOT to:

* create a poker bot,
* automate poker play,
* solve poker mathematically,
* reproduce game-theory-optimal (GTO) play,
* or build an AI capable of superhuman decision-making.

Instead, the purpose of AdamsPokeR is to:

* simulate realistic-enough poker environments,
* apply deterministic human-readable strategy systems,
* compare long-run outcomes across large simulation samples,
* and identify whether relatively simple heuristic systems can consistently outperform weaker strategies.

The project is designed around the idea that most human players cannot realistically:

* memorize solver outputs,
* compute pot equity in real time,
* perform recursive game-tree analysis,
* or execute mixed-frequency GTO strategies.

Therefore, AdamsPokeR focuses specifically on simplified strategic systems that:

* humans can remember,
* humans can apply under pressure,
* and humans can realistically use at a poker table.

Examples of these simplified systems include:

* hand-strength thresholds,
* position-aware decisions,
* simple stack-to-pot heuristics,
* draw continuation rules,
* aggression/passivity modifiers,
* and simplified betting equations.

The project philosophy is much closer to:

* behavioral simulation,
* bounded rationality,
* and practical decision science,
  than to poker AI research.

---

# Core Project Principles

## 1. The Engine Must NOT Contain Hidden Strategy Logic

The simulation engine itself should:

* manage state,
* manage stacks,
* manage pots,
* manage betting legality,
* manage action order,
* and resolve outcomes.

The engine should NOT:

* secretly optimize player behavior,
* secretly calculate ideal play,
* secretly compute EV-based actions,
* or contain hidden strategic assumptions.

All player behavior must remain externally visible and interpretable through decision-table systems.

---

## 2. Decision Tables Control Player Behavior

Player behavior should be controlled through deterministic decision-table objects (I call them dt_ objects).

Decision tables may contain:

* hand-strength thresholds,
* position logic,
* stack logic,
* draw logic,
* pot-size logic,
* aggression rules,
* and player-type modifiers.

Decision tables should remain:

* readable,
* editable,
* interpretable,
* and understandable by humans.

The project intentionally avoids opaque black-box strategy systems.

---

## 3. Environmental Realism Matters More Than Strategic Complexity

The poker environment must behave realistically enough that strategy comparisons are meaningful.

This includes:

* realistic betting flow,
* realistic stack accounting,
* realistic pot accounting,
* realistic positional order,
* and realistic betting-round progression.

However, the project does NOT require:

* full casino-grade realism,
* infinite betting trees,
* recursive solver behavior,
* or mathematically solved poker.

Bounded realism is preferred over excessive complexity.

---

## 4. The System Should Remain Computationally Manageable

The project intentionally limits complexity to preserve:

* interpretability,
* reproducibility,
* simulation speed,
* and development stability.

Current intended constraints include:

* maximum 6 players,
* standardized 100-unit stacks,
* maximum 2 betting cycles per street,
* deterministic action systems,
* no infinite betting loops,
* and simplified all-in handling.

These constraints are intentional design decisions, not shortcomings.

---

## 5. AdamsPokeR Is a Research and Learning Tool

The project is intended to:

* improve understanding of poker strategy,
* test simplified heuristics,
* explore long-run strategic outcomes,
* and reduce emotional or impulsive decision-making.

The project is NOT intended for:

* automated online poker play,
* circumventing poker platform rules,
* real-time assistance,
* or exploitative botting behavior.

---

# Current Architecture

## Existing Systems

The current project (timestamp = May 11th 2026) already includes:

* deck generation,
* deck shuffling,
* card dealing,
* poker-hand evaluation,
* tiebreaker logic,
* showdown winner resolution,
* simulation-block generation,
* positional assignment,
* player-type assignment,
* contextual feature generation,
* deterministic decision-table application,
* stack accounting,
* pot accounting,
* and simulation testing infrastructure.

The project also currently supports:

* wildcard decision-table matching using NA values,
* reproducible simulations,
* and large-scale simulation blocks.

---

# Current Development Priorities

The current priority is improving:

* betting-round realism,
* legal-action flow,
* amount-to-call logic,
* per-street investment tracking,
* and limited reopen-action handling.

The current project does NOT yet attempt:

* advanced bluff systems,
* psychological modeling,
* solver logic,
* equilibrium play,
* or machine learning.

---

# Planned Betting Structure

The intended betting system is intentionally simplified.

Per street:

## Cycle 1

All active players act sequentially.

If no raise occurs:

* the betting round closes.

If a raise occurs:

* remaining active players enter Cycle 2.

## Cycle 2

Players may:

* fold,
* call,
* raise,
* or move all-in.

After Cycle 2:

* the street closes automatically.

This structure preserves:

* positional dynamics,
* aggression response,
* and strategic pressure,
  while avoiding:
* infinite recursive betting trees,
* solver-level complexity,
* and excessive engine bloat.

---

# Intended Long-Term Outcome

The long-term goal of AdamsPokeR is to identify whether relatively simple and human-usable strategic systems can:

* improve long-run poker performance,
* reduce impulsive or emotional play,
* simplify decision-making under uncertainty,
* and outperform weaker heuristic systems over large sample sizes.

The ideal output of the project is not to determine a form of 'perfect poker play', but look for practical simplified systems that people can realistically remember and apply in their friendly games. I would imagine (and I would hope) that no-one would attempt to play professionally based on simplified systems, which we can define as any decision system with clear and predictable criteria which might leave them exploitable to professional-level play. Pro players spend cumulative days and weeks learning solvers, GTO play, range construction... the list goes on. 

---

# Development Philosophy

When troubleshooting or expanding AdamsPokeR:

* avoid unnecessary complexity,
* avoid hidden strategy assumptions,
* avoid drifting toward solver architecture,
* avoid premature optimization,
* and avoid features that do not directly support the core project goals.

New features should only be added if they:

* improve realism meaningfully,
* improve interpretability,
* improve strategic comparison validity,
* or directly support simplified human-usable strategy testing.

# DISCLAIMER

AdamsPokeR, TaggTechRnD, and Adam Taggart take NO responsibility for people making dumb decisions based on the use of this package or any of its functions. Indepedent, informed, and consenting adults of sound mind are the only people who should participate in poker play of any kind and we DO NOT condone or encourage people to gamble (which poker is still classed as a gambling sport) with real money. This package was developed to better understand the dynamics and strategy of the game, and is not liable in any way for actions taken by players using this package towards anything other than educational ends. 

This also means I can't claim a cut of anything you win, so relax. 
