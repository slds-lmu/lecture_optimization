
todos aus dem "dive into DL buch". 



mehr auf die unterschiede zwischen train- und testfehler eigehen? und was das für optim (auf ML) bedeutet?

12.3.1. One-Dimensional Gradient Descent: hat ein winziges gutes argument dass GD runterget wenn man tylor ansieht



12.3.3.1. Newton’s Method
Now let’s consider a nonconvex function, such as for some constant 
After all, note that in Newton’s method we end up dividing by the Hessian. This means that if the second derivative is negative we may walk into the direction of increasing the value of f. 
That is a fatal flaw of the algorithm. ---> <EXAMPLE>

zeigt auch dass man durch LR ein besseres verhalrten haben kann. gibt es dazu auch theorie?


12.3.3.2. Convergence Analysis --> für Newton



12.3.3.3. Preconditioning --> kurz erwähenn, 1 slide


bei SGD: besser sagen warum LR anpassung so wichtig ist / schwierig ist
(steht allerdings auch nicht im lipton)


vielleicht die SGD sachen nach 2nd order? würde erlauben bei sgd++ mehr drauf einzugehen dass erweiterungen davon 2nd order irgendwie approximiert
wäre auch halbwegs in historischer reigenfolge?

von DR: 
Die adaptive Step Size Methods wie Adagrad können auch als 2nd order Approximations mit diagonal preconditioning gesehen werden. In Dive2DL wird das ein bisschen so motiviert. Ich finde, dass das ein wichtiger Zusammenhang ist, aber da wir bisher ja Newtons Method etc. noch nicht eingeführt haben würde ich vorschlagen, dass wir im Second order kapitel ein kurzes deck dazu machen?
Timo hat mich darauf aufmerksam gemacht, dass 1_GD schon eine Slide zu Backprop ist. Ich würde die dann einfach in das neue Autodiff Deck einarbeiten? Außerdem finde ich eig., dass man den Autodiff Teil auch zu 12_in_practice hinzufügen könnte. Aber das können wir ja dann noch entscheiden.
bernd_bischl
:
11:29
2 min zoom?



12.4.2. Dynamic Learning Rate
A popular choice is polynomial decay. In the case of convex optimization there are a number of proofs that show that this rate is well behaved.
--> aha. wo?

haben aber 1-2 nette bilder für den vergleich zwischen exp und poly decay


---> ansehen
 since in general minimizing nonlinear nonconvex problems is NP hard. For a survey see e.g., the excellent lecture notes of Tibshirani 2015.



SGD: 12.4.3. Convergence Analysis for Convex Objectives
--> nehmen


Prove that for the function 
 adding normal noise to the gradient is equivalent to minimizing a loss function  where 
 is drawn from a normal distribution
--> gute aufgabe


 proximal gradient / ISTA --> anschauen ob wir dafür noch platz haben


move learn rate schedules from GD to SGD part
Schedules (polynomial/exponential decay) are an SGD topic, not a deterministic one. In deterministic GD the true gradient vanishes at the optimum and a line search is available, 
so decaying schedules are essentially never used. They exist precisely because SGD can't do a line search (noisy gradients) and needs decay for convergence.
I'd say this explicitly so students don't conflate the two regimes — that's probably the single most clarifying sentence in this part of the lecture.

Exact line search: keep it, but as a teaching device, not a recommendation. Two payoffs: on a quadratic it has a closed form (shows the principle cleanly), and it demonstrates that even perfect step choice doesn't fix zigzagging — consecutive gradients come out orthogonal and the rate is still ((κ−1)/(κ+1))². That's a direct callback to your conditioning category and the strongest possible argument that the step size isn't the bottleneck.


To your direct question — does anyone do anything else besides backtracking? Yes, two things:

Wolfe / strong-Wolfe line search with polynomial interpolation (e.g. Moré–Thuente). This is the actual standard in serious solvers, not plain Armijo halving. Two reasons worth a conceptual slide: interpolation finds an acceptable step in very few function evaluations, and the curvature condition (the second Wolfe condition) is what plain Armijo lacks. That matters because standalone GD is rare — the same line search feeds nonlinear CG and L-BFGS, where the curvature condition is what keeps the quasi-Newton Hessian approximation positive-definite. So Wolfe is the natural bridge to your later lectures.
Barzilai–Borwein (spectral) step size. A step computed from the last two iterates/gradients (a quasi-Newton-flavored quotient), with little or no line search — just a nonmonotone safeguard. It accelerates plain GD dramatically and is genuinely used. Elegant one-slide item: "cheap second-order-ish information without a Hessian."

My recommendation on depth:

Real derivation: Armijo (have it) + Wolfe conditions, at least conceptually (state the curvature condition, say why, point forward to L-BFGS).
One slide each: constant-1/L-as-the-principled-baseline; Barzilai–Borwein as the "fast plain GD" trick.
Explicitly label exact line search + schedules as motivating failures. That arc — constant → "but which?" → exact → "too expensive and doesn't even fix zigzag" → Armijo "we only need sufficient decrease" — is good pedagogy. It's not an empty enumeration as long as you name it as motivation and land on Wolfe+interpolation as the thing serious code actually does.


Isn't it a little bit unrealistic to assume that we know that Lipschitz constant in practice?

5:04 PM
Yes — for a generic objective you essentially never know L, and even when a bound exists it's usually a loose global overestimate, so 1/L would be far too conservative. So you're right to push on this.

But two qualifications keep it from being a useless idea:

1. There's a class of problems where L is genuinely known/cheap. This is the deterministic ML bread-and-butter:

Least squares / ridge: the smooth part has Hessian XᵀX (plus λI), so L = largest eigenvalue of XᵀX, computable once via a few power-iteration steps.
Logistic regression: L ≤ ¼‖X‖² (from the ≤¼ bound on the logistic second derivative), a closed-form upper bound.
For exactly these problems — which are also where proximal gradient / ISTA lives — constant 1/L is not a fiction, it's standard. That's the honest scope of the claim.

2. When you don't know L, you estimate it locally — and the tool is backtracking again. This is the unifying point worth making to students: backtracking on the Armijo condition is exactly a procedure for discovering a usable local L on the fly. Each rejected step says "my current 1/L guess is too big," and you shrink. You can even make this explicit with a backtracking step that also grows the step when accepted (try to increase α between iterations, shrink on rejection) — that's an adaptive local-L estimate, and it's what good ISTA/FISTA implementations actually do.

So the resolution for the lecture: constant 1/L is the clean theoretical baseline and the practical choice for a specific well-understood problem class; for everything else, backtracking is the mechanism that replaces the unknown L with a cheap local estimate. That closes the loop back to your earlier instinct — backtracking really is the workhorse — and it gives the theory a concrete reason to exist rather than leaving 1/L as an unrealistic assumption.

So I'd present 1/L not as "assume you know L" but as "if you know L (and for ridge/logistic/lasso you do), here's the principled step; otherwise backtracking estimates it." That framing is both honest and tidy.

---------------------

mehr zu momentum bei SGD
So momentum directly addresses your "first-order limitation / slow rate" category too — it's how a first-order method claws back toward second-order-like behavior without a Hessian.
The one caveat to state explicitly: momentum's role and analysis change under noise. In SGD, the story becomes variance smoothing / averaging, the constants and guarantees differ, and "momentum" in deep-learning practice (the β≈0.9 you'll meet later) is doing something related but not identical. So introduce it deterministically with the clean heavy-ball/Nesterov rate story, then when you reach SGD, revisit it as "same update, different justification."
That sequencing is actually a feature: students see the idea in the setting where it provably works, then see how it transfers and what gets murkier.
One honesty note: the heavy-ball rate is rigorous specifically on quadratics (and locally); globally for general smooth convex functions Nesterov's method is the one with the clean guarantee, and plain heavy-ball can even fail to converge on some non-quadratic examples. Worth a footnote so you're not overclaiming the heavy-ball rate in full generality.

noise smoothing, sgteht auch im zhang-DDL, erhöht effective sample size

---------------------

12.5.2. Minibatches
n practice we pick a minibatch that is large enough to offer good computational efficiency while still fitting into the memory of a GPU. To illustrate the savings let’s have a look at some code. In it we perform the same matrix-matrix multiplication, but this time broken up into “minibatches” of 64 columns at a time.

pytorch
mxnet
tensorflow
timer.start()
for j in range(0, 256, 64):
    A[:, j:j+64] = torch.mm(B, C[:, j:j+64])
timer.stop()
print(f'performance in Gigaflops: block {0.03 / timer.times[3]:.3f}')
Copy to clipboard
performance in Gigaflops: block 37.640
As we can see, the computation on the minibatch is essentially as efficient as on the full matrix. A word of caution is in order. In Section 8.5 we used a type of regularization that was heavily dependent on the amount of variance in a minibatch. As we increase the latter, the variance decreases and with it the benefit of the noise-injection due to batch normalization. See e.g., Ioffe (2017) for details on how to rescale and compute the appropriate terms.


---------------------

https://distill.pub/2017/momentum/

---------------------

zum momemtum bei ill-Conditioning haben wir eigentlich ein ganz gutes Bild, aber hier in dem Deep Dive, in dem Dive into Deep Learning Buch, gibt es noch ein kleines bisschen mehr.
By construction, the gradient in the 
 direction is much higher and changes much more rapidly than in the horizontal 
 direction. Thus we are stuck between two undesirable choices: if we pick a small learning rate we ensure that the solution does not diverge in the 
 direction but we are saddled with slow convergence in the 
 direction. Conversely, with a large learning rate we progress rapidly in the 
 direction but diverge in 
. The example below illustrates what happens even after a slight increase in learning rate from 
 to 
. Convergence in the 
 direction improves but the overall solution quality is much worse.


---------------------


However, this is not really what we did. In the toy examples in the current section we simply added noise to an otherwise non-stochastic gradient, i.e., we pretended to have pairs 
. It turns out that this is justified here (see the exercises for a detailed discussion). More troubling is that in all previous discussions we clearly did not do this. Instead we iterated over all instances exactly once. To see why this is preferable consider the converse, namely that we are sampling 
 observations from the discrete distribution with replacement. The probability of choosing an element 
 at random is 
. Thus to choose it at least once is
(12.4.18)
A similar reasoning shows that the probability of picking some sample (i.e., training example) exactly once is given by
(12.4.19)¶
Sampling with replacement leads to an increased variance and decreased data efficiency relative to sampling without replacement. Hence, in practice we perform the latter (and this is the default choice throughout this book). Last note that repeated passes through the training dataset traverse it in a different random order.
--> kurz sagen


minibatches:
At the heart of the decision to use minibatches is computational efficiency. This is most easily understood when considering parallelization to multiple GPUs and multiple servers. In this case we need to send at least one image to each GPU. With 8 GPUs per server and 16 servers we already arrive at a minibatch size no smaller than 128.



12.5.1. Vectorization and Caches
--> 1 slide






Weaknesses of GD:
1. Non-convexity / multimodality (landscape has multiple critical points)
Local minima
Saddle points
Sensitivity to initialization (which basin you land in)
Converges only to stationary points (not necessarily a minimum)

2. Conditioning / geometry of the landscape (shape distorts the gradient direction)

Ill-conditioning (high Hessian condition number)
Oscillation/zigzagging in narrow ravines
Sensitivity to feature/variable scaling

3. Vanishing gradient signal (gradient magnitude gives poor guidance)
Plateaus / flat regions
Slow crawl near saddle points (overlaps with category 1 — saddles cause both wrong-direction and weak-signal problems)
--> missing, we might also put this into a DL specific deck
