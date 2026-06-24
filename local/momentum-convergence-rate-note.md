# Suspected Error: Convergence Rate in the Complex Eigenvalue Case

## Setup

The per-component momentum recursion in eigenspace is governed by the matrix

$$R = \begin{pmatrix} \varphi & \alpha\lambda_i \\ -\varphi & 1 - \alpha\lambda_i \end{pmatrix}$$

Its characteristic polynomial gives eigenvalues

$$\sigma_{1,2} = \frac{(1 + \varphi - \alpha\lambda_i) \pm \sqrt{(1 + \varphi - \alpha\lambda_i)^2 - 4\varphi}}{2}$$

## The claim in the slides (and the Distill blogpost)

When the discriminant is negative (complex eigenvalue case), the convergence rate reduces to $2\sqrt{\varphi}$.

## Why this looks wrong

Two basic facts about $2\times 2$ matrices:

1. **$\det(R) = \varphi$** (exact, by direct computation):
$$\det(R) = \varphi(1 - \alpha\lambda_i) - (-\varphi)(\alpha\lambda_i) = \varphi - \varphi\alpha\lambda_i + \varphi\alpha\lambda_i = \varphi$$

2. **For complex conjugate eigenvalue pairs**, $\sigma_2 = \bar\sigma_1$, so:
$$|\sigma_1|^2 = \sigma_1 \cdot \bar\sigma_1 = \sigma_1 \cdot \sigma_2 = \det(R) = \varphi$$

Therefore $|\sigma| = \sqrt{\varphi}$, not $2\sqrt{\varphi}$.

## Where the factor of 2 might come from

Possibly a confusion with the formula for the spectral radius in terms of the trace. In the complex case $|\sigma|^2 = \det(R)$ is exact and the trace is irrelevant. Alternatively, if one mistakenly used $|\sigma| = \frac{1}{2}|\text{tr}(R)|$ (valid only for real equal eigenvalues, not complex ones), and then substituted, a spurious factor could appear.

## Status

This is a suspected error, not a confirmed one. The same expression appears in the Distill blogpost, so either both sources share the same mistake, or there is a different definition of "convergence rate" or a different normalization of $R$ being used that we have not identified.
