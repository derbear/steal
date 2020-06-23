# SOV auction arithmetic analysis

## Parameters and bounds

All parameters below are unsigned 64-bit integers.

+---------------------------+---------------------------------------------------+
| $A \leq 10^9 * 10^2$      | Anchor divided by $10^4$                          |
+---------------------------+---------------------------------------------------+
| $S \leq 10^8$             | Total supply divided by $10^4$                    |
+---------------------------+---------------------------------------------------+
| $T \leq 10^3$             | Number of tranches                                |
+---------------------------+---------------------------------------------------+
| $L \leq 10^2$             | Lookback parameter                                |
+---------------------------+---------------------------------------------------+
| $t_0$                     | Size of initial $L$ tranches                      |
+---------------------------+---------------------------------------------------+
| $0 \leq c \leq 10^4$      | Supply ratio times $10^4$                         |
+---------------------------+---------------------------------------------------+
| $\sum r \leq 10^8 * 10^6$ | Total raised in the last $L$ tranches             |
+---------------------------+---------------------------------------------------+
| $\sum t \leq 10^7 * 10^4$ | Aggregate tranche size over the last $L$ tranches |
+---------------------------+---------------------------------------------------+
| $m$                       | Minimum tranche size                              |
+---------------------------+---------------------------------------------------+

## Tranche computation

If the index of the current tranche is less than $L$, return $t_0$.
If $\sum r = 0$ or $Sc \leq \sum t$, return $m$.
Otherwise, return the following:

$$
\frac{2 \sum r (Sc - \sum t)}{LAc + T \sum r}
$$

### Precision

In the numerator,

$$S \leq 10^8, c \leq 10^4 \implies Sc \leq 10^{12}$$ in units of supply;

$$\sum t \leq 10^7 * 10^4 \implies \sum t \leq 10^{11}$$ in units of supply;

this implies the difference is at most $10^{12}$.

$$\sum r \leq 2 * 10^8 * 10^6 \leq 10^{14}$$ in units of tether and

which implies the numerator is at most $2 * 10^{14} * 10^2 * 10^{12} \leq 10^{29}$.

In the denominator,

$$L \leq 10^2, A \leq 10^9 * 10^2, c \leq 10^4 \implies LAc \leq 10^{17}$$ units of tether, and

$$T \leq 10^3, \sum r \leq 10^8 * 10^6 = 10^{14} \implies T \sum r \leq 10^{17}$$ units of tether,

which implies the denominator is at most $10^{18}$.

If $n$ is the numerator and $d$ is the denominator, then there exists some
$q \leq \frac{10^{29}}{10^{17}} = 10^{12}, r \leq 10^{17}$ such that $qd + r = n$ with
$r < d$.

Since $2^{63} \leq 10^{18}$, both $q$ and $r$ must fit in 63-bit unsigned integers.

Division is not performed, so we are guaranteed no unnecessary loss in precision.

Note that $q$ is the tranche size.

## Payouts

Let $b_u$ be the bid amount for user $u$ in tether
and $\sum b$ be the total amount bid in the tranche,
with $t_i$ being the current tranche size.

Assume that $\sum b \leq \sum r \leq 10^{14}$.

Then $u$ receives a payout of $p_u = \frac{t_i b_u}{\sum b}$, or alternatively,

$$p_u \sum b + r = t_i b_u$$

where $r < \sum b \leq 10^{14}$ is a 63-bit integer.

Division is not performed, so we are guaranteed no unnecessary loss in precision.
