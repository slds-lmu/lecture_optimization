


if __name__ == '__main__':
    # ------------------------------------------------------------------------------
    # multicrit
    #
    # DATA: explore the relationship between hotel price per night
    #       and customer ratings using Expedia data.
    # ------------------------------------------------------------------------------

    import numpy as np
    import pandas as pd
    import matplotlib.pyplot as plt
    import seaborn as sns
    import pyreadr

    sns.set_style("white")

    # ------------------------------------------------------------------------------
    # Load data (equivalent to readRDS)
    # ------------------------------------------------------------------------------
    result = pyreadr.read_r("expedia_example.rds")
    df = result[None]

    # ------------------------------------------------------------------------------
    # Figure 1
    # ------------------------------------------------------------------------------
    plt.figure(figsize=(4, 2))
    plt.scatter(df["mean_price"], -df["mean_rating"], s=9, c="black")  # ggplot size 1.5
    plt.ylim(-5.5, -2)
    plt.xlabel("Price per night")
    plt.ylabel("Rating")
    plt.tight_layout()
    plt.savefig("expedia-1-1.pdf")
    plt.close()

    # ------------------------------------------------------------------------------
    # Figure 2 (two highlighted points)
    # ------------------------------------------------------------------------------
    fig, axes = plt.subplots(1, 2, figsize=(6.5, 2))

    # p1
    axes[0].scatter(df["mean_price"], -df["mean_rating"], s=9, c="black")
    axes[0].scatter(
        df.iloc[15:17]["mean_price"],
        -df.iloc[15:17]["mean_rating"],
        s=12,  # ggplot size 2
        c=["green", "red"]
    )
    axes[0].set_ylim(-5.5, -2)
    axes[0].set_xlabel("Price per night")
    axes[0].set_ylabel("Rating")

    # p2
    axes[1].scatter(df["mean_price"], -df["mean_rating"], s=12, c="black")
    axes[1].scatter(
        df.iloc[[9, 15]]["mean_price"],
        -df.iloc[[9, 15]]["mean_rating"],
        s=12,
        c="orange"
    )
    axes[1].set_ylim(-5.5, -2)
    axes[1].set_xlabel("Price per night")
    axes[1].set_ylabel("Rating")

    plt.tight_layout()
    plt.savefig("expedia-2-1.pdf")
    plt.close()

    # Individual saves (p1)
    fig, ax = plt.subplots(figsize=(4, 2))
    ax.scatter(df["mean_price"], -df["mean_rating"], s=9, c="black")
    ax.scatter(
        df.iloc[15:17]["mean_price"],
        -df.iloc[15:17]["mean_rating"],
        s=12,
        c=["green", "red"]
    )
    ax.set_ylim(-5.5, -2)
    ax.set_xlabel("Price per night")
    ax.set_ylabel("Rating")
    plt.tight_layout()
    plt.savefig("expedia-3-1.pdf")
    plt.close()

    # Individual saves (p2)
    fig, ax = plt.subplots(figsize=(4, 2))
    ax.scatter(df["mean_price"], -df["mean_rating"], s=12, c="black")
    ax.scatter(
        df.iloc[[9, 15]]["mean_price"],
        -df.iloc[[9, 15]]["mean_rating"],
        s=12,
        c="orange"
    )
    ax.set_ylim(-5.5, -2)
    ax.set_xlabel("Price per night")
    ax.set_ylabel("Rating")
    plt.tight_layout()
    plt.savefig("expedia-4-1.pdf")
    plt.close()

    # ------------------------------------------------------------------------------
    # Pareto frontier computation
    # ------------------------------------------------------------------------------
    df["mean_rating"] = -df["mean_rating"]

    P = (
        df.sort_values(["mean_rating", "mean_price"])
        .loc[lambda x: ~x["mean_price"].cummin().duplicated()]
    )

    plt.figure(figsize=(4, 2))
    plt.scatter(df["mean_price"], df["mean_rating"], s=12, c="black")
    plt.scatter(P["mean_price"], P["mean_rating"], s=12, c="orange")
    plt.plot(P["mean_price"], P["mean_rating"], c="orange")
    plt.ylim(-5, -2)
    plt.xlabel("Price per night")
    plt.ylabel("Rating")
    plt.tight_layout()
    plt.savefig("expedia-5-1.pdf")
    plt.close()


    # ------------------------------------------------------------------------------
    # Single-objective functions
    # ------------------------------------------------------------------------------
    def fun(x):
        return (x - 1) ** 2


    x = np.linspace(0, 3, 500)

    plt.figure(figsize=(3, 3))
    plt.plot(x, fun(x), c="black")
    plt.scatter([1], [0], c="green", s=36)
    plt.xlabel("λ")
    plt.ylabel("c")
    plt.tight_layout()
    plt.savefig("expedia-6-1.pdf")
    plt.close()


    def fun1(x):
        return (x - 1) ** 2


    def fun2(x):
        return 3 * (x - 2) ** 2


    plt.figure(figsize=(3, 3))
    plt.plot(x, fun1(x), c="black")
    plt.plot(x, fun2(x), c="blue")
    plt.tight_layout()
    plt.savefig("expedia-7-1.pdf")
    plt.close()

    # ------------------------------------------------------------------------------
    # Pareto front in objective space
    # ------------------------------------------------------------------------------
    x = np.linspace(0, 3, 1000)
    xpareto = np.linspace(1, 2, 1000)

    plt.figure(figsize=(3, 3))
    plt.scatter(fun1(x), fun2(x), s=0.5, c="black")
    plt.scatter(fun1(xpareto), fun2(xpareto), s=0.5, c="green")
    plt.tight_layout()
    plt.savefig("expedia-8-1.pdf")
    plt.close()

    # ------------------------------------------------------------------------------
    # Weighted sum optimization
    # ------------------------------------------------------------------------------
    df["apriori"] = df["mean_price"] + 50 * df["mean_rating"]
    best = df.loc[df["apriori"].idxmin()]

    fig, axes = plt.subplots(1, 2, figsize=(4, 2))

    axes[0].scatter(df["apriori"], np.zeros(len(df)), s=12, c="black")
    axes[0].scatter(best["apriori"], 0, c="green", s=12)
    axes[0].set_xlabel("Weighted sum")
    axes[0].set_yticks([])

    axes[1].scatter(df["mean_price"], df["mean_rating"], s=12, c="black")
    axes[1].scatter(best["mean_price"], best["mean_rating"], s=12, c="green")
    axes[1].set_ylim(-5, -2)
    axes[1].set_xlabel("Price per night")
    axes[1].set_ylabel("Rating")

    plt.tight_layout()
    plt.savefig("expedia-9-1.pdf")
    plt.close()

    # ------------------------------------------------------------------------------
    # Multi-stage filtering
    # ------------------------------------------------------------------------------
    plt.figure(figsize=(6.5, 2))

    plt.subplot(1, 2, 1)
    plt.scatter(df["mean_price"], df["mean_rating"], s=12, c="black")
    plt.scatter(
        df.loc[df["mean_rating"] == -5, "mean_price"],
        df.loc[df["mean_rating"] == -5, "mean_rating"],
        s=12,
        c="orange"
    )
    plt.ylim(-5, -2)
    plt.title("1) max. rating")
    plt.xlabel("Price per night")
    plt.ylabel("Rating")

    plt.subplot(1, 2, 2)
    subset = df[(df["mean_rating"] == -5) & (df["mean_price"] < 150)]
    plt.scatter(df["mean_price"], df["mean_rating"], s=12, c="black")
    plt.scatter(subset["mean_price"], subset["mean_rating"], s=12, c="green")
    plt.ylim(-5, -2)
    plt.title("2) min. price")
    plt.xlabel("Price per night")

    plt.tight_layout()
    plt.savefig("expedia-10-1.pdf")
    plt.close()


    # ------------------------------------------------------------------------------
    # Contour plot (final figure)
    # ------------------------------------------------------------------------------
    def f1(x):
        return 0.01 * np.sum(x ** 2) - 2


    def f2(x):
        return 0.01 * np.sum(np.array([0.1, 0.3]) * (x - np.array([-10, 20])) ** 2)


    x1 = np.linspace(-10, 20, 100)
    x2 = np.linspace(-10, 20, 100)
    X1, X2 = np.meshgrid(x1, x2)

    Y1 = np.vectorize(lambda a, b: f1(np.array([a, b])))(X1, X2)
    Y2 = np.vectorize(lambda a, b: f2(np.array([a, b])))(X1, X2)

    plt.figure(figsize=(6, 6))
    plt.imshow(
        Y1,
        extent=[-20, 40, -20, 40],
        origin="lower",
        aspect="auto",
        cmap="Greys"
    )
    plt.contour(X1, X2, Y1, levels=15, colors="black")
    plt.contour(X1, X2, Y2, levels=15, colors="blue")
    plt.xlim(-20, 40)
    plt.ylim(-20, 40)
    plt.tight_layout()
    plt.savefig("expedia-13-1.pdf")
    plt.close()

    import numpy as np
    import matplotlib.pyplot as plt
    import pandas as pd

    # -------------------------------
    # Load data
    # -------------------------------
    import pyreadr

    result = pyreadr.read_r("expedia_example.rds")
    df = result[None]

    # -------------------------------
    # Prepare objectives for minimization
    # -------------------------------
    # Objective 1: price (minimize)
    df["f1"] = df["mean_price"]

    # Objective 2: negative rating (minimize)
    # Higher rating (-2 > -5) becomes smaller f2 = -mean_rating
    df["f2"] = -df["mean_rating"]

    # -------------------------------
    # Compute weakly and strongly efficient points
    # -------------------------------
    n = len(df)
    weakly_efficient = np.ones(n, dtype=bool)
    efficient = np.ones(n, dtype=bool)  # Pareto front / efficient points
    strictly_efficient = np.ones(n, dtype=bool)

    for i in range(n):
        for j in range(n):
            if i == j:
                continue

            # Weakly efficient: j strictly better in all objectives
            if (df.loc[j, "f1"] < df.loc[i, "f1"]) and (df.loc[j, "f2"] < df.loc[i, "f2"]):
                weakly_efficient[i] = False

            # Efficient / Pareto: j better or equal in all, strictly better in at least one
            if (df.loc[j, "f1"] <= df.loc[i, "f1"] and df.loc[j, "f2"] <= df.loc[i, "f2"]) and \
                    (df.loc[j, "f1"] < df.loc[i, "f1"] or df.loc[j, "f2"] < df.loc[i, "f2"]):
                efficient[i] = False

            # Strictly efficient: j better or equal in all objectives (equality allowed)
            if (df.loc[j, "f1"] <= df.loc[i, "f1"]) and (df.loc[j, "f2"] <= df.loc[i, "f2"]):
                strictly_efficient[i] = False

    df["weakly_efficient"] = weakly_efficient
    df["efficient"] = efficient
    df["strictly_efficient"] = strictly_efficient

    # -------------------------------
    # Plot all points
    # -------------------------------
    plt.figure(figsize=(6, 4))

    # All points
    plt.scatter(df["mean_price"], -df["mean_rating"], s=12, c="black", label="Other points")

    # Weakly efficient points
    plt.scatter(
        df.loc[df["weakly_efficient"], "mean_price"],
        -df.loc[df["weakly_efficient"], "mean_rating"],
        s=12,
        c="green",
        label="Weakly efficient"
    )

    # Strongly efficient points (Pareto front)
    plt.scatter(
        df.loc[df["efficient"], "mean_price"],
        -df.loc[df["efficient"], "mean_rating"],
        s=12,
        c="orange",
        label="efficient"
    )

    # Strongly efficient points (Pareto front)
    plt.scatter(
        df.loc[df["strictly_efficient"], "mean_price"],
        -df.loc[df["strictly_efficient"], "mean_rating"],
        s=12,
        c="orange",
        label="Strictly efficient"
    )


    plt.xlabel("Price per night")
    plt.ylabel("Rating")
    plt.title("(Weakly and strictly) Efficient Points")
    plt.gca().set_ylim(-5.5, -1.5)
    plt.gca().set_yticks([-5, -4, -3, -2])
    #plt.gca().invert_yaxis()  # Higher ratings (less negative) on top
    plt.legend()
    plt.tight_layout()
    plt.savefig("expedia_efficient.pdf")
    plt.close()



    # ------------------------------------------------------------------------------
    # Weighted sum scalarization
    # ------------------------------------------------------------------------------

    # Example Expedia
    def weighted_sum_scalarization(df, w1=1.0, w2=1.0, cols=("f1", "f2")):
        """
        Compute the weighted-sum scalarization for two objectives (both minimization).

        Inputs:
        - df: pandas DataFrame containing objective columns
        - w1, w2: scalar weights for objective 1 and 2
        - cols: tuple with column names (f1_col, f2_col)

        Returns: (best_idx, best_row, scores_series)
        """
        f1_col, f2_col = cols
        f1 = df[f1_col].astype(float).copy()
        f2 = df[f2_col].astype(float).copy()

        scores = w1 * f1 + w2 * f2
        idx = scores.idxmin()
        return idx, df.loc[idx], scores


    # python
    import numpy as np
    import matplotlib.pyplot as plt

    # weights to highlight
    weights = [(1.0, 20.0), (1.0, 50.0), (1.0, 100.0)]
    colors = ["green", "orange", "red"]

    # collect selected points
    pareto_ids = []
    for w1, w2 in weights:
        idx, best_row, _ = weighted_sum_scalarization(df, w1=w1, w2=w2, cols=("f1", "f2"))
        pareto_ids.append(idx)

    # Plot all points (use -mean_rating so higher ratings appear upward)
    x = df["mean_price"].astype(float)
    y = -df["mean_rating"].astype(float)

    plt.figure(figsize=(6, 4))
    plt.scatter(x, y, s=12, c="black", label="All points")

    # Strongly efficient points (Pareto front)
    plt.scatter(
        df.loc[df["efficient"], "mean_price"],
        -df.loc[df["efficient"], "mean_rating"],
        s=12,
        c="orange",
        label="Pareto-optimal points"
    )

    # Print 3 text-boxes for selected points
    for (w1, w2), idx in zip(weights, pareto_ids):
        best_row = df.loc[idx]
        plt.text(
            best_row["mean_price"] + 2,
            -best_row["mean_rating"] + 0.2,
            f"w=[{int(w1)},{int(w2)}]",
            color="green",
            fontsize=9,
            bbox=dict(facecolor="white", alpha=0.6, edgecolor="green")
        )

    plt.xlabel("Price per night",fontsize=16)
    plt.ylabel("Rating",fontsize=16)
    plt.title("Weighted sum scalarization",fontsize=16)
    plt.legend(loc="best")
    plt.gca().set_ylim(-5.5, -1.5)
    plt.gca().set_yticks([-5, -4, -3, -2])
    plt.tight_layout()
    plt.savefig("expedia_weighted_sum.pdf")
    plt.close()

    # Example synthetic functions
    # python
    import numpy as np
    import matplotlib.pyplot as plt

    # Define objectives (decision variable x)
    def fun1(x):
        return (x - 1) ** 2

    def fun2(x):
        return 3 * (x - 2) ** 2

    # Dense sampling of decision space
    x = np.linspace(0.0, 3.0, 301)
    f1 = fun1(x)
    f2 = fun2(x)

    # Compute Pareto (non-dominated) set
    n = len(x)
    dominated = np.zeros(n, dtype=bool)
    for i in range(n):
        for j in range(n):
            if j == i:
                continue
            # j dominates i if j is no worse in both and strictly better in at least one
            if (f1[j] <= f1[i] and f2[j] <= f2[i]) and (f1[j] < f1[i] or f2[j] < f2[i]):
                dominated[i] = True
                break
    pareto_mask = ~dominated
    pareto_indices = np.where(pareto_mask)[0]

    # Sweep weights for weighted-sum scalarization
    #weights = np.linspace(0, 1, 50_000)1
    weights = np.linspace(0, 1, 100)
    selected_indices = set()
    tol = 1e-12
    for w in weights:
        scores = w * f1 + (1.0 - w) * f2
        min_score = scores.min()
        ties = np.where(np.isclose(scores, min_score, atol=tol))[0]
        for idx in ties:
            selected_indices.add(int(idx))

    # Compare coverage
    pareto_set = set(int(i) for i in pareto_indices)
    found_on_weights = pareto_set & selected_indices
    missing_on_weights = pareto_set - selected_indices

    print(f"Total sample points: {n}")
    print(f"Pareto points found by dominance test: {len(pareto_set)}")
    print(f"Unique points selected by weighted-sum sweep: {len(selected_indices)}")
    print(f"Pareto points recovered by weighted-sum: {len(found_on_weights)}")
    print(f"Pareto points missed by weighted-sum: {len(missing_on_weights)}")
    if missing_on_weights:
        print("Example missing x values (up to 10):", np.round(x[list(missing_on_weights)][:10], 6))

    # Plot objective space and decision space diagnostics
    plt.figure(figsize=(10, 4))

    # Objective space (f1 vs f2)
    plt.subplot(1, 2, 2)
    plt.scatter(f1, f2, s=6, c='lightgray', label='All points')
    plt.scatter(f1[list(pareto_indices)], f2[list(pareto_indices)], s=10, c='orange', label='Pareto (non-dominated)')
    sel_idx_list = sorted(selected_indices)
    plt.scatter(f1[sel_idx_list], f2[sel_idx_list], s=10, c='green', label='Selected by weights', alpha=0.8)
    if missing_on_weights:
        miss_idx_list = sorted(missing_on_weights)
        plt.scatter(f1[miss_idx_list], f2[miss_idx_list], s=10, c='red', label='Pareto missed by weights')
    plt.xlabel("$f_1$",fontsize=16)
    plt.ylabel("f_2",fontsize=16)
    plt.title('Objective space', fontsize=16)
    plt.legend()

    # Decision space (x vs objectives)
    plt.subplot(1, 2, 1)
    plt.plot(x, f1, c='black', label="$f_1$")
    plt.plot(x, f2, c='blue', label="$f_2$")
    plt.scatter(x[list(pareto_indices)], f1[list(pareto_indices)], c='orange', s=10, marker='o', label='Pareto (f1 view)')
    # mark selected x positions from weights on top
    plt.scatter(x[sel_idx_list], f1[sel_idx_list], c='green', s=10, marker='x', label='Selected by weights')
    plt.xlabel('x',fontsize=16)
    plt.ylabel('objective value',fontsize=16)
    plt.title('Decision space ($f_1$ / $f_2$ curves)', fontsize=16)
    plt.legend()

    plt.tight_layout()
    plt.savefig("expedia-weighted-pareto_100.pdf")
    plt.close()


    # Sweep weights for weighted-sum scalarization
    #weights = np.linspace(0, 1, 50_000)1
    weights = np.linspace(0, 1, 300)
    selected_indices = set()
    tol = 1e-12
    for w in weights:
        scores = w * f1 + (1.0 - w) * f2
        min_score = scores.min()
        ties = np.where(np.isclose(scores, min_score, atol=tol))[0]
        for idx in ties:
            selected_indices.add(int(idx))

    # Compare coverage
    pareto_set = set(int(i) for i in pareto_indices)
    found_on_weights = pareto_set & selected_indices
    missing_on_weights = pareto_set - selected_indices

    print(f"Total sample points: {n}")
    print(f"Pareto points found by dominance test: {len(pareto_set)}")
    print(f"Unique points selected by weighted-sum sweep: {len(selected_indices)}")
    print(f"Pareto points recovered by weighted-sum: {len(found_on_weights)}")
    print(f"Pareto points missed by weighted-sum: {len(missing_on_weights)}")
    if missing_on_weights:
        print("Example missing x values (up to 10):", np.round(x[list(missing_on_weights)][:10], 6))

    # Plot objective space and decision space diagnostics
    plt.figure(figsize=(10, 4))

    # Objective space (f1 vs f2)
    plt.subplot(1, 2, 2)
    plt.scatter(f1, f2, s=6, c='lightgray', label='All points')
    plt.scatter(f1[list(pareto_indices)], f2[list(pareto_indices)], s=10, c='orange', label='Pareto (non-dominated)')
    sel_idx_list = sorted(selected_indices)
    plt.scatter(f1[sel_idx_list], f2[sel_idx_list], s=10, c='green', label='Selected by weights', alpha=0.8)
    if missing_on_weights:
        miss_idx_list = sorted(missing_on_weights)
        plt.scatter(f1[miss_idx_list], f2[miss_idx_list], s=10, c='red', label='Pareto missed by weights')
    plt.xlabel("$f_1$",fontsize=16)
    plt.ylabel("$f_2$",fontsize=16)
    plt.title('Objective space',fontsize=16)
    plt.legend()

    # Decision space (x vs objectives)
    plt.subplot(1, 2, 1)
    plt.plot(x, f1, c='black', label="$f_1$")
    plt.plot(x, f2, c='blue', label="$f_2$")
    plt.scatter(x[list(pareto_indices)], f1[list(pareto_indices)], c='orange', s=10, marker='o', label='Pareto (f1 view)')
    # mark selected x positions from weights on top
    plt.scatter(x[sel_idx_list], f1[sel_idx_list], c='green', s=10, marker='x', label='Selected by weights')
    plt.xlabel('x',fontsize=16)
    plt.ylabel('objective value',fontsize=16)
    plt.title('Decision space ($f_1$ / $f_2$ curves)', fontsize=16)
    plt.legend()

    plt.tight_layout()
    plt.savefig("expedia-weighted-pareto_300.pdf")
    plt.close()

    # Plot objective space and decision space diagnostics
    plt.figure(figsize=(6, 4))

    # Decision space (x vs objectives)
    plt.plot(x, f1, c='black', label="$f_1$")
    plt.plot(x, f2, c='blue', label="$f_2$")
    plt.scatter(x[list(pareto_indices)], f1[list(pareto_indices)], c='orange', s=10, marker='o',
                label='Pareto ($f_1$ view)')
    # mark selected x positions from weights on top
    plt.xlabel('x', fontsize=16)
    plt.ylabel('objective value', fontsize=16)
    plt.title('Decision space ($f_1$ / $f_2$ curves)', fontsize=16)
    plt.legend()

    plt.tight_layout()
    plt.savefig("expedia-weighted-pareto_raw.pdf")
    plt.close()

    import matplotlib.pyplot as plt
    import numpy as np

    # Create the figure
    fig, ax = plt.subplots(figsize=(8, 8))

    # 1. Define the unit arc: x1^2 + x2^2 = 1
    theta = np.linspace(0, np.pi / 2, 200)
    x_arc = np.cos(theta)
    y_arc = np.sin(theta)

    # 2. Generate an equidistant grid of points for region Y
    # Using a spacing of 0.1 for the structured dot look
    x_grid = np.arange(0, 1.51, 0.1)
    y_grid = np.arange(0, 1.51, 0.1)
    X, Y = np.meshgrid(x_grid, y_grid)

    # Filter: points satisfy x^2 + y^2 >= 0.98 and stay within bounds
    mask = (X ** 2 + Y ** 2 >= 1) & (X <= 1.5) & (Y <= 1.5)
    ax.scatter(X[mask], Y[mask], s=2, color='grey', marker='o')

    # 3. Plot the nondominated front (the arc)
    ax.plot(x_arc, y_arc, color='orange', linewidth=1.5)

    # 4. Plot the discrete points at the intercepts (y1 and y2)
    ax.scatter([1, 0], [0, 1], color='blue', s=60, zorder=10)

    # 5. Text annotations
    ax.text(0.73, -0.1, r'$f(\mathbf{x}^{(1)})$', fontsize=24,color="blue")
    ax.text(0.05, 0.85, r'$f(\mathbf{x}^{(2)})$', fontsize=24,color="blue")
    ax.text(0.6, 0.6, r'$\mathcal{F}$', fontsize=28,color="orange")
    ax.text(1.2, 1.2, r'$f(\mathcal{S})$', fontsize=28)

    # Define parameters
    w1, w2 = 1, 2  # Weights
    c1 = 3  # Constant value
    c2 = 2.5# Another constant value
    c3 = 1
    # 1. Define the range for x1
    x1 = np.linspace(-0.5, 2, 100)
    # 2. Calculate x2 based on the rearranged formula
    # x2 = (c - w1*x1) / w2
    x2_c1 = (c1 - w1 * x1) / w2
    x2_c2 = (c2 - w1 * x1) / w2
    x2_c3 = (c3 - w1 * x1) / w2
    # 4. Plot
    plt.plot(x1, x2_c1, linestyle='--',label="c1",color="blue")
    ax.text(0.6-0.15, (c1-w1*0.6)/w2-0.15, "$w_1 x_1 + w_2 x_2 = 3$", fontsize=20, rotation=-27, color="blue")
    plt.plot(x1, x2_c2, linestyle='--',label="c2",color="blue")
    ax.text(0.8-0.17, (c2-w1*0.8)/w2-0.17, "$w_1 x_1 + w_2 x_2 = 2.5$", fontsize=20, rotation=-27, color="blue")
    plt.plot(x1, x2_c3, linestyle='--',label="c2",color="blue")
    ax.text(0.3-0.27, (c3-w1*0.3)/w2-0.27, "$w_1 x_1 + w_2 x_2 = 1$ (minimized)", fontsize=20, rotation=-27, color="blue")


    # 6. Remove all default axis components
    ax.set_xlim(-0.2, 1.8)
    ax.set_ylim(-0.2, 1.8)
    ax.set_aspect('equal')
    ax.axis('off')

    # 7. Draw manual arrows for the axes
    # X-axis
    ax.annotate('', xy=(1.75, 0), xytext=(-0.1, 0),
                arrowprops=dict(arrowstyle="-|>", color='black', lw=1.2, mutation_scale=20))
    # Y-axis
    ax.annotate('', xy=(0, 1.75), xytext=(0, -0.1),
                arrowprops=dict(arrowstyle="-|>", color='black', lw=1.2, mutation_scale=20))

    # 8. Add manual tick marks and numerical labels
    ticks = [0.0, 0.5, 1.0, 1.5]
    for t in ticks:
        # X-axis ticks
        ax.plot([t, t], [-0.05, 0.05], color='black', lw=1)
        ax.text(t, -0.15, f'{t:.1f}', ha='center', fontsize=16)
        # Y-axis ticks
        ax.plot([-0.05, 0.05], [t, t], color='black', lw=1)
        ax.text(-0.1, t, f'{t:.1f}', va='center', ha='right', fontsize=16)
    plt.tight_layout()
    plt.savefig("example_unit_arc.pdf")
    plt.show()
    plt.close()



    # Epsilon-constraint method plot

    # Create the figure
    fig, ax = plt.subplots(figsize=(8, 8))

    # 1. Define the unit arc: x1^2 + x2^2 = 1
    theta = np.linspace(0, np.pi / 2, 200)
    x_arc = np.cos(theta)
    y_arc = np.sin(theta)

    # 2. Generate an equidistant grid of points for region Y
    # Using a spacing of 0.1 for the structured dot look
    x_grid = np.arange(0, 1.51, 0.1)
    y_grid = np.arange(0, 1.51, 0.1)
    X, Y = np.meshgrid(x_grid, y_grid)

    # Filter: points satisfy x^2 + y^2 >= 0.98 and stay within bounds
    mask = (X ** 2 + Y ** 2 >= 1) & (X <= 1.5) & (Y <= 1.5)
    ax.scatter(X[mask], Y[mask], s=2, color='grey', marker='o')

    # 3. Plot the nondominated front (the arc)
    ax.plot(x_arc, y_arc, color='orange', linewidth=1.5)

    # 5. Text annotations
    ax.text(0.6, 0.6, r'$\mathcal{F}$', fontsize=28, color="orange")
    ax.text(1.2, 1.2, r'$f(\mathcal{S})$', fontsize=28)


    # Define parameters
    epsilon = 0.5
    ax.axhline(epsilon, color='blue', linestyle='--', linewidth=1.5, zorder=10)
    ax.text(0.4,epsilon - 0.15, rf'$x_2 \leq {epsilon}$', color='blue', fontsize=24, va='bottom')
    ax.text(np.sqrt(1-epsilon**2)+0.05,epsilon+0.05, r'$f(\mathbf{x}^*)$', fontsize=28, color="blue")

    # shade vertical region x1 <= epsilon
    ax.axhspan(-0.2, epsilon, color='blue', alpha=0.12, zorder=2)
    # 4. Plot the discrete points at the intercepts (y1 and y2)
    ax.scatter([np.sqrt(1-epsilon**2)],[epsilon], color='blue', s=60, zorder=10)


    # 6. Remove all default axis components
    ax.set_xlim(-0.2, 1.8)
    ax.set_ylim(-0.2, 1.8)
    ax.set_aspect('equal')
    ax.axis('off')

    # 7. Draw manual arrows for the axes
    # X-axis
    ax.annotate('', xy=(1.75, 0), xytext=(-0.1, 0),
                arrowprops=dict(arrowstyle="-|>", color='black', lw=1.2, mutation_scale=20))
    # Y-axis
    ax.annotate('', xy=(0, 1.75), xytext=(0, -0.1),
                arrowprops=dict(arrowstyle="-|>", color='black', lw=1.2, mutation_scale=20))

    # 8. Add manual tick marks and numerical labels
    ticks = [0.0, 0.5, 1.0, 1.5]
    for t in ticks:
        # X-axis ticks
        ax.plot([t, t], [-0.05, 0.05], color='black', lw=1)
        ax.text(t, -0.15, f'{t:.1f}', ha='center', fontsize=16)
        # Y-axis ticks
        ax.plot([-0.05, 0.05], [t, t], color='black', lw=1)
        ax.text(-0.1, t, f'{t:.1f}', va='center', ha='right', fontsize=16)
    plt.tight_layout()
    plt.savefig("example_epsilon_constraint.pdf")
    plt.show()
    plt.close()