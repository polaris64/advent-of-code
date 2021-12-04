import re

FABRIC_W = 1000
FABRIC_H = 1000

def get_claims():
    with open("input.txt") as f:
        claims_list = f.read().split("\n")
        claims = dict()
        for claim in claims_list:
            matches = re.match(r"^#(\d+)\s*@\s*(\d+),(\d+)\s*:\s*(\d+)x(\d+)", claim)
            if matches is not None:
                g = matches.groups()
                if len(g) == 5:
                    claim_id = int(g[0])
                    claims[claim_id] = {
                        # "l": int(g[1]),
                        # "t": int(g[2]),
                        # "w": int(g[3]),
                        # "h": int(g[4]),
                        "x1": int(g[1]),
                        "x2": int(g[1]) + int(g[3]),
                        "y1": int(g[2]),
                        "y2": int(g[2]) + int(g[4]),
                    }
        return claims

def build_fabric(w, h):
    fabric = list()
    for i in range(FABRIC_H):
        fabric.append(list([0 for x in range(FABRIC_W)]))
    return fabric

def add_claims_to_fabric(fabric, claims):
    for (cid, claim) in claims.items():
        for y in range(claim["y1"] - 1, claim["y2"] - 1):
            for x in range(claim["x1"] - 1, claim["x2"] - 1):
                fabric[y][x] += 1

def count_claim_overlaps(claims):
    fabric = build_fabric(FABRIC_W, FABRIC_H)
    add_claims_to_fabric(fabric, claims)

    count = 0
    for y in range(FABRIC_H):
        for x in range(FABRIC_W):
            if fabric[y][x] >= 2:
                count += 1

    return count

def get_non_overlap_claims(claims):
    fabric = build_fabric(FABRIC_W, FABRIC_H)
    add_claims_to_fabric(fabric, claims)
    res = list()
    for (cid, claim) in claims.items():
        max_overlaps = 0
        for y in range(claim["y1"] - 1, claim["y2"] - 1):
            for x in range(claim["x1"] - 1, claim["x2"] - 1):
                if fabric[y][x] > max_overlaps:
                    max_overlaps = fabric[y][x]
        if max_overlaps == 1:
            res.append("#{}".format(cid))
    return res

claims = get_claims()
print("There are {} claims".format(len(claims.keys())))
print(
    "The number of square inches used more than once is: {}".format(
        count_claim_overlaps(claims)
    )
)

print(
    "The following claims have no overlaps: {}".format(
        ", ".join(get_non_overlap_claims(claims))
    )
)
