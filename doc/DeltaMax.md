# Delta Max Explained
## Short intro
The diff/delta display shows the difference in blood glucose between "now" and the last reading. Delta Max controls how far Trndi will look back to calculate this value, if the previous reading is _missing_.

Trndi will always use the closest reading to calculate from, this value regulates how long ago Trndi will try to find a reading when the previous reading(s) are missing.

* __2__ = 5 minutes (2nd trend dot)
* __3__ = 10 minutes (3rd trend dot)
* etc.

## 🔧 How it works
The setting is an integer N (e.g., 2, 3, 4).
When computing a delta for a reading A, the driver uses the next available later reading B — but only if B is within N positions from A (missing entries count as positions).
Formally: delta is calculated only if (index_of_B − index_of_A) < N.
## 🧩 Examples
### N = 2 → only immediate next reading is allowed (no gaps).
A B C D → A→B calculated ✅
A _ B C → A→B NOT calculated ❌
### N = 3 → allows one missing between them.
A _ B C → A→B calculated ✅
### N = 4 → allows up to two missing between.
A _ _ C → A→C calculated ✅

> 💡 Tip: Use small values (2–4) to avoid misleading deltas computed over long gaps; larger values may produce deltas that are less representative of recent changes.