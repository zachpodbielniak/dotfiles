#!/usr/bin/env python3
# dotfiles - Personal configuration files and scripts
# Copyright (C) 2026  Zach Podbielniak
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""
Tests for bin/scripts/tsmctl against synthetic TSM data; no WoW install,
network or user data required.

    python3 tests/scripts/test_tsmctl.py
"""

import hashlib
import importlib.machinery
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from types import ModuleType
from typing import Any

ROOT: Path = Path(__file__).resolve().parents[2]
SCRIPT: Path = ROOT / "bin" / "scripts" / "tsmctl"
B32_DIGITS: str = "0123456789abcdefghijklmnopqrstuv"
ITEMINFO_ALPHABET: str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
NOW: int = int(time.time())


def load_module() -> ModuleType:
	"""Import the extensionless script as a module without running main()."""
	loader: importlib.machinery.SourceFileLoader = importlib.machinery.SourceFileLoader("tsmctl", str(SCRIPT))
	spec: Any = importlib.util.spec_from_loader("tsmctl", loader)
	module: ModuleType = importlib.util.module_from_spec(spec)
	sys.dont_write_bytecode = True
	loader.exec_module(module)
	return module


tsmctl: ModuleType = load_module()


def b32(value: int) -> str:
	"""Encode like the TSM app: lowercase base 32."""
	if value == 0:
		return "0"
	digits: list[str] = []
	while value:
		value, rest = divmod(value, 32)
		digits.append(B32_DIGITS[rest])
	return "".join(reversed(digits))


def iteminfo_record(item_level: int, vendor_sell: int, max_stack: int, quality: int, class_id: int) -> str:
	"""Pack one 24-char TSM item-info record (little-endian 6-bit digits)."""
	def multi(value: int, chars: int) -> str:
		if value < 0:
			return "_" * chars
		return "".join(ITEMINFO_ALPHABET[(value >> (6 * i)) & 63] for i in range(chars))

	def single(value: int) -> str:
		return "_" if value < 0 else ITEMINFO_ALPHABET[value]

	return (
		multi(item_level, 2) + multi(1, 2) + multi(vendor_sell, 5) + multi(max_stack, 2) + single(-1)
		+ multi(-1, 5) + single(class_id) + single(0) + single(quality) + single(0) + single(0) + single(10) + single(-1)
	)


def lua_quote(text: str) -> str:
	return '"' + text.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n") + '"'


def write_fixture(base: Path) -> None:
	"""
	Build <base>/_retail_ with two accounts and an AppData.lua:
	  * Alpha (Testrealm) buys 10 Widgets at 1g, sells 6 at 2g net, has 4 left
	  * Alpha mails 500g to her alt Beta (internal; must not hit the P&L)
	  * a sale dated 90 days in the future (TSM's token-mail bug)
	  * account TWO duplicates one sale row (account sync) that must dedupe
	"""
	retail: Path = base / "_retail_"
	sales: str = "itemString,stackSize,quantity,price,otherPlayer,player,time,source\n" + "\n".join([
		f"i:1001,3,6,20000,Buyerone,Alpha,{NOW - 3600},Auction",
		f"i:1002,1,2,50000,Merchant,Alpha,{NOW - 7200},Vendor",
		f"i:1003,1,1,1000000,Multiple Buyers,Alpha,{NOW + 90 * 86400},Auction",
	])
	buys: str = "itemString,stackSize,quantity,price,otherPlayer,player,time,source\n" + f"i:1001,5,10,10000,Sellerone-Otherrealm,Alpha,{NOW - 86400},Auction"
	expense: str = "type,amount,otherPlayer,player,time\n" + "\n".join([
		f"Postage,30,Beta,Alpha,{NOW - 5000}",
		f"Money Transfer,5000000,Beta,Alpha,{NOW - 5000}",
		f"Repair Bill,12345,Merchant,Alpha,{NOW - 4000}",
	])
	expired: str = "itemString,stackSize,quantity,player,time\n" + f"i:1001,1,4,Alpha,{NOW - 1800}"
	names: str = "\x02".join(["Widget", "Junk", "Token-ish"])
	strings: str = "\x02".join(["i:1001", "i:1002", "i:1003"])
	data: str = iteminfo_record(10, 5000, 20, 2, 7) + iteminfo_record(1, 25000, 1, 0, 15) + iteminfo_record(1, -1, 1, 4, 18)
	gold_log: str = f"minute,copper\n{(NOW - 10 * 86400) // 60},10000000\n{(NOW - 86400) // 60},30000000"
	account_one: str = "\n".join([
		"TradeSkillMasterDB = {",
		'["_syncOwner"] = {},',
		'["_syncAccountKey"] = {},',
		f'["r@Testrealm@internalData@csvSales"] = {lua_quote(sales)},',
		f'["r@Testrealm@internalData@csvBuys"] = {lua_quote(buys)},',
		f'["r@Testrealm@internalData@csvExpense"] = {lua_quote(expense)},',
		f'["r@Testrealm@internalData@csvExpired"] = {lua_quote(expired)},',
		'["r@Testrealm@internalData@csvIncome"] = "type,amount,otherPlayer,player,time",',
		'["s@Alpha - Alliance - Testrealm@internalData@money"] = 25000000,',
		'["s@Alpha - Alliance - Testrealm@internalData@classKey"] = "DEMONHUNTER",',
		f'["s@Alpha - Alliance - Testrealm@internalData@goldLog"] = {lua_quote(gold_log)},',
		'["s@Alpha - Alliance - Testrealm@internalData@bagQuantity"] = { ["i:1001"] = 4, },',
		'["s@Alpha - Alliance - Testrealm@internalData@auctionQuantity"] = {},',
		'["s@Beta - Alliance - Testrealm@internalData@money"] = 5000000,',
		'["s@Beta - Alliance - Testrealm@internalData@classKey"] = "MAGE",',
		'["g@ @internalData@warbankMoney"] = 1000000,',
		'["g@ @internalData@warbankQuantity"] = { ["i:1002"] = 3, },',
		'["p@Default@userData@items"] = { ["i:1001"] = "Flips`Widgets", },',
		'["p@Default@userData@groups"] = { ["Flips"] = {}, ["Flips`Widgets"] = {}, },',
		"}",
		"TSMItemInfoDB = {",
		f'["names"] = {lua_quote(names)},',
		f'["itemStrings"] = {lua_quote(strings)},',
		f'["data"] = {lua_quote(data)},',
		'["versionStr"] = "test",',
		"}",
		"",
	]).replace("\n", "\r\n")
	account_two: str = "\n".join([
		"TradeSkillMasterDB = {",
		f'["r@Testrealm@internalData@csvSales"] = {lua_quote(sales.splitlines()[0] + chr(10) + sales.splitlines()[1])},',
		'["s@Gamma - Horde - Otherrealm@internalData@money"] = 70000,',
		"}",
		"",
	])
	for account, text in (("ONE", account_one), ("TWO#1", account_two)):
		sv: Path = retail / "WTF" / "Account" / account / "SavedVariables"
		sv.mkdir(parents=True)
		(sv / "TradeSkillMaster.lua").write_text(text, encoding="utf-8")

	def dataset(tag: str, where: str, fields: list[str], rows: dict[str, list[int]]) -> str:
		body: str = ",".join("{" + (item[2:] if item.startswith("i:") else f'"{item}"') + "," + ",".join(b32(v) for v in values) + "}" for item, values in rows.items())
		field_list: str = ",".join(f'"{f}"' for f in ["itemString"] + fields)
		return f'select(2, ...).LoadData("{tag}","{where}",[[return {{downloadTime={NOW - 600},fields={{{field_list}}},data={{{body}}}}}]])'

	appdata: list[str] = [
		'select(2, ...).LoadData("APP_INFO","Global",[[return {version=1,lastSync=' + str(NOW - 300) + ',message={id=0,msg=""},news={}}]])',
		dataset("AUCTIONDB_NON_COMMODITY_DATA", "Testrealm", ["minBuyout", "numAuctions", "marketValueRecent"], {"i:1001": [15000, 3, 26000], "i:1004": [100000, 2, 400000]}),
		dataset("AUCTIONDB_NON_COMMODITY_SCAN_STAT", "Testrealm", ["marketValue"], {"i:1001": [25000], "i:1004": [400000]}),
		dataset("AUCTIONDB_REGION_SALE", "US", ["regionSale", "regionSoldPerDay", "regionSalePercent"], {"i:1001": [22000, 5500, 300], "i:1004": [380000, 2000, 250]}),
		dataset("AUCTIONDB_REGION_STAT", "US", ["regionMarketValue"], {"i:1001": [24000], "i:1003": [9999999]}),
	]
	appdata_path: Path = retail / "Interface" / "AddOns" / "TradeSkillMaster_AppHelper" / "AppData.lua"
	appdata_path.parent.mkdir(parents=True)
	appdata_path.write_text("\n".join(appdata) + "\n", encoding="utf-8")


def tree_digest(base: Path) -> str:
	digest: Any = hashlib.sha256()
	for path in sorted(base.rglob("*")):
		if path.is_file():
			digest.update(str(path.relative_to(base)).encode())
			digest.update(path.read_bytes())
	return digest.hexdigest()


class TsmctlUnitTests(unittest.TestCase):
	"""Pure functions: parsers, price expressions, money and time."""

	def test_lua_parser_values(self) -> None:
		source: bytes = b'X = { ["a"] = "q\\"uote\\n\\065", [2] = -1.5, b = true, { 1, 2 }, ["c"] = [[long\nstring]], ["d"] = nil, } Y = 7\r\n'
		parsed: dict[str, Any] = tsmctl.LuaParser(source, "test").parse_assignments()
		self.assertEqual(parsed["Y"], 7)
		table: dict[Any, Any] = parsed["X"]
		self.assertEqual(table["a"], 'q"uote\nA')
		self.assertEqual(table[2], -1.5)
		self.assertIs(table["b"], True)
		self.assertEqual(table[1], [1, 2])
		self.assertEqual(table["c"], "long\nstring")

	def test_lua_parser_array_and_error(self) -> None:
		parsed: dict[str, Any] = tsmctl.LuaParser(b'A = { "x", "y" } B = {}', "t").parse_assignments()
		self.assertEqual(parsed["A"], ["x", "y"])
		self.assertEqual(parsed["B"], {})
		with self.assertRaises(tsmctl.TsmctlError):
			tsmctl.LuaParser(b"A = { os.execute('x') }", "t").parse_assignments()

	def test_parse_money(self) -> None:
		self.assertEqual(tsmctl.parse_money("250g"), 2500000)
		self.assertEqual(tsmctl.parse_money("12g50s"), 125000)
		self.assertEqual(tsmctl.parse_money("75s"), 7500)
		self.assertEqual(tsmctl.parse_money("1,500"), 15000000)
		self.assertEqual(tsmctl.parse_money("-5g"), -50000)
		with self.assertRaises(tsmctl.TsmctlError):
			tsmctl.parse_money("lots")

	def test_fmt_money(self) -> None:
		self.assertEqual(tsmctl.fmt_money(123456789, "g"), "12,345.68g")
		self.assertEqual(tsmctl.fmt_money(123456789, "gsc"), "12,345g 67s 89c")
		self.assertEqual(tsmctl.fmt_money(-150, "gsc"), "-1s 50c")
		self.assertEqual(tsmctl.fmt_money(None, "g"), "-")

	def test_parse_when(self) -> None:
		now: Any = tsmctl.datetime.now().astimezone()
		self.assertAlmostEqual(tsmctl.parse_when("7d", now), int(now.timestamp()) - 7 * 86400, delta=2)
		self.assertAlmostEqual(tsmctl.parse_when("12h", now), int(now.timestamp()) - 12 * 3600, delta=2)
		self.assertIsNone(tsmctl.parse_when("all", now))
		self.assertEqual(tsmctl.parse_when("1790000000", now), 1790000000)
		with self.assertRaises(tsmctl.TsmctlError):
			tsmctl.parse_when("next tuesday", now)

	def test_item_helpers(self) -> None:
		self.assertEqual(tsmctl.item_base("i:237917::i165"), "i:237917")
		self.assertEqual(tsmctl.item_base("p:1720:i1"), "p:1720")
		self.assertEqual(tsmctl.wowhead_url("i:5::1:2"), "https://www.wowhead.com/item=5")

	def test_simple_yaml(self) -> None:
		parsed: dict[str, Any] = tsmctl.SimpleYaml.parse('host: mob-zach  # comment\nah_cut: 5\naccounts:\n  - ONE\n  - "TWO#1"\nflag: true\nlist: [a, b]\n', "t")
		self.assertEqual(parsed, {"host": "mob-zach", "ah_cut": 5, "accounts": ["ONE", "TWO#1"], "flag": True, "list": ["a", "b"]})


class FakePricer(object):
	"""Feeds fixed source values to PriceExpr."""

	def __init__(self, values: dict[str, float]):
		self.values: dict[str, float] = values

	def source(self, name: str, item: str, realm: str) -> Any:
		return self.values.get(name)


class PriceExprTests(unittest.TestCase):
	def evaluate(self, text: str, **values: float) -> Any:
		return tsmctl.PriceExpr(text).evaluate(FakePricer(values), "i:1", "r")

	def test_functions(self) -> None:
		self.assertEqual(self.evaluate("min(dbmarket, dbregionsaleavg)", dbmarket=10.0, dbregionsaleavg=5.0), 5.0)
		self.assertEqual(self.evaluate("min(dbmarket, dbregionsaleavg)", dbmarket=10.0), 10.0)
		self.assertEqual(self.evaluate("first(dbmarket, vendorsell)", vendorsell=3.0), 3.0)
		self.assertEqual(self.evaluate("avg(dbmarket, dbhistorical)", dbmarket=10.0, dbhistorical=20.0), 15.0)
		self.assertEqual(self.evaluate("check(dbregionsaleavg, dbmarket, vendorsell)", dbmarket=9.0, vendorsell=1.0), 1.0)
		self.assertEqual(self.evaluate("check(dbregionsaleavg, dbmarket, vendorsell)", dbregionsaleavg=2.0, dbmarket=9.0, vendorsell=1.0), 9.0)

	def test_arithmetic_and_literals(self) -> None:
		self.assertEqual(self.evaluate("80% dbmarket", dbmarket=100.0), 80.0)
		self.assertEqual(self.evaluate("dbmarket - 1g50s", dbmarket=20000.0), 5000.0)
		self.assertEqual(self.evaluate("(dbmarket + 10c) * 2", dbmarket=5.0), 30.0)
		self.assertIsNone(self.evaluate("dbmarket * 2"))

	def test_rejects_unknown(self) -> None:
		for text in ("dbmarkett", "sum(dbmarket)", "dbmarket +", "check(dbmarket)"):
			with self.assertRaises(tsmctl.TsmctlError):
				tsmctl.PriceExpr(text)


class TsmctlCliTests(unittest.TestCase):
	"""End-to-end runs of the script against the synthetic install."""

	@classmethod
	def setUpClass(cls) -> None:
		cls.temp: tempfile.TemporaryDirectory[str] = tempfile.TemporaryDirectory(prefix="tsmctl-test-")
		cls.base: Path = Path(cls.temp.name)
		cls.wow: Path = cls.base / "wow"
		write_fixture(cls.wow)
		cls.digest: str = tree_digest(cls.wow)
		cls.notes: Path = cls.base / "notes"
		cls.reports: Path = cls.notes / "02_areas" / "personal" / "wow" / "accounting"
		cls.reports.mkdir(parents=True)
		(cls.reports / "00_index.org").write_text(":PROPERTIES:\n:ID:       idx\n:END:\n#+title: Accounting Index\n#+updated: x\n\n* Reports\n\n* See also\n\n- [[id:other][Other]]\n", encoding="utf-8")
		cls.env: dict[str, str] = {
			**os.environ, "HOME": str(cls.base), "XDG_CACHE_HOME": str(cls.base / "cache"), "NO_COLOR": "1",
			"TSMCTL_NOTES_DIR": str(cls.notes), "TSMCTL_REPORT_DIR": str(cls.reports), "PYTHONDONTWRITEBYTECODE": "1",
		}
		for name in ("TSMCTL_HOST", "TSMCTL_CONFIG", "TSMCTL_WOW_DIR", "TSM_WOW_DIR", "TSMCTL_REALM"):
			cls.env.pop(name, None)

	@classmethod
	def tearDownClass(cls) -> None:
		cls.temp.cleanup()

	def run_cli(self, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
		cmd: list[str] = [sys.executable, str(SCRIPT), "--no-config", "--local", "--wow-dir", str(self.wow), *args]
		proc: subprocess.CompletedProcess[str] = subprocess.run(cmd, capture_output=True, text=True, env=self.env, timeout=60)
		if check:
			self.assertEqual(proc.returncode, 0, msg=f"{args}: {proc.stderr}")
			self.assertNotIn("Traceback", proc.stderr)
		return proc

	def json_of(self, *args: str) -> dict[str, Any]:
		return json.loads(self.run_cli("-o", "json", *args).stdout)

	def test_help_and_license(self) -> None:
		self.assertIn("examples:", self.run_cli("--help").stdout)
		self.assertIn("Affero", self.run_cli("--license").stdout)

	def test_sales_dedupe_and_future_clamp(self) -> None:
		doc: dict[str, Any] = self.json_of("get", "sales", "--since", "all", "-n", "0")
		rows: list[dict[str, Any]] = doc["sections"][0]["tables"][0]["rows"]
		self.assertEqual(len(rows), 3, "duplicate synced sale row must be dropped")
		widget: dict[str, Any] = next(r for r in rows if r["item"] == "i:1001")
		self.assertEqual(widget["total_copper"], 120000)
		self.assertEqual(widget["item_name"], "Widget")
		proc: subprocess.CompletedProcess[str] = self.run_cli("get", "sales")
		self.assertIn("dated in the future", proc.stderr)

	def test_pnl_excludes_internal_transfers(self) -> None:
		values: dict[str, Any] = self.json_of("pnl", "--since", "all", "--by", "none")["sections"][0]["values"]
		# Sales 120000 + 100000 + 1000000; buys 100000; expenses 12345 (postage/money to Beta are internal).
		self.assertEqual(values["Revenue"], 1220000)
		self.assertEqual(values["Purchases"], -100000)
		self.assertEqual(values["Expenses"], -12345)
		self.assertEqual(values["Net cash flow"], 1220000 - 100000 - 12345)
		internal: dict[str, Any] = self.json_of("pnl", "--since", "all", "--by", "none", "--include-internal")["sections"][0]["values"]
		self.assertEqual(internal["Expenses"], -(12345 + 30 + 5000000))

	def test_flips_and_sellthrough(self) -> None:
		rows: list[dict[str, Any]] = self.json_of("flips", "--since", "all")["sections"][0]["tables"][0]["rows"]
		widget: dict[str, Any] = rows[0]
		self.assertEqual((widget["bought"], widget["sold"], widget["open"]), (10, 6, 4))
		self.assertEqual(widget["realized_copper"], 6 * 20000 - 6 * 10000)
		st: list[dict[str, Any]] = self.json_of("sellthrough", "--since", "all", "--min-listings", "1")["sections"][0]["tables"][0]["rows"]
		self.assertAlmostEqual(st[0]["rate"], 6 / 10)

	def test_inventory_valuation_uses_check_default(self) -> None:
		rows: list[dict[str, Any]] = self.json_of("get", "inventory")["sections"][0]["tables"][0]["rows"]
		by_item: dict[str, dict[str, Any]] = {r["item"]: r for r in rows}
		# Widget: min(dbmarket 25000, regionsale 22000) = 22000 x 4.
		self.assertEqual(by_item["i:1001"]["value_copper"], 88000)
		# Junk has no region sales -> vendor sell 25000 x 3 (warbank).
		self.assertEqual(by_item["i:1002"]["value_copper"], 75000)

	def test_networth_minimum_and_market(self) -> None:
		section: dict[str, Any] = self.json_of("networth")["sections"][0]
		values: dict[str, Any] = section["values"]
		gold: int = 25000000 + 5000000 + 1000000 + 70000
		# Minimum: Widget 4 x min(25000, 22000); Junk 3 x vendor 25000 (no region sales).
		self.assertEqual(values["Minimum net worth"], gold + 4 * 22000 + 3 * 25000)
		# Market: Widget 4 x dbmarket 25000; Junk falls through to vendor 25000.
		self.assertEqual(values["Market net worth"], gold + 4 * 25000 + 3 * 25000)
		self.assertEqual(values["Market minus minimum"], 4 * 3000)
		self.assertEqual(values["of which items with no region sales"], 3 * 25000)
		totals: dict[str, Any] = section["tables"][0]["totals"]
		self.assertEqual(totals["total_min_copper"], values["Minimum net worth"])
		self.assertEqual(totals["total_market_copper"], values["Market net worth"])
		only_min: dict[str, Any] = self.json_of("networth", "--valuation", "min")["sections"][0]["values"]
		self.assertNotIn("Market net worth", only_min)
		summary: dict[str, Any] = self.json_of("report", "--sections", "summary")["sections"][0]["values"]
		self.assertEqual(summary["Net worth, market"], values["Market net worth"])
		self.assertEqual(summary["Net worth, minimum"], values["Minimum net worth"])

	def test_market_scaling_and_deals(self) -> None:
		rows: list[dict[str, Any]] = self.json_of("get", "prices", "1001", "--realm", "test")["sections"][0]["tables"][0]["rows"]
		self.assertAlmostEqual(rows[0]["rate"], 0.3)
		self.assertAlmostEqual(rows[0]["spd"], 5.5)
		self.assertEqual(rows[0]["minbuyout_copper"], 15000)
		deals: list[dict[str, Any]] = self.json_of("deals", "--realm", "Testrealm", "--min-profit", "1g")["sections"][0]["tables"][0]["rows"]
		self.assertEqual([d["item"] for d in deals], ["i:1004"])
		self.assertEqual(deals[0]["profit_copper"], int(380000 * 0.95 - 100000))

	def test_gold_and_groups(self) -> None:
		gold: dict[str, Any] = self.json_of("get", "gold")["sections"][0]["tables"][0]
		self.assertEqual(gold["totals"]["gold_copper"], 25000000 + 5000000 + 1000000 + 70000)
		classes: set[str] = {r["class"] for r in gold["rows"]}
		self.assertIn("Demon Hunter", classes)
		groups: list[dict[str, Any]] = self.json_of("get", "groups")["sections"][0]["tables"][0]["rows"]
		self.assertIn("Flips > Widgets", {g["group"] for g in groups})
		sales: dict[str, Any] = self.json_of("get", "sales", "--group", "Widgets", "--since", "all")
		self.assertEqual(len(sales["sections"][0]["tables"][0]["rows"]), 1)

	def test_all_commands_all_formats(self) -> None:
		commands: list[list[str]] = [
			["status"], ["paths"], ["config"], ["get", "accounts"], ["get", "realms"], ["get", "chars"], ["get", "gold-history"],
			["get", "ledger"], ["get", "expenses"], ["get", "expired"], ["get", "auctions"], ["get", "professions"], ["get", "sources"],
			["search", "widg"], ["describe", "item", "Widget"], ["describe", "char", "Alpha"], ["describe", "realm", "test"],
			["describe", "group", "Flips"], ["pnl", "--by", "day"], ["pnl", "--by", "item"], ["top", "items", "--by", "roi"],
			["top", "buyers"], ["top", "chars"], ["networth", "--by", "char"], ["networth", "--valuation", "market", "--by", "location"],
			["report", "--deals"],
		]
		for fmt in ("text", "wide", "org", "csv", "tsv", "json"):
			for command in commands:
				with self.subTest(fmt=fmt, command=command):
					self.run_cli("-o", fmt, *command)

	def test_options_after_command(self) -> None:
		doc: dict[str, Any] = json.loads(self.run_cli("get", "sales", "-o", "json", "--since", "all", "--realm", "test").stdout)
		self.assertEqual(doc["title"], "Sales")

	def test_errors_are_clean(self) -> None:
		for args in (["get", "nope"], ["describe", "item", "zzzz-no-item"], ["--since", "garbage", "pnl"], ["deals", "--realm", "Nowhere"], ["--price-source", "bogus(", "networth"]):
			with self.subTest(args=args):
				proc: subprocess.CompletedProcess[str] = self.run_cli(*args, check=False)
				self.assertEqual(proc.returncode, 1)
				self.assertIn("tsmctl: error:", proc.stderr)
				self.assertNotIn("Traceback", proc.stderr)

	def test_save_creates_roam_node_and_links_index(self) -> None:
		first: subprocess.CompletedProcess[str] = self.run_cli("report", "--save")
		self.assertEqual(first.stdout, "")
		saved: list[Path] = sorted(self.reports.glob("*_tsm_report.org"))
		self.assertEqual(len(saved), 1)
		text: str = saved[0].read_text(encoding="utf-8")
		self.assertTrue(text.startswith(":PROPERTIES:\n:ID:       "))
		node_id: str = text.split("\n")[1].split()[-1]
		for header in ("#+title:", "#+description:", "#+authors:", "#+categories:", "#+created:", "#+updated:", "#+version: 1.0"):
			self.assertIn(header, text)
		self.run_cli("report", "--save")
		again: str = saved[0].read_text(encoding="utf-8")
		self.assertIn(f":ID:       {node_id}", again, "regenerating must keep the roam ID")
		self.assertIn("#+version: 1.1", again)
		index: str = (self.reports / "00_index.org").read_text(encoding="utf-8")
		self.assertEqual(index.count(f"[[id:{node_id}]"), 1)
		self.assertLess(index.index(node_id), index.index("* See also"), "report link belongs under * Reports")

	def test_save_refuses_foreign_file(self) -> None:
		target: Path = self.base / "mine.org"
		target.write_text("* my own notes\n", encoding="utf-8")
		proc: subprocess.CompletedProcess[str] = self.run_cli("pnl", "--save", str(target), check=False)
		self.assertEqual(proc.returncode, 1)
		self.assertEqual(target.read_text(encoding="utf-8"), "* my own notes\n")

	def test_zz_wow_files_untouched(self) -> None:
		self.assertEqual(tree_digest(self.wow), self.digest, "tsmctl must never modify WoW/TSM files")


if __name__ == "__main__":
	unittest.main(verbosity=2)
