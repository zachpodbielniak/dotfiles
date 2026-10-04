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
import http.server
import importlib.machinery
import io
import importlib.util
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tarfile
import tempfile
import threading
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


# ============================================================================
# Optional sources: Altoholic DataStore and Syndicator
# ============================================================================

def to_lua(value: Any, indent: int = 0) -> str:
	"""
	Serialise like WoW does: tables one entry per line, arrays positional
	(None becomes a positional nil), anything sparse as [n] = keys.
	"""
	pad: str = "\t" * (indent + 1)
	if value is None:
		return "nil"
	if isinstance(value, bool):
		return "true" if value else "false"
	if isinstance(value, (int, float)):
		return repr(value)
	if isinstance(value, str):
		return lua_quote(value)
	if isinstance(value, list):
		body: str = "".join(f"{pad}{to_lua(v, indent + 1)},\n" for v in value)
		return "{\n" + body + "\t" * indent + "}"
	if isinstance(value, dict):
		# Like the client: the run of keys 1..k is positional, the rest [k] = v.
		lines: list[str] = []
		run: int = 0
		while (run + 1) in value:
			run += 1
			lines.append(f"{pad}{to_lua(value[run], indent + 1)},\n")
		for key, item in value.items():
			if isinstance(key, int) and 1 <= key <= run:
				continue
			key_text: str = f"[{key}]" if isinstance(key, int) else f"[{lua_quote(str(key))}]"
			lines.append(f"{pad}{key_text} = {to_lua(item, indent + 1)},\n")
		return "{\n" + "".join(lines) + "\t" * indent + "}"
	raise TypeError(f"cannot serialise {type(value)}")


def write_sv(path: Path, tables: dict[str, Any]) -> None:
	"""Write a SavedVariables file with CRLF line endings, as the WoW client does."""
	path.parent.mkdir(parents=True, exist_ok=True)
	text: str = "\n".join(f"{name} = {to_lua(value)}" for name, value in tables.items()) + "\n"
	path.write_bytes(text.replace("\n", "\r\n").encode("utf-8"))


def pack_slot(item_id: int, count: int, legacy: bool = False) -> int:
	"""DataStore container slot: count in bits 0-15 (legacy: 0-9) and the item id above."""
	return (item_id << (10 if legacy else 16)) + count


def base_info(level: int, class_id: int, race_id: int, gender: int = 2) -> int:
	"""DataStore_Characters BaseInfo: bits 0-6 level, 7-10 class, 11-17 race, 18-19 gender."""
	return level + (class_id << 7) + (race_id << 11) + (gender << 18)


def recipe(recipe_id: int, learned: bool, rank: int = 0, ranks: int = 0, difficulty: int = 0) -> int:
	"""DataStore_Crafts packed recipe: bits 0-1 colour, 2 learned, 3-4 rank, 5-6 ranks, 7+ id."""
	return difficulty + ((1 if learned else 0) << 2) + (rank << 3) + (ranks << 5) + (recipe_id << 7)


def item_link(item_id: int, name: str, bonuses: list[int] = ()) -> str:  # type: ignore[assignment]
	"""A modern item link; bonus ids sit after the context field (index 12 is their count)."""
	bonus: str = f"{len(bonuses)}:" + ":".join(str(b) for b in bonuses) if bonuses else ""
	return f"|cnIQ2:|Hitem:{item_id}::::::::80:577:::{bonus}::::::|h[{name}]|h|r"


def write_ops_fixture(base: Path) -> None:
	"""
	One account (MAIN) with all three sources, built so every rule shows:

	  * Alpha (Testrealm) is in TSM (gold 1,000g, last seen 2h ago) and in
	    DataStore (1,500g, lastUpdate a minute ago): DataStore's fresher
	    gold, bags and bank win; TSM keeps the warband bank (dated) over
	    DataStore's undated copy.
	  * Beta is TSM only, seen 20 days ago, with TSM's own first-expiry
	    time for an auction that has ended.
	  * Gamma (DataStore only, an Orc, so Horde) was seen 30 days ago.
	  * Delta is on Linkedrealm, connected to Testrealm; the guild
	    "Guildies" is recorded from both realms, by TSM and by DataStore.
	  * Epsilon exists only in Syndicator, with mail expiring in 20 hours.
	  * Alpha's auctions: #111 in both DataStore and Syndicator (joined),
	    #222 posted for two days, #333 already expired (DataStore), #444
	    expired days ago (Syndicator only).
	"""
	sv: Path = base / "_retail_" / "WTF" / "Account" / "MAIN" / "SavedVariables"
	sv.mkdir(parents=True)
	tsm: str = "\n".join([
		"TradeSkillMasterDB = {",
		'["s@Alpha - Alliance - Testrealm@internalData@money"] = 10000000,',
		'["s@Alpha - Alliance - Testrealm@internalData@classKey"] = "DEMONHUNTER",',
		f'["s@Alpha - Alliance - Testrealm@internalData@goldLogLastUpdate"] = {NOW - 7200},',
		'["s@Alpha - Alliance - Testrealm@internalData@bagQuantity"] = { ["i:1001"] = 4, },',
		'["s@Alpha - Alliance - Testrealm@internalData@bankQuantity"] = { ["i:1002"] = 1, },',
		'["s@Beta - Alliance - Testrealm@internalData@money"] = 500000,',
		'["s@Beta - Alliance - Testrealm@internalData@classKey"] = "MAGE",',
		f'["s@Beta - Alliance - Testrealm@internalData@goldLogLastUpdate"] = {NOW - 20 * 86400},',
		'["s@Beta - Alliance - Testrealm@internalData@bagQuantity"] = { ["i:1001"] = 2, },',
		f'["f@Alliance - Testrealm@internalData@expiringAuction"] = {{ ["Beta"] = {NOW - 3600}, }},',
		'["f@Alliance - Testrealm@internalData@expiringMail"] = {},',
		'["f@Alliance - Testrealm@internalData@guildVaults"] = { ["Guildies"] = { ["i:1003"] = 1, }, },',
		'["f@Alliance - Linkedrealm@internalData@guildVaults"] = { ["Guildies"] = { ["i:1003"] = 1, }, },',
		'["g@ @internalData@warbankMoney"] = 1000000,',
		f'["g@ @internalData@warbankGoldLogLastUpdate"] = {NOW - 7200},',
		'["g@ @internalData@warbankQuantity"] = { ["i:1002"] = 3, },',
		"}",
		"TSMItemInfoDB = {",
		f'["names"] = {lua_quote(chr(2).join(["Widget", "Junk", "Gizmo", "Reagent", "Trinket"]))},',
		f'["itemStrings"] = {lua_quote(chr(2).join(["i:1001", "i:1002", "i:1003", "i:1004", "i:1005"]))},',
		f'["data"] = {lua_quote("".join(iteminfo_record(10, v, 20, 2, 7) for v in (5000, 25000, 1000, 300, 70000)))},',
		"}",
		"",
	]).replace("\n", "\r\n")
	(sv / "TradeSkillMaster.lua").write_bytes(tsm.encode("utf-8"))

	write_sv(sv / "DataStore.lua", {
		"DataStore_GuildIDs": {"List": ["Default.Testrealm.Guildies", "Default.Linkedrealm.Guildies"], "Count": 2},
		"DataStore_CharacterIDs": {"List": ["Default.Testrealm.Alpha", "Default.Testrealm.Gamma", "Default.Linkedrealm.Delta"], "Count": 3},
		# Sparse: written as {1, [3] = 2} by the client.
		"DataStore_CharacterGuilds": {1: 1, 3: 2},
		"DataStore_ConnectedRealms": {"Linkedrealm": "Testrealm"},
		"DataStore_RealmNames": {"Testrealm": "Testrealm", "Linkedrealm": "Linkedrealm"},
	})
	write_sv(sv / "DataStore_Characters.lua", {"DataStore_Characters_Info": [
		{"name": "Alpha", "money": 15000000, "played": 360000, "lastUpdate": NOW - 60, "lastLogoutTimestamp": NOW - 60, "zone": "Stormwind City", "BaseInfo": base_info(80, 12, 4)},
		{"name": "Gamma", "money": 100000, "played": 7200, "lastUpdate": NOW - 30 * 86400, "lastLogoutTimestamp": NOW - 30 * 86400, "zone": "Orgrimmar", "BaseInfo": base_info(70, 8, 2)},
		{"name": "Delta", "money": 10000, "played": 600, "lastUpdate": NOW - 3600, "lastLogoutTimestamp": NOW - 3600, "zone": "Dornogal", "BaseInfo": base_info(10, 1, 37)},
	]})
	alpha_containers: dict[Any, Any] = {
		# Backpack: two empty slots (positional nils), a Widget with bonus ids, a battle pet.
		0: {"items": [None, None, pack_slot(1001, 5), pack_slot(4242, 1)], "links": [None, None, item_link(1001, "Widget", [6655, 1692]), "|cnIQ3:|Hbattlepet:1720:25:3:1500:300:280:BattlePet-0-1|h[Pet]|h|r"]},
		# Reagent bag, still in DataStore's legacy 10-bit count format.
		5: {"items": {3: pack_slot(1004, 7, legacy=True)}, "links": {}},
		6: {"items": {1: pack_slot(1002, 2)}, "links": {1: item_link(1002, "Junk")}},
		-7: {"items": {1: pack_slot(1005, 1)}, "links": {}},
	}
	write_sv(sv / "DataStore_Containers.lua", {
		"DataStore_Containers_Characters": {
			1: {"lastUpdate": NOW - 60, "bankInfo": 1234, "Containers": alpha_containers},
			3: {"lastUpdate": NOW - 3600, "Containers": {0: {"items": {1: pack_slot(1003, 1)}, "links": {}}}},
		},
		"DataStore_Containers_Warbank": {12: {"items": {1: pack_slot(1002, 9)}, "links": {}, "name": "Tab"}},
		# The same guild bank seen from both connected realms; each copy has one fresher tab.
		"DataStore_Containers_Guilds": [
			{"money": 0, "faction": "Alliance", "Tabs": [
				{"ClientTime": NOW - 100, "items": {1: pack_slot(1003, 4)}, "links": {}, "name": "T1"},
				{"ClientTime": NOW - 900, "items": {1: pack_slot(1001, 1)}, "links": {}, "name": "T2"},
			]},
			{"money": 0, "faction": "Alliance", "Tabs": [
				{"ClientTime": NOW - 500, "items": {1: pack_slot(1003, 6)}, "links": {}, "name": "T1"},
				{"ClientTime": NOW - 50, "items": {1: pack_slot(1001, 2)}, "links": {}, "name": "T2"},
			]},
		],
		"DataStore_Containers_VoidStorage": {},
	})
	write_sv(sv / "DataStore_Auctions.lua", {
		"DataStore_Auctions_Characters": {1: {"lastUpdate": NOW - 600, "lastAuctionsScan": NOW - 600}},
		"DataStore_Auctions_AuctionsList": {1: [
			"0|1001|2||0|30000|3600|111",
			"0|1004|1||5000|90000|172800|222",
			"0|1005|1||0|70000|300|333",
		]},
	})
	write_sv(sv / "DataStore_Mails.lua", {"DataStore_Mails_Characters": {
		1: {"lastUpdate": NOW - 600, "Mails": [
			{"money": 1000000, "sender": "Auction House", "subject": "Auction successful: Widget", "text": "", "returned": False, "lastCheck": NOW - 600, "daysLeft": 2.0},
			{"itemID": 1002, "count": 4, "link": item_link(1002, "Junk"), "sender": "Beta", "returned": False, "lastCheck": NOW - 600, "daysLeft": 20.0},
			{"money": 0, "sender": "Some NPC", "subject": "A letter", "text": "hello", "returned": False, "lastCheck": NOW - 600, "daysLeft": 10.0},
		], "MailCache": [
			{"link": item_link(1001, "Widget"), "count": 1, "sender": "Beta", "lastCheck": NOW - 100, "daysLeft": 30},
		]},
		3: {"lastUpdate": NOW - 3600, "Mails": [], "MailCache": []},
	}})
	write_sv(sv / "DataStore_Currencies.lua", {
		"DataStore_Currencies_Catalog": {"List": ["Trader's Tender", "Valor"], "Count": 2},
		"DataStore_Currencies_Max": {2032: 0, 1191: 1000 + (500 << 20)},
		"DataStore_Currencies_Characters": {
			1: {"lastUpdate": NOW - 60, "Currencies": [1 + (1 << 8) + (1805 << 18), 1 + (2 << 8) + (50 << 18)], "Totals": {2032: 1805 + (3 << 20), 1191: 50, 9999: 0}},
			# Both ids hold 7 here: only Alpha's counts can tell them apart.
			3: {"lastUpdate": NOW - 3600, "Currencies": [1 + (1 << 8) + (7 << 18)], "Totals": {2032: 7, 1191: 7}},
		},
	})
	write_sv(sv / "DataStore_Crafts.lua", {
		"DataStore_Crafts_RecipeCategories": {100: "Bags"},
		"DataStore_Crafts_Characters": {1: {"lastUpdate": NOW - 60, "Professions": [
			{"Name": "Tailoring", "Crafts": {100: [recipe(12345, True, 1, 3), recipe(12346, False, 0, 0, 3)]}},
		]}},
	})
	write_sv(sv / "Syndicator.lua", {"SYNDICATOR_DATA": {
		"Version": 2,
		"Characters": {
			"Alpha-Testrealm": {
				"details": {"character": "Alpha", "realm": "Testrealm", "realmNormalized": "Testrealm", "faction": "Alliance", "race": "Night Elf", "className": "DEMONHUNTER", "guild": "Guildies-Testrealm"},
				"money": 99900, "bags": [[{"itemID": 1001, "itemCount": 99, "itemLink": item_link(1001, "Widget")}]],
				"auctions": [
					{"auctionID": 111, "itemID": 1001, "itemCount": 2, "itemLink": item_link(1001, "Widget", [6655, 1692]), "expirationTime": NOW + 2000},
					{"auctionID": 444, "itemID": 1002, "itemCount": 1, "itemLink": item_link(1002, "Junk"), "expirationTime": NOW - 5 * 86400},
				],
				"mail": [], "currencies": {2032: 1},
			},
			"Epsilon-Linkedrealm": {
				"details": {"character": "Epsilon", "realm": "Linkedrealm", "realmNormalized": "Linkedrealm", "faction": "Horde", "race": "Orc", "className": "WARRIOR"},
				"money": 20000, "bags": [],
				"mail": [{"itemID": 1001, "itemCount": 3, "itemLink": item_link(1001, "Widget"), "expirationTime": NOW + 20 * 3600}],
				"auctions": [], "currencies": {2032: 5},
			},
		},
		"Warband": [{"money": 7000000, "bank": [{"slots": [{"itemID": 1002, "itemCount": 50}]}]}],
		"Guilds": {"Guildies-Testrealm": {"money": 0, "details": {"guild": "Guildies", "realm": "Testrealm", "faction": "Alliance"}, "bank": [{"slots": [{"itemID": 1003, "itemCount": 77}]}]}},
	}})


def write_edge_fixture(base: Path) -> None:
	"""
	One account with no TSM at all, a DataStore missing most modules, a
	corrupt DataStore_Mails.lua and a corrupt Syndicator.lua: everything
	that can be read must load, the rest must only warn.
	"""
	sv: Path = base / "_retail_" / "WTF" / "Account" / "ALTONLY" / "SavedVariables"
	sv.mkdir(parents=True)
	write_sv(sv / "DataStore.lua", {"DataStore_CharacterIDs": {"List": ["Default.Edgerealm.Zeta"], "Count": 1}})
	write_sv(sv / "DataStore_Characters.lua", {"DataStore_Characters_Info": [
		{"name": "Zeta", "money": 42 * 10000, "lastUpdate": NOW - 100, "lastLogoutTimestamp": NOW - 100, "BaseInfo": base_info(60, 3, 1)},
	]})
	(sv / "DataStore_Mails.lua").write_bytes(b"DataStore_Mails_Characters = {\r\n{ [\"Mails\"] = { oops\r\n")
	(sv / "Syndicator.lua").write_bytes(b"\x00\x01 this is not lua at all")


def load_data(wow: Path, *args: str) -> Any:
	"""Load a fixture in-process: returns (Context, TsmData) for model-level checks."""
	parsed: Any = tsmctl.build_parser().parse_args(["--no-config", "--local", "--wow-dir", str(wow), "-q", *args, "status"])
	tsmctl.normalize_args(parsed)
	ctx: Any = tsmctl.Context(parsed, tsmctl.build_settings(parsed))
	return ctx, ctx.data


class OptionalSourceUnitTests(unittest.TestCase):
	"""Decoders and the reconciliation rule, without files."""

	def test_item_string_from_link(self) -> None:
		self.assertEqual(tsmctl.item_string_from_link(item_link(9944, "x", [6655, 1692])), "i:9944::2:1692:6655")
		self.assertEqual(tsmctl.item_string_from_link("|cnIQ1:|Hitem:240160::::::::1:71::::1:40:2591::::Player-1:|h[Bag]|h|r"), "i:240160")
		self.assertEqual(tsmctl.item_string_from_link("|Hbattlepet:1720:25:3:1500:300:280:BattlePet-0-1|h[Pet]|h"), "p:1720:25:3")
		self.assertEqual(tsmctl.item_string_from_link("|Hkeystone:180653:375:12:9:0:0:0|h[Keystone]|h"), "i:180653")
		self.assertEqual(tsmctl.item_string_from_link("", 77), "i:77")
		self.assertEqual(tsmctl.item_string_from_link(None), "")

	def test_slot_unpacking_handles_legacy_counts(self) -> None:
		self.assertEqual(tsmctl.ds_unpack_slot(pack_slot(237917, 2000)), (237917, 2000))
		# 1004 has bits 3-5 set, so the old 10-bit layout is recognised.
		self.assertEqual(tsmctl.ds_unpack_slot(pack_slot(1004, 7, legacy=True)), (1004, 7))
		self.assertEqual(tsmctl.ds_unpack_slot(pack_slot(1000, 1, legacy=True), force_legacy=True), (1000, 1))

	def test_pick_freshest(self) -> None:
		pick: Any = tsmctl.pick_freshest
		self.assertEqual(pick([("tsm", 1000, "a"), ("datastore", 5000, "b")])[0], "datastore")
		self.assertEqual(pick([("tsm", 5000, "a"), ("datastore", 5100, "b")])[0], "tsm", "within the tie window TSM wins")
		self.assertEqual(pick([("tsm", None, "a"), ("datastore", 10, "b")])[0], "datastore", "dated beats undated")
		self.assertEqual(pick([("syndicator", None, "a"), ("datastore", None, "b")])[0], "datastore", "undated: priority")
		self.assertIsNone(pick([]))

	def test_currency_names_resolve_by_count(self) -> None:
		names: dict[int, str] = tsmctl.resolve_currency_names([
			({2032: 1805, 1191: 50}, [("Trader's Tender", 1805), ("Valor", 50)]),
			({2032: 7, 1191: 7}, [("Trader's Tender", 7)]),
		])
		self.assertEqual(names, {2032: "Trader's Tender", 1191: "Valor"})
		ambiguous: dict[int, str] = tsmctl.resolve_currency_names([({1: 5, 2: 5}, [("A", 5)])])
		self.assertEqual(ambiguous, {}, "a count shared by two ids names neither")

	def test_parse_duration_and_sources(self) -> None:
		self.assertEqual(tsmctl.parse_duration("6h"), 6 * 3600)
		self.assertEqual(tsmctl.parse_duration("45m"), 45 * 60)
		self.assertEqual(tsmctl.parse_duration("3", "d"), 3 * 86400)
		self.assertEqual(tsmctl.parse_duration("1.5d"), 129600)
		with self.assertRaises(tsmctl.TsmctlError):
			tsmctl.parse_duration("soon")
		self.assertEqual(tsmctl.parse_source_list(["syndicator,altoholic"]), ["datastore", "syndicator"])
		self.assertEqual(tsmctl.parse_source_list("all"), [])
		with self.assertRaises(tsmctl.TsmctlError):
			tsmctl.parse_source_list("bogus")


class FakeTarPopen(object):
	"""Stands in for `ssh host tar cf -`: streams a prepared tar archive."""

	archive: bytes = b""

	def __init__(self, cmd: list[str], stdout: Any = None, stderr: Any = None):
		self.stdout: Any = io.BytesIO(FakeTarPopen.archive)
		self.stderr: Any = io.BytesIO(b"")
		self.returncode: int = 0

	def kill(self) -> None:
		pass

	def wait(self, timeout: Any = None) -> int:
		return 0


class RemoteMirrorTests(unittest.TestCase):
	"""The ssh mirror lists every source's files and accepts only those."""

	def test_listing_and_tar_are_strict(self) -> None:
		with tempfile.TemporaryDirectory(prefix="tsmctl-mirror-") as temp:
			args: Any = tsmctl.build_parser().parse_args(["--no-config", "--host", "far-away.invalid", "--cache-dir", temp, "status"])
			tsmctl.normalize_args(args)
			source: Any = tsmctl.DataSource(tsmctl.build_settings(args))
			sv: str = "WTF/Account/MAIN/SavedVariables"
			listing: str = "".join(f"{NOW}.0\t10\t{sv}/{name}\n" for name in ("TradeSkillMaster.lua", "DataStore_Mails.lua", "Syndicator.lua", "Evil.lua"))
			commands: list[str] = []

			def fake_ssh(remote_cmd: str, binary: bool = False, timeout: int = 120) -> Any:
				commands.append(remote_cmd)
				return subprocess.CompletedProcess([], 0, stdout=listing, stderr="")

			# The archive also carries a file nobody asked for and a symlink.
			buffer: io.BytesIO = io.BytesIO()
			with tarfile.open(fileobj=buffer, mode="w") as archive:
				for name in ("TradeSkillMaster.lua", "DataStore_Mails.lua", "Syndicator.lua", "Evil.lua"):
					data: bytes = b"X = 1\n"
					info: tarfile.TarInfo = tarfile.TarInfo(f"{sv}/{name}")
					info.size = len(data)
					archive.addfile(info, io.BytesIO(data))
				link: tarfile.TarInfo = tarfile.TarInfo(f"{sv}/DataStore.lua")
				link.type = tarfile.SYMTYPE
				link.linkname = "/etc/passwd"
				archive.addfile(link)
			FakeTarPopen.archive = buffer.getvalue()
			original: Any = tsmctl.subprocess.Popen
			source._ssh = fake_ssh
			tsmctl.subprocess.Popen = FakeTarPopen
			try:
				source._sync({})
			finally:
				tsmctl.subprocess.Popen = original
			self.assertIn("-name DataStore_Mails.lua", commands[0])
			self.assertIn("-name Syndicator.lua", commands[0])
			root: Path = source.root / "WTF" / "Account" / "MAIN" / "SavedVariables"
			self.assertEqual(sorted(p.name for p in root.iterdir()), ["DataStore_Mails.lua", "Syndicator.lua", "TradeSkillMaster.lua"])
			manifest: dict[str, Any] = json.loads(source._manifest_path().read_text(encoding="utf-8"))
			self.assertNotIn(f"{sv}/Evil.lua", manifest["files"], "names outside SOURCE_FILES are never mirrored")
			# A wanted name that arrives as a symlink is refused, not followed.
			listing = f"{NOW}.0\t10\t{sv}/DataStore.lua\n"
			tsmctl.subprocess.Popen = FakeTarPopen
			try:
				with self.assertRaises(tsmctl.TsmctlError):
					source._sync({})
			finally:
				tsmctl.subprocess.Popen = original
			self.assertFalse(os.path.lexists(root / "DataStore.lua"))


class OptionalSourceCliTests(unittest.TestCase):
	"""TSM + DataStore + Syndicator together: merge, reconcile and the commands built on them."""

	@classmethod
	def setUpClass(cls) -> None:
		cls.temp: tempfile.TemporaryDirectory[str] = tempfile.TemporaryDirectory(prefix="tsmctl-ops-")
		cls.base: Path = Path(cls.temp.name)
		cls.wow: Path = cls.base / "wow"
		cls.edge: Path = cls.base / "edge"
		write_ops_fixture(cls.wow)
		write_edge_fixture(cls.edge)
		cls.digests: dict[str, str] = {"wow": tree_digest(cls.wow), "edge": tree_digest(cls.edge)}
		cls.env: dict[str, str] = {**os.environ, "HOME": str(cls.base), "XDG_CACHE_HOME": str(cls.base / "cache"), "NO_COLOR": "1", "PYTHONDONTWRITEBYTECODE": "1"}
		for name in ("TSMCTL_HOST", "TSMCTL_CONFIG", "TSMCTL_WOW_DIR", "TSM_WOW_DIR", "TSMCTL_REALM", "TSMCTL_SOURCES"):
			cls.env.pop(name, None)

	@classmethod
	def tearDownClass(cls) -> None:
		cls.temp.cleanup()

	def run_cli(self, *args: str, wow: Any = None, check: bool = True) -> subprocess.CompletedProcess[str]:
		cmd: list[str] = [sys.executable, str(SCRIPT), "--no-config", "--local", "--wow-dir", str(wow or self.wow), *args]
		proc: subprocess.CompletedProcess[str] = subprocess.run(cmd, capture_output=True, text=True, env=self.env, timeout=60)
		if check:
			self.assertEqual(proc.returncode, 0, msg=f"{args}: {proc.stderr}")
			self.assertNotIn("Traceback", proc.stderr)
		return proc

	def table(self, *args: str, index: int = 0) -> dict[str, Any]:
		doc: dict[str, Any] = json.loads(self.run_cli("-o", "json", *args).stdout)
		return [t for s in doc["sections"] for t in s["tables"]][index]

	def test_characters_merge_across_sources(self) -> None:
		rows: dict[str, dict[str, Any]] = {r["name"]: r for r in self.table("get", "chars", "-n", "0")["rows"]}
		self.assertEqual(set(rows), {"Alpha", "Beta", "Gamma", "Delta", "Epsilon"})
		alpha: dict[str, Any] = rows["Alpha"]
		self.assertEqual((alpha["level"], alpha["class"], alpha["race"], alpha["guild"]), (80, "Demon Hunter", "Night Elf", "Guildies"))
		self.assertEqual(alpha["gold_copper"], 15000000, "DataStore's gold is a minute old, TSM's two hours")
		self.assertEqual(alpha["played"], 360000)
		self.assertEqual((rows["Gamma"]["faction"], rows["Gamma"]["class"]), ("Horde", "Mage"), "faction comes from the race")
		self.assertEqual(rows["Delta"]["race"], "Mechagnome")
		self.assertEqual(rows["Epsilon"]["faction"], "Horde", "a Syndicator-only character still appears")
		self.assertEqual(rows["Beta"]["gold_copper"], 500000)

	def test_holdings_reconcile_by_freshness(self) -> None:
		ctx, data = load_data(self.wow)
		held: dict[tuple[str, str, str], int] = {}
		sources: dict[tuple[str, str], str] = {}
		for h in data.holdings:
			key: tuple[str, str, str] = (h.owner, h.location, tsmctl.item_base(h.item))
			held[key] = held.get(key, 0) + h.qty
			sources[(h.owner, h.location)] = h.source
		# Alpha's bags and bank: DataStore is fresher than TSM.
		self.assertEqual(held[("Alpha", "bag", "i:1001")], 5)
		self.assertEqual(held[("Alpha", "bag", "i:1004")], 7, "legacy 10-bit slot decoded")
		self.assertEqual(held[("Alpha", "bag", "p:1720")], 1, "battle pet named from its link, not the icon id")
		self.assertEqual(held[("Alpha", "bank", "i:1002")], 2)
		self.assertEqual(held[("Alpha", "reagent_bank", "i:1005")], 1)
		self.assertEqual(sources[("Alpha", "bag")], "datastore")
		# Beta has only TSM; the warband bank is dated in TSM only, so TSM keeps it.
		self.assertEqual(held[("Beta", "bag", "i:1001")], 2)
		self.assertEqual(held[("Warbank", "warbank", "i:1002")], 3)
		self.assertEqual(sources[("Warbank", "warbank")], "tsm")
		# One guild on two connected realms, from two sources: counted once, each tab from its freshest copy.
		guild: list[Any] = [h for h in data.holdings if h.location == "guild"]
		self.assertEqual({tsmctl.item_base(h.item): h.qty for h in guild}, {"i:1003": 4, "i:1001": 2})
		self.assertEqual({h.source for h in guild}, {"datastore"})
		# Auctions come from DataStore (dated) rather than Syndicator.
		self.assertEqual(held[("Alpha", "auction", "i:1001")], 2)
		self.assertEqual(sources[("Alpha", "auction")], "datastore")

	def test_model_for_export(self) -> None:
		"""The dataclasses the export builds on: joined auctions, mail, named currencies, recipes."""
		ctx, data = load_data(self.wow)
		auctions: dict[int, Any] = {a.auction_id: a for a in data.auctions}
		joined: Any = auctions[111]
		self.assertEqual((joined.char, joined.realm, joined.qty, joined.buyout, joined.buyout_total), ("Alpha", "Testrealm", 2, 30000, 60000))
		self.assertEqual(joined.sources, ["datastore", "syndicator"])
		self.assertIn("|Hitem:1001:", joined.link, "the raw link survives for the instrument mapping")
		self.assertAlmostEqual(joined.expires_at, NOW + 2000, delta=5)
		self.assertEqual((auctions[222].bid, auctions[222].bid_each), (5000, 5000))
		mails: list[Any] = [m for m in data.mails if m.char == "Alpha"]
		self.assertEqual(len(mails), 4)
		self.assertEqual(sum(m.money for m in mails), 1000000)
		cached: Any = next(m for m in mails if m.from_cache)
		self.assertEqual((cached.item, cached.qty, cached.sender), ("i:1001", 1, "Beta"))
		returned_flags: set[bool] = {m.returned for m in mails}
		self.assertEqual(returned_flags, {False})
		names: dict[tuple[str, int], str] = {(c.char, c.currency_id): c.name for c in data.currencies}
		self.assertEqual(names[("Delta", 1191)], "Valor")
		recipes: list[Any] = sorted(data.recipes, key=lambda r: r.recipe_id)
		self.assertEqual([(r.recipe_id, r.learned, r.rank, r.max_rank, r.difficulty, r.category) for r in recipes], [(12345, True, 1, 3, 0, "Bags"), (12346, False, 0, 0, 3, "Bags")])
		alpha: Any = data.char_ref("alpha", "Testrealm")
		self.assertEqual((alpha.money_source, alpha.last_seen), ("datastore", NOW - 60))
		self.assertEqual(sorted(alpha.sources), ["datastore", "syndicator", "tsm"])

	def test_sources_selection(self) -> None:
		tsm_only: dict[str, dict[str, Any]] = {r["name"]: r for r in self.table("--sources", "tsm", "get", "chars")["rows"]}
		self.assertEqual(set(tsm_only), {"Alpha", "Beta"})
		self.assertEqual(tsm_only["Alpha"]["gold_copper"], 10000000)
		ds_only: dict[str, dict[str, Any]] = {r["name"]: r for r in self.table("--sources", "datastore", "get", "chars")["rows"]}
		self.assertEqual(set(ds_only), {"Alpha", "Gamma", "Delta"})
		self.assertEqual(ds_only["Alpha"]["gold_copper"], 15000000)
		states: dict[str, str] = {r["source"]: r["state"] for r in self.table("--sources", "syndicator", "status")["rows"]}
		self.assertEqual(states, {"tsm": "not selected", "datastore": "not selected", "syndicator": "used"})
		env_run: subprocess.CompletedProcess[str] = subprocess.run(
			[sys.executable, str(SCRIPT), "--no-config", "--local", "--wow-dir", str(self.wow), "-o", "json", "get", "chars"],
			capture_output=True, text=True, env={**self.env, "TSMCTL_SOURCES": "tsm"}, timeout=60,
		)
		self.assertEqual(len(json.loads(env_run.stdout)["sections"][0]["tables"][0]["rows"]), 2)
		bad: subprocess.CompletedProcess[str] = self.run_cli("--sources", "nope", "status", check=False)
		self.assertEqual(bad.returncode, 1)
		self.assertIn("unknown data source", bad.stderr)

	def test_optional_files_missing_or_corrupt_only_warn(self) -> None:
		proc: subprocess.CompletedProcess[str] = self.run_cli("-o", "json", "get", "chars", wow=self.edge)
		rows: list[dict[str, Any]] = json.loads(proc.stdout)["sections"][0]["tables"][0]["rows"]
		self.assertEqual([(r["name"], r["level"], r["gold_copper"]) for r in rows], [("Zeta", 60, 420000)])
		self.assertIn("DataStore_Mails.lua", proc.stderr)
		self.assertIn("Syndicator", proc.stderr)
		self.assertIn("DataStore_Auctions.lua not found", proc.stderr)
		status: dict[str, Any] = json.loads(self.run_cli("-o", "json", "-q", "status", wow=self.edge).stdout)
		states: dict[str, str] = {r["source"]: r["state"] for r in next(s for s in status["sections"] if s["title"] == "Sources")["tables"][0]["rows"]}
		self.assertEqual(states, {"tsm": "missing", "datastore": "used", "syndicator": "error"})
		for command in (["attention"], ["get", "auctions"], ["get", "mail"], ["get", "currencies"], ["get", "recipes"]):
			with self.subTest(command=command):
				self.run_cli("-q", *command, wow=self.edge)
		nothing: subprocess.CompletedProcess[str] = self.run_cli("--sources", "tsm", "status", wow=self.edge, check=False)
		self.assertEqual(nothing.returncode, 1, "asking only for a source no account has is an error")

	def test_auction_join_and_expiry(self) -> None:
		rows: list[dict[str, Any]] = self.table("get", "auctions", "-n", "0")["rows"]
		by_id: dict[int, dict[str, Any]] = {r["id"]: r for r in rows}
		self.assertEqual(set(by_id), {111, 222, 333, 444})
		joined: dict[str, Any] = by_id[111]
		self.assertEqual(joined["sources"], "datastore+syndicator")
		self.assertEqual(joined["item"], "i:1001::2:1692:6655", "item string from Syndicator's link")
		self.assertEqual((joined["buyout_copper"], joined["total_copper"], joined["qty"]), (30000, 60000, 2))
		self.assertAlmostEqual(joined["left"], 2000, delta=120, msg="Syndicator's absolute expiry wins over DataStore's scan + time left")
		self.assertEqual(by_id[222]["bid_copper"], 5000)
		self.assertLess(by_id[333]["left"], 0)
		self.assertEqual(by_id[444]["sources"], "syndicator")
		self.assertIsNone(by_id[444]["buyout_copper"])
		soon: list[dict[str, Any]] = self.table("get", "auctions", "--expiring", "2h", "-n", "0")["rows"]
		self.assertEqual({r["id"] for r in soon}, {111, 333, 444}, "--expiring keeps expired ones and drops #222")
		self.assertEqual([r["id"] for r in soon], [444, 333, 111], "soonest (most overdue) first")
		fallback: dict[str, Any] = self.table("get", "auctions", "--by", "char")
		self.assertIn("key", fallback["columns"], "--by keeps the aggregated holdings view")

	def test_mail(self) -> None:
		rows: list[dict[str, Any]] = self.table("get", "mail", "-n", "0")["rows"]
		self.assertEqual(len(rows), 4, "letter without money or items hidden")
		self.assertEqual(rows[0]["char"], "Epsilon", "soonest to expire first")
		money: dict[str, Any] = next(r for r in rows if r["money_copper"])
		self.assertEqual((money["char"], money["money_copper"]), ("Alpha", 1000000))
		self.assertAlmostEqual(money["left"], 2 * 86400 - 600, delta=120)
		self.assertEqual(sum(1 for r in rows if r["pending"]), 1, "the alt's unseen mail is marked")
		self.assertEqual(len(self.table("get", "mail", "--all", "-n", "0")["rows"]), 5)
		urgent: list[dict[str, Any]] = self.table("get", "mail", "--expiring", "3d", "-n", "0")["rows"]
		self.assertEqual({r["char"] for r in urgent}, {"Alpha", "Epsilon"})
		self.assertEqual(len(urgent), 2)

	def test_currencies(self) -> None:
		rows: list[dict[str, Any]] = self.table("get", "currencies", "-n", "0")["rows"]
		alpha: dict[str, dict[str, Any]] = {r["currency"]: r for r in rows if r["char"] == "Alpha"}
		self.assertEqual((alpha["Trader's Tender"]["qty"], alpha["Trader's Tender"]["week"]), (1805, 3))
		self.assertEqual((alpha["Valor"]["max"], alpha["Valor"]["weekly"]), (1000, 500))
		self.assertEqual(alpha["Trader's Tender"]["source"], "datastore", "DataStore (dated) beats Syndicator")
		delta: dict[str, Any] = next(r for r in rows if r["char"] == "Delta" and r["id"] == 2032)
		self.assertEqual(delta["currency"], "Trader's Tender", "named through Alpha's counts")
		epsilon: dict[str, Any] = next(r for r in rows if r["char"] == "Epsilon")
		self.assertEqual((epsilon["currency"], epsilon["qty"], epsilon["source"]), ("Trader's Tender", 5, "syndicator"))
		totals: dict[int, dict[str, Any]] = {r["id"]: r for r in self.table("get", "currencies", "--by", "currency")["rows"]}
		self.assertEqual(totals[2032]["qty"], 1805 + 7 + 5)
		self.assertNotIn(9999, totals, "zero balances are hidden")

	def test_recipes(self) -> None:
		rows: list[dict[str, Any]] = self.table("get", "recipes")["rows"]
		self.assertEqual([(r["id"], r["category"], r["rank"]) for r in rows], [(12345, "Bags", "1/3")])
		self.assertEqual(len(self.table("get", "recipes", "--all")["rows"]), 2)
		self.assertEqual(len(self.table("get", "recipes", "--profession", "tail", "--char", "Alpha")["rows"]), 1)
		self.assertEqual(len(self.table("get", "recipes", "--profession", "mining")["rows"]), 0)

	def test_attention_order_and_thresholds(self) -> None:
		doc: dict[str, Any] = json.loads(self.run_cli("-o", "json", "attention").stdout)
		rows: list[dict[str, Any]] = doc["sections"][0]["tables"][0]["rows"]
		order: list[tuple[str, str]] = [(r["kind"] if "kind" in r else r["what"], r["char"]) for r in rows]
		self.assertEqual(order, [
			("mail expiring", "Epsilon"), ("mail expiring", "Alpha"),
			("auctions expired, collect", "Alpha"), ("auctions expired, collect", "Beta"),
			("auctions expiring", "Alpha"), ("mail waiting", "Alpha"),
			("not seen lately", "Gamma"), ("not seen lately", "Beta"),
		])
		expired: dict[str, Any] = rows[2]
		self.assertEqual(expired["count"], 2, "DataStore's #333 and Syndicator's #444")
		self.assertIsNone(rows[3]["count"], "Beta is known only from TSM's first-expiry time")
		notes: list[str] = doc["sections"][0]["notes"]
		self.assertTrue(notes[0].startswith("- log in to Linkedrealm: Epsilon"), notes)
		self.assertTrue(notes[1].startswith("- log in to Testrealm: Alpha, Beta"), notes)
		self.assertIn("Gamma (Testrealm)", notes[2])
		tight: list[dict[str, Any]] = json.loads(self.run_cli("-o", "json", "attention", "--expiring", "30m", "--mail-expiring", "12h", "--stale", "25d").stdout)["sections"][0]["tables"][0]["rows"]
		kinds: list[tuple[str, str]] = [(r["what"], r["char"]) for r in tight]
		self.assertNotIn(("auctions expiring", "Alpha"), kinds)
		self.assertNotIn(("mail expiring", "Epsilon"), kinds)
		self.assertIn(("mail waiting", "Epsilon"), kinds)
		self.assertNotIn(("not seen lately", "Beta"), kinds, "20 days is not stale at 25d")
		self.assertIn(("not seen lately", "Gamma"), kinds)

	def test_new_commands_all_formats(self) -> None:
		commands: list[list[str]] = [
			["attention"], ["get", "auctions"], ["get", "auctions", "--expiring", "6h"], ["get", "auctions", "--by", "item"],
			["get", "mail"], ["get", "mail", "--expiring", "3d", "--all"], ["get", "currencies"], ["get", "currencies", "--by", "currency"],
			["get", "recipes", "--all"], ["describe", "char", "Alpha"], ["get", "chars"], ["get", "accounts"], ["status"],
			["get", "inventory", "--by", "location"], ["networth", "--by", "location"], ["report"],
		]
		for fmt in ("text", "wide", "org", "csv", "tsv", "json"):
			for command in commands:
				with self.subTest(fmt=fmt, command=command):
					self.run_cli("-q", "-o", fmt, *command)

	def test_describe_char_enriched(self) -> None:
		doc: dict[str, Any] = json.loads(self.run_cli("-o", "json", "describe", "char", "Alpha").stdout)
		values: dict[str, Any] = doc["sections"][0]["values"]
		self.assertEqual((values["Level"], values["Race"], values["Posted auctions"]), (80, "Night Elf", 4))
		self.assertEqual(values["Mail waiting (gold)"], 1000000)
		titles: list[str] = [s["title"] for s in doc["sections"]]
		for title in ("Auctions (soonest to end first)", "Mail with gold or items", "Currencies"):
			self.assertIn(title, titles)

	def test_status_reports_sources(self) -> None:
		status: dict[str, Any] = json.loads(self.run_cli("-o", "json", "status").stdout)
		sources: dict[str, Any] = {r["source"]: r for r in next(s for s in status["sections"] if s["title"] == "Sources")["tables"][0]["rows"]}
		self.assertEqual({k: v["state"] for k, v in sources.items()}, {"tsm": "used", "datastore": "used", "syndicator": "used"})
		self.assertEqual(sources["datastore"]["files"], 7)
		self.assertIsNotNone(sources["datastore"]["data"])

	def test_commands_all_formats_with_every_source(self) -> None:
		commands: list[list[str]] = [
			["get", "chars"], ["get", "accounts"], ["get", "gold"], ["status"], ["get", "auctions"], ["describe", "char", "Alpha"],
			["get", "inventory", "--by", "location"], ["networth", "--by", "location"], ["report"],
		]
		for fmt in ("text", "wide", "org", "csv", "tsv", "json"):
			for command in commands:
				with self.subTest(fmt=fmt, command=command):
					self.run_cli("-q", "-o", fmt, *command)

	def test_zz_wow_files_untouched(self) -> None:
		self.assertEqual(tree_digest(self.wow), self.digests["wow"], "tsmctl must never modify SavedVariables")
		self.assertEqual(tree_digest(self.edge), self.digests["edge"], "tsmctl must never modify SavedVariables")


# ============================================================================
# VENTURE: export contract, stable ids, push, units
# ============================================================================

DECIMAL_RE: Any = re.compile(r"^[0-9]+(\.[0-9]{1,4})?$")
ZONED_RE: Any = re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})$")
STRICT: dict[str, set[str]] = {
	"account": {"type", "key", "name", "kind", "group", "venue", "last_seen", "attrs"},
	"account_snapshot": {"type", "account", "at", "covers"},
	"balance": {"type", "account", "currency", "amount", "at"},
	"holding": {"type", "account", "place", "instrument", "quantity", "at"},
	"position": {"type", "account", "venue", "id", "instrument", "quantity", "price", "bid", "expires_at", "posted_at"},
	"inbound": {"type", "account", "id", "sender", "subject", "money", "instrument", "quantity", "expires_at", "returned", "cod"},
	"txn": {"type", "id", "account", "venue", "kind", "instrument", "quantity", "unit_price", "amount", "counterparty", "source", "at"},
}
REQUIRED: dict[str, set[str]] = {
	"venue": {"key"}, "instrument": {"key"}, "snapshot": {"venue", "taken_at"}, "stat": {"venue", "instrument"},
	"account": {"key", "kind"}, "account_snapshot": {"account", "at", "covers"}, "balance": {"account", "currency", "amount"},
	"holding": {"account", "place", "instrument", "quantity"},
	"position": {"account", "venue", "id", "instrument", "quantity", "price", "expires_at"},
	"inbound": {"account", "id"}, "txn": {"id", "account", "kind", "at"},
}
MONEY_MEMBERS: set[str] = {"amount", "price", "bid", "money", "cod", "unit_price", "min", "market", "mean", "median", "sale_avg", "historical"}
TIME_MEMBERS: set[str] = {"at", "last_seen", "expires_at", "posted_at", "taken_at"}
PLACES: set[str] = {"bag", "bank", "reagent_bank", "warbank", "guild", "mail", "auction", "void", "equipped", "currency", "other"}
COVER_OF: dict[str, str] = {"balance": "balances", "holding": "holdings", "position": "positions", "inbound": "inbound"}


def contract_problems(text: str, currency: str = "GOLD") -> list[str]:
	"""
	An independent reading of VENTURE's protocol-1 rules (docs/plugins.org,
	src/plugin/venture-jsonl.c and the feed batch), so the export is checked
	by something other than tsmctl's own checker. Returns the problems.
	"""
	problems: list[str] = []
	snapshotted: set[str] = set()
	rows_before: dict[str, set[str]] = {}
	ids: dict[str, set[str]] = {}
	for number, line in enumerate(text.splitlines(), 1):
		obj: dict[str, Any] = json.loads(line)
		kind: str = obj.get("type", "")
		where: str = f"line {number} ({kind})"
		if kind not in REQUIRED:
			problems.append(f"{where}: unexpected type")
			continue
		if kind in STRICT and set(obj) - STRICT[kind]:
			problems.append(f"{where}: unknown members {sorted(set(obj) - STRICT[kind])}")
		missing: set[str] = REQUIRED[kind] - set(obj)
		if missing:
			problems.append(f"{where}: missing {sorted(missing)}")
		for name, value in obj.items():
			if name in MONEY_MEMBERS and not (isinstance(value, str) and DECIMAL_RE.match(value)):
				problems.append(f"{where}: {name} is not a decimal string with at most 4 places: {value!r}")
			if name in TIME_MEMBERS and not (isinstance(value, str) and ZONED_RE.match(value)):
				problems.append(f"{where}: {name} is not a zoned time: {value!r}")
			if name in ("quantity", "listings") and (isinstance(value, bool) or not isinstance(value, int)):
				problems.append(f"{where}: {name} is not an integer")
			if isinstance(value, str) and len(value.encode("utf-8")) > 512:
				problems.append(f"{where}: {name} longer than 512 bytes")
		if kind == "account":
			if obj.get("kind") not in ("character", "shared", "guild", "other"):
				problems.append(f"{where}: bad account kind")
			attrs: Any = obj.get("attrs", {})
			if not isinstance(attrs, dict) or any(isinstance(v, (dict, list)) for v in attrs.values()) or len(attrs) > 64:
				problems.append(f"{where}: attrs not a flat object")
		elif kind == "account_snapshot":
			account: str = obj["account"]
			covers: list[str] = obj["covers"]
			if not covers or len(set(covers)) != len(covers) or set(covers) - {"holdings", "positions", "inbound", "balances"}:
				problems.append(f"{where}: bad covers {covers}")
			if account in snapshotted:
				problems.append(f"{where}: second snapshot for {account}")
			if rows_before.get(account, set()) & set(covers):
				problems.append(f"{where}: snapshot after the rows it covers")
			snapshotted.add(account)
		elif kind in COVER_OF:
			rows_before.setdefault(obj["account"], set()).add(COVER_OF[kind])
			if kind == "balance" and obj["currency"].upper() != currency:
				problems.append(f"{where}: balance in {obj['currency']}")
			if kind == "holding" and (obj["place"] not in PLACES or obj["quantity"] < 0):
				problems.append(f"{where}: bad place or quantity")
			if kind == "position" and obj["quantity"] < 1:
				problems.append(f"{where}: position quantity below 1")
			if kind == "inbound":
				if not ({"money", "instrument", "cod"} & set(obj)):
					problems.append(f"{where}: nothing to collect")
				if ("instrument" in obj) != ("quantity" in obj):
					problems.append(f"{where}: instrument without quantity")
				if "returned" in obj and not isinstance(obj["returned"], bool):
					problems.append(f"{where}: returned not a boolean")
		elif kind == "txn":
			goods: bool = obj["kind"] in ("sale", "buy", "expired", "cancelled")
			if obj["kind"] not in ("sale", "buy", "income", "expense", "expired", "cancelled"):
				problems.append(f"{where}: bad txn kind")
			if goods and not ({"instrument", "quantity"} <= set(obj)):
				problems.append(f"{where}: {obj['kind']} without instrument and quantity")
			if obj["kind"] in ("expired", "cancelled") and ({"amount", "unit_price"} & set(obj)):
				problems.append(f"{where}: {obj['kind']} carries money")
			if obj["kind"] in ("sale", "buy", "income", "expense") and "amount" not in obj:
				problems.append(f"{where}: no amount")
		elif kind == "stat":
			figures: set[str] = {"min", "market", "mean", "median", "sale_avg", "historical", "quantity", "listings", "sold", "sale_rate", "sold_per_day"}
			if not (figures & set(obj)):
				problems.append(f"{where}: stat without a figure")
			if "sale_rate" in obj and not (DECIMAL_RE.match(obj["sale_rate"]) and float(obj["sale_rate"]) <= 1):
				problems.append(f"{where}: sale_rate out of 0..1")
		elif kind == "snapshot" and not isinstance(obj.get("complete", False), bool):
			problems.append(f"{where}: complete not a boolean")
		if kind in ("position", "inbound", "txn"):
			seen: set[str] = ids.setdefault(kind, set())
			if obj.get("id") in seen:
				problems.append(f"{where}: repeated id")
			seen.add(obj.get("id"))
	return problems


def parse_lines(text: str) -> list[dict[str, Any]]:
	return [json.loads(line) for line in text.splitlines() if line.strip()]


class VentureUnitTests(unittest.TestCase):
	"""Key mapping, money and slugs."""

	def test_blizzard_key_mapping(self) -> None:
		key: Any = tsmctl.venture_item_key
		self.assertEqual(key("i:2770").key, "2770", "a plain item is its id")
		mapped: Any = key("i:9944::3:6655:1692:1692")
		self.assertEqual((mapped.key, mapped.parent), ("9944:b1692,6655", "9944"), "bonuses sorted ascending, repeats dropped, parent is the plain item")
		self.assertEqual(key("i:133358::2:7756:13828:1:9:70").key, "133358:b7756,13828", "TSM modifier pairs never reach the key")
		self.assertEqual(key("i:133358::2:7756:13828:1:9:70").modifiers, "9=70")
		level: Any = key("i:1420::i13")
		self.assertEqual((level.key, level.level), ("1420", "13"), "TSM's level-only form drops to the plain item, the level kept for attrs")
		pet: Any = key("p:1720:25:3")
		self.assertEqual((pet.key, pet.parent, pet.species), ("82800:p1720", "82800", 1720))
		self.assertEqual(key("p:1720:i1").key, "82800:p1720")
		for bad in ("", "x:1", "i:", "i:abc", "i:0"):
			self.assertIsNone(key(bad), bad)

	def test_money_and_ratios(self) -> None:
		self.assertEqual(tsmctl.venture_money(27249501812), "2724950.1812")
		self.assertEqual(tsmctl.venture_money(1000000), "100")
		self.assertEqual(tsmctl.venture_money(50), "0.005")
		self.assertEqual(tsmctl.venture_money(0), "0")
		with self.assertRaises(ValueError):
			tsmctl.venture_money(-1)
		self.assertEqual(tsmctl.venture_milli(300), "0.3")
		self.assertEqual(tsmctl.venture_milli(5500), "5.5")
		self.assertEqual(tsmctl.venture_milli(1000), "1")
		self.assertEqual(tsmctl.venture_time(0), "1970-01-01T00:00:00Z")

	def test_realm_slug(self) -> None:
		self.assertEqual(tsmctl.realm_slug("Thorium Brotherhood"), "thorium-brotherhood")
		self.assertEqual(tsmctl.realm_slug("Mal'Ganis"), "malganis")
		self.assertEqual(tsmctl.realm_slug("Area 52"), "area-52")

	def test_txn_identity_ignores_quantity(self) -> None:
		a: Any = tsmctl.Txn(kind="sale", realm="R", account="A", time=5, player="P", item="i:1", stack=1, qty=3, price=10, raw_time=5)
		b: Any = tsmctl.Txn(kind="sale", realm="R", account="B", time=9, player="P", item="i:1", stack=1, qty=7, price=10, raw_time=5)
		self.assertEqual(a.identity, b.identity, "quantity, account and the clamped time are not part of a row's identity")
		self.assertNotEqual(a.identity, tsmctl.Txn(kind="sale", realm="R", account="A", time=5, player="P", item="i:1", stack=1, qty=3, price=10, raw_time=5, occurrence=1).identity)

	def test_settle_waits_for_quiet(self) -> None:
		with tempfile.TemporaryDirectory(prefix="tsmctl-settle-") as temp:
			path: Path = Path(temp) / "TradeSkillMaster.lua"
			path.write_text("x", encoding="utf-8")
			old: float = time.time() - 600
			os.utime(path, (old, old))
			started: float = time.monotonic()
			tsmctl.venture_settle([path], 5)
			self.assertLess(time.monotonic() - started, 1.0, "files untouched for ten minutes are already settled")
			os.utime(path, None)
			started = time.monotonic()
			tsmctl.venture_settle([path], 1)
			self.assertGreaterEqual(time.monotonic() - started, 0.9, "a file written just now is waited on")

	def test_contract_check_catches_breakage(self) -> None:
		"""tsmctl's own checker refuses what VENTURE refuses (it gates every push)."""
		check: Any = tsmctl.VentureCheck("GOLD")
		lines: list[str] = [
			'{"protocol":1}',
			'{"type":"holding","account":"A","place":"bag","instrument":"1","quantity":1}',
			'{"type":"account_snapshot","account":"A","at":"2026-10-04T00:00:00Z","covers":["holdings"]}',
			'{"type":"account_snapshot","account":"A","at":"2026-10-04T00:00:00Z","covers":["balances"]}',
			'{"type":"balance","account":"A","currency":"USD","amount":"1.23456"}',
			'{"type":"position","account":"A","venue":"v","id":"1","instrument":"1","quantity":1,"price":12.5,"expiry":"2026-10-04T00:00:00Z"}',
			'{"type":"txn","id":"t","account":"A","kind":"expired","instrument":"1","quantity":1,"amount":"5","at":"2026-10-04 00:00"}',
			'{"type":"inbound","account":"A","id":"m","sender":"x"}',
		]
		for number, line in enumerate(lines, 1):
			check.line(number, line)
		text: str = "\n".join(check.problems)
		for expected in ("line 1", "comes after", "second snapshot", "currency must be", "more than 4 decimal", "unknown member 'expiry'", "price must be a decimal", "expires_at is required", "carries no money", "with a zone", "nothing to collect"):
			self.assertIn(expected, text)


class VentureExportTests(unittest.TestCase):
	"""`tsmctl export --format venture` against both synthetic installs."""

	@classmethod
	def setUpClass(cls) -> None:
		cls.temp: tempfile.TemporaryDirectory[str] = tempfile.TemporaryDirectory(prefix="tsmctl-venture-")
		cls.base: Path = Path(cls.temp.name)
		cls.ops: Path = cls.base / "ops"
		cls.tsm: Path = cls.base / "tsm"
		write_ops_fixture(cls.ops)
		write_fixture(cls.tsm)
		cls.digests: dict[str, str] = {"ops": tree_digest(cls.ops), "tsm": tree_digest(cls.tsm)}
		cls.env: dict[str, str] = {**os.environ, "HOME": str(cls.base), "XDG_CACHE_HOME": str(cls.base / "cache"), "XDG_CONFIG_HOME": str(cls.base / "config"), "NO_COLOR": "1", "PYTHONDONTWRITEBYTECODE": "1"}
		for name in ("TSMCTL_HOST", "TSMCTL_CONFIG", "TSMCTL_WOW_DIR", "TSM_WOW_DIR", "TSMCTL_REALM", "TSMCTL_SOURCES", "TSMCTL_VENTURE_TOKEN", "VENTURE_TOKEN", "TSMCTL_VENTURE_URL", "TSMCTL_VENTURE_SOURCE"):
			cls.env.pop(name, None)

	@classmethod
	def tearDownClass(cls) -> None:
		cls.temp.cleanup()

	def run_cli(self, wow: Path, *args: str, check: bool = True, env: Any = None) -> subprocess.CompletedProcess[str]:
		cmd: list[str] = [sys.executable, str(SCRIPT), "--no-config", "--local", "--wow-dir", str(wow), *args]
		proc: subprocess.CompletedProcess[str] = subprocess.run(cmd, capture_output=True, text=True, env=env or self.env, timeout=120)
		if check:
			self.assertEqual(proc.returncode, 0, msg=f"{args}: {proc.stderr}")
			self.assertNotIn("Traceback", proc.stderr)
		return proc

	def export(self, wow: Path, *args: str) -> str:
		return self.run_cli(wow, "-q", "export", "--format", "venture", *args).stdout

	def test_every_line_meets_the_contract(self) -> None:
		for wow, extra in ((self.ops, ()), (self.tsm, ()), (self.tsm, ("--market", "all")), (self.ops, ("--include-internal",))):
			with self.subTest(wow=wow.name, extra=extra):
				text: str = self.export(wow, *extra)
				self.assertTrue(text.endswith("\n"))
				self.assertEqual(contract_problems(text), [])
				self.assertNotIn('"protocol"', text.splitlines()[0], "protocol 1 has no header line")

	def test_ops_export_content(self) -> None:
		lines: list[dict[str, Any]] = parse_lines(self.export(self.ops))
		accounts: dict[str, dict[str, Any]] = {l["key"]: l for l in lines if l["type"] == "account"}
		self.assertIn("Alpha-Testrealm", accounts)
		alpha: dict[str, Any] = accounts["Alpha-Testrealm"]
		self.assertEqual((alpha["kind"], alpha["venue"], alpha["group"]), ("character", "testrealm", "Testrealm"))
		self.assertEqual((alpha["attrs"]["level"], alpha["attrs"]["login_account"], alpha["attrs"]["played_seconds"]), (80, "MAIN", 360000))
		self.assertEqual(accounts["warbank:MAIN"]["kind"], "shared")
		guilds: list[str] = [k for k, v in accounts.items() if v["kind"] == "guild"]
		self.assertEqual(guilds, ["guild:Guildies-Testrealm"], "one guild seen from two connected realms is one account, named after its home realm")
		balances: dict[str, str] = {l["account"]: l["amount"] for l in lines if l["type"] == "balance"}
		self.assertEqual(balances["Alpha-Testrealm"], "1500", "15,000,000 copper is 1500 gold")
		self.assertEqual(balances["warbank:MAIN"], "100")
		positions: dict[str, dict[str, Any]] = {l["id"]: l for l in lines if l["type"] == "position"}
		self.assertEqual(set(positions), {"111", "222", "333"}, "#444 is Syndicator-only and has no known buyout: left out, not invented")
		self.assertEqual((positions["111"]["price"], positions["111"]["quantity"], positions["111"]["instrument"]), ("3", 2, "1001:b1692,6655"))
		self.assertEqual(positions["222"]["bid"], "0.5")
		inbound: list[dict[str, Any]] = [l for l in lines if l["type"] == "inbound"]
		self.assertEqual({(l.get("money"), l.get("instrument"), l.get("quantity")) for l in inbound if l["account"] == "Alpha-Testrealm"}, {
			("100", None, None), (None, "1002", 4), (None, "1001", 1),
		}, "one line per item or money entry; the bare letter is left out")
		self.assertEqual(len([l for l in inbound if l["account"] == "Alpha-Testrealm"]), 3)
		holdings: dict[tuple[str, str, str], int] = {(l["account"], l["place"], l["instrument"]): l["quantity"] for l in lines if l["type"] == "holding"}
		self.assertEqual(holdings[("Alpha-Testrealm", "currency", "currency:2032")], 1805)
		self.assertEqual(holdings[("Alpha-Testrealm", "reagent_bank", "1005")], 1)
		self.assertEqual(holdings[("Alpha-Testrealm", "bag", "82800:p1720")], 1)
		instruments: dict[str, dict[str, Any]] = {l["key"]: l for l in lines if l["type"] == "instrument"}
		self.assertEqual(instruments["currency:2032"]["name"], "Trader's Tender")
		self.assertEqual(instruments["1001"]["attrs"]["vendor_sell"], 5000)
		self.assertEqual(instruments["1001:b1692,6655"]["parent"], "1001")
		order: list[str] = [l["key"] for l in lines if l["type"] == "instrument"]
		self.assertLess(order.index("1001"), order.index("1001:b1692,6655"), "a plain item comes before its variants")

	def test_coverage_is_only_claimed_with_detail_data(self) -> None:
		def covers(text: str) -> dict[str, list[str]]:
			return {l["account"]: l["covers"] for l in parse_lines(text) if l["type"] == "account_snapshot"}

		full: dict[str, list[str]] = covers(self.export(self.ops))
		self.assertEqual(full["Alpha-Testrealm"], ["holdings", "positions", "inbound", "balances"])
		self.assertEqual(full["Beta-Testrealm"], ["holdings", "balances"], "Beta is TSM-only: no auction ids, no mailbox")
		self.assertEqual(full["Delta-Linkedrealm"], ["holdings", "inbound", "balances"], "an empty mailbox that was read is still covered")
		self.assertEqual(full["Gamma-Testrealm"], ["balances"])
		self.assertEqual(full["Epsilon-Linkedrealm"], ["holdings", "positions", "inbound", "balances"])
		tsm_only: dict[str, list[str]] = covers(self.export(self.ops, "--sources", "tsm"))
		for account, kinds in tsm_only.items():
			self.assertNotIn("positions", kinds, account)
			self.assertNotIn("inbound", kinds, account)
		lines: list[dict[str, Any]] = parse_lines(self.export(self.ops, "--sources", "tsm"))
		self.assertFalse([l for l in lines if l["type"] in ("position", "inbound")])

	def test_snapshot_at_is_the_files_time(self) -> None:
		"""An export of older files must be older, so VENTURE skips it instead of rolling back."""
		lines: list[dict[str, Any]] = parse_lines(self.export(self.ops))
		newest: float = max(p.stat().st_mtime for p in (self.ops / "_retail_" / "WTF" / "Account" / "MAIN" / "SavedVariables").iterdir())
		at: str = next(l["at"] for l in lines if l["type"] == "account_snapshot" and l["account"] == "Alpha-Testrealm")
		self.assertEqual(at, tsmctl.venture_time(newest))

	def test_ledger_lines_and_internal_transfers(self) -> None:
		lines: list[dict[str, Any]] = parse_lines(self.export(self.tsm))
		txns: list[dict[str, Any]] = [l for l in lines if l["type"] == "txn"]
		self.assertEqual(sorted(t["kind"] for t in txns), ["buy", "expense", "expired", "sale", "sale", "sale"], "postage and the 500g to an alt are internal")
		widget: dict[str, Any] = next(t for t in txns if t["kind"] == "sale" and t["instrument"] == "1001")
		self.assertEqual((widget["quantity"], widget["unit_price"], widget["amount"], widget["counterparty"], widget["source"], widget["venue"]), (6, "2", "12", "Buyerone", "Auction", "testrealm"))
		self.assertEqual(sum(1 for t in txns if t["kind"] == "sale" and t.get("instrument") == "1001"), 1, "account TWO's synced copy of the row is one row")
		expired: dict[str, Any] = next(t for t in txns if t["kind"] == "expired")
		self.assertNotIn("amount", expired)
		internal: list[dict[str, Any]] = [l for l in parse_lines(self.export(self.tsm, "--include-internal")) if l["type"] == "txn"]
		self.assertEqual(len(internal), len(txns) + 2)
		recent: list[dict[str, Any]] = [l for l in parse_lines(self.export(self.tsm, "--since", "2h")) if l["type"] == "txn"]
		self.assertLess(len(recent), len(txns))
		self.assertTrue(recent)

	def test_market_scope_and_stats(self) -> None:
		lines: list[dict[str, Any]] = parse_lines(self.export(self.tsm))
		stats: dict[tuple[str, str], dict[str, Any]] = {(l["venue"], l["instrument"]): l for l in lines if l["type"] == "stat"}
		self.assertEqual(stats[("testrealm", "1001")]["min"], "1.5")
		self.assertEqual(stats[("testrealm", "1001")]["market"], "2.5")
		self.assertEqual(stats[("testrealm", "1001")]["listings"], 3)
		region: dict[str, Any] = stats[("region-us", "1001")]
		self.assertEqual((region["sale_avg"], region["sale_rate"], region["sold_per_day"], region["market"]), ("2.2", "0.3", "5.5", "2.4"))
		self.assertNotIn(("testrealm", "1004"), stats, "an item nobody holds or traded is out of the default scope")
		snapshots: list[dict[str, Any]] = [l for l in lines if l["type"] == "snapshot"]
		self.assertTrue(snapshots and not any(s["complete"] for s in snapshots), "a scoped snapshot is a sample")
		every: list[dict[str, Any]] = parse_lines(self.export(self.tsm, "--market", "all"))
		self.assertIn(("testrealm", "1004"), {(l["venue"], l["instrument"]) for l in every if l["type"] == "stat"})
		self.assertTrue(all(l["complete"] for l in every if l["type"] == "snapshot"))
		none: list[dict[str, Any]] = parse_lines(self.export(self.tsm, "--market", "none"))
		self.assertFalse([l for l in none if l["type"] in ("snapshot", "stat")])
		venues: dict[str, dict[str, Any]] = {l["key"]: l for l in lines if l["type"] == "venue"}
		self.assertEqual((venues["testrealm"]["group"], venues["region-us"]["group"]), ("us", "us"))

	def test_deterministic_and_stable_ids(self) -> None:
		first: str = self.export(self.tsm)
		self.assertEqual(first, self.export(self.tsm), "the same files make the same bytes")

	def test_txn_ids_survive_a_merge_that_grows_quantity(self) -> None:
		with tempfile.TemporaryDirectory(prefix="tsmctl-merge-") as temp:
			wow: Path = Path(temp) / "wow"
			shutil.copytree(self.tsm, wow)
			before: dict[str, dict[str, Any]] = {l["id"]: l for l in parse_lines(self.export(wow)) if l["type"] == "txn"}
			# TSM merges a later identical sale into the row: quantity 6 -> 9.
			for account in ("ONE", "TWO#1"):
				path: Path = wow / "_retail_" / "WTF" / "Account" / account / "SavedVariables" / "TradeSkillMaster.lua"
				text: str = path.read_text(encoding="utf-8")
				path.write_text(text.replace("i:1001,3,6,20000,Buyerone", "i:1001,3,9,20000,Buyerone"), encoding="utf-8")
			after: dict[str, dict[str, Any]] = {l["id"]: l for l in parse_lines(self.export(wow)) if l["type"] == "txn"}
			self.assertEqual(set(before), set(after), "no id changes when a quantity grows")
			# The row TSM dated in the future is dated at the file's save
			# time, which the edit moved; its id stays. Compare the rest.
			changed: list[str] = [i for i in before if {k: v for k, v in before[i].items() if k != "at"} != {k: v for k, v in after[i].items() if k != "at"}]
			self.assertEqual(len(changed), 1)
			self.assertEqual((before[changed[0]]["quantity"], after[changed[0]]["quantity"], after[changed[0]]["amount"]), (6, 9, "18"))
			# Only one account's copy saw the merge: still one row, the larger.
			path = wow / "_retail_" / "WTF" / "Account" / "TWO#1" / "SavedVariables" / "TradeSkillMaster.lua"
			path.write_text(path.read_text(encoding="utf-8").replace("i:1001,3,9,20000,Buyerone", "i:1001,3,6,20000,Buyerone"), encoding="utf-8")
			mixed: list[dict[str, Any]] = [l for l in parse_lines(self.export(wow)) if l["type"] == "txn" and l["id"] == changed[0]]
			self.assertEqual([l["quantity"] for l in mixed], [9])

	def test_check_mode_and_file_output(self) -> None:
		doc: dict[str, Any] = json.loads(self.run_cli(self.ops, "-q", "-o", "json", "export", "--check").stdout)
		values: dict[str, Any] = doc["sections"][0]["values"]
		self.assertEqual(values["Contract check"], "ok")
		counts: dict[str, int] = {r["type"]: r["lines"] for r in doc["sections"][0]["tables"][0]["rows"]}
		self.assertEqual(counts["position"], 3)
		target: Path = self.base / "out.jsonl"
		self.run_cli(self.ops, "-q", "export", "--file", str(target))
		self.assertEqual(target.stat().st_mode & 0o777, 0o600, "an export is private data")
		self.assertEqual(contract_problems(target.read_text(encoding="utf-8")), [])
		bad: subprocess.CompletedProcess[str] = self.run_cli(self.ops, "export", "--format", "csv", check=False)
		self.assertEqual(bad.returncode, 1)

	def test_zz_wow_files_untouched(self) -> None:
		self.assertEqual(tree_digest(self.ops), self.digests["ops"])
		self.assertEqual(tree_digest(self.tsm), self.digests["tsm"])


class FakeVenture(object):
	"""
	A stand-in for VENTURE's push and runs endpoints on 127.0.0.1, recording
	every request. `answers` maps a path prefix to (status, JSON body, extra
	headers). While a request is in flight it scans every process's command
	line for the token, which must never be there.
	"""

	def __init__(self, token: str):
		self.token: str = token
		self.requests: list[dict[str, Any]] = []
		self.answers: dict[str, tuple[int, Any, dict[str, str]]] = {}
		fake: FakeVenture = self

		class Handler(http.server.BaseHTTPRequestHandler):
			def log_message(self, *args: Any) -> None:
				pass

			def handle_any(self) -> None:
				length: int = int(self.headers.get("Content-Length") or 0)
				body: bytes = self.rfile.read(length) if length else b""
				leaked: bool = False
				for cmdline in Path("/proc").glob("[0-9]*/cmdline"):
					try:
						if fake.token.encode() in cmdline.read_bytes():
							leaked = True
					except OSError:
						continue
				fake.requests.append({"method": self.command, "path": self.path, "headers": dict(self.headers), "body": body, "leaked": leaked})
				status, payload, headers = next((v for k, v in fake.answers.items() if self.path.startswith(k)), (404, {"error": {"message": "no such route"}}, {}))
				data: bytes = json.dumps(payload).encode()
				self.send_response(status)
				self.send_header("Content-Type", "application/json")
				self.send_header("Content-Length", str(len(data)))
				for name, value in headers.items():
					self.send_header(name, value)
				self.end_headers()
				self.wfile.write(data)

			do_GET = handle_any
			do_POST = handle_any

		self.server: http.server.ThreadingHTTPServer = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
		self.url: str = f"http://127.0.0.1:{self.server.server_address[1]}"
		self.thread: threading.Thread = threading.Thread(target=self.server.serve_forever, daemon=True)
		self.thread.start()

	def close(self) -> None:
		self.server.shutdown()
		self.server.server_close()


class VenturePushTests(unittest.TestCase):
	"""`tsmctl venture push|status` against a fake server."""

	TOKEN: str = "vt_test_0123456789abcdef"

	@classmethod
	def setUpClass(cls) -> None:
		cls.temp: tempfile.TemporaryDirectory[str] = tempfile.TemporaryDirectory(prefix="tsmctl-push-")
		cls.base: Path = Path(cls.temp.name)
		cls.wow: Path = cls.base / "wow"
		write_ops_fixture(cls.wow)

	@classmethod
	def tearDownClass(cls) -> None:
		cls.temp.cleanup()

	def setUp(self) -> None:
		self.fake: FakeVenture = FakeVenture(self.TOKEN)
		self.home: Path = Path(tempfile.mkdtemp(prefix="home-", dir=self.base))
		self.env: dict[str, str] = {**os.environ, "HOME": str(self.home), "XDG_CACHE_HOME": str(self.home / "cache"), "XDG_CONFIG_HOME": str(self.home / "config"), "NO_COLOR": "1", "PYTHONDONTWRITEBYTECODE": "1", "TSMCTL_VENTURE_TOKEN": self.TOKEN}
		for name in ("TSMCTL_HOST", "TSMCTL_CONFIG", "TSMCTL_WOW_DIR", "TSM_WOW_DIR", "TSMCTL_SOURCES", "VENTURE_TOKEN", "TSMCTL_VENTURE_URL", "TSMCTL_VENTURE_SOURCE", "TSMCTL_VENTURE_TOKEN_FILE"):
			self.env.pop(name, None)

	def tearDown(self) -> None:
		self.fake.close()

	def push(self, *args: str, env: Any = None, url: Any = None) -> subprocess.CompletedProcess[str]:
		cmd: list[str] = [sys.executable, str(SCRIPT), "--no-config", "--local", "--wow-dir", str(self.wow), "-q", "venture", "push", "--url", url or self.fake.url, "--source", "7", *args]
		self.assertNotIn(self.TOKEN, " ".join(cmd))
		return subprocess.run(cmd, capture_output=True, text=True, env=env or self.env, timeout=120)

	def test_push_with_wait(self) -> None:
		self.fake.answers["/api/v1/feeds/7/push"] = (200, {"status": "finished", "data_source_id": 7, "push_id": "p-1", "run": {"id": 3, "status": "ok", "trigger": "push", "rows": 42, "refused": 0, "notes": "push: accounts 6 (6 new)"}}, {})
		proc: subprocess.CompletedProcess[str] = self.push("--wait", "--organization", "2")
		self.assertEqual(proc.returncode, 0, proc.stderr)
		self.assertIn("accounts 6 (6 new)", proc.stdout)
		self.assertEqual(len(self.fake.requests), 1)
		request: dict[str, Any] = self.fake.requests[0]
		self.assertEqual(request["method"], "POST")
		self.assertEqual(request["path"], "/api/v1/feeds/7/push?wait=1&organization_id=2")
		self.assertEqual(request["headers"]["Authorization"], f"Bearer {self.TOKEN}")
		self.assertEqual(request["headers"]["Content-Type"], "application/x-ndjson")
		self.assertFalse(request["leaked"], "the token was visible in a process's command line")
		self.assertEqual(contract_problems(request["body"].decode("utf-8")), [])
		self.assertNotIn(self.TOKEN, proc.stdout + proc.stderr, "the token is never printed")

	def test_push_queued_and_if_changed(self) -> None:
		self.fake.answers["/api/v1/feeds/7/push"] = (202, {"status": "queued", "data_source_id": 7, "push_id": "p-2"}, {})
		first: subprocess.CompletedProcess[str] = self.push("--if-changed")
		self.assertEqual(first.returncode, 0, first.stderr)
		self.assertIn("queued", first.stdout)
		self.assertNotIn("wait", self.fake.requests[0]["path"])
		second: subprocess.CompletedProcess[str] = self.push("--if-changed")
		self.assertEqual(second.returncode, 0, second.stderr)
		self.assertIn("unchanged", second.stdout)
		self.assertEqual(len(self.fake.requests), 1, "an unchanged export is not sent again")

	def test_exit_codes(self) -> None:
		self.fake.answers["/api/v1/feeds/7/push"] = (409, {"error": {"code": "conflict", "message": "The source's provider is not push"}}, {})
		refused: subprocess.CompletedProcess[str] = self.push()
		self.assertEqual(refused.returncode, 4)
		self.assertIn("provider is not push", refused.stderr)
		self.fake.answers["/api/v1/feeds/7/push"] = (200, {"status": "finished", "push_id": "p", "run": {"id": 4, "status": "failed", "error": "push: Line 3: bad"}}, {})
		self.assertEqual(self.push("--wait").returncode, 5)
		self.fake.answers["/api/v1/feeds/7/push"] = (200, {"status": "finished", "push_id": "p", "run": {"id": 5, "status": "partial"}}, {})
		self.assertEqual(self.push("--wait").returncode, 6)
		self.fake.answers["/api/v1/feeds/7/push"] = (302, {}, {"Location": "http://127.0.0.2:9/steal"})
		count: int = len(self.fake.requests)
		self.assertEqual(self.push().returncode, 4, "a redirect is refused, not followed with the token")
		self.assertEqual(len(self.fake.requests), count + 1)
		closed: socket.socket = socket.socket()
		closed.bind(("127.0.0.1", 0))
		port: int = closed.getsockname()[1]
		closed.close()
		self.assertEqual(self.push(url=f"http://127.0.0.1:{port}").returncode, 3)
		plain: subprocess.CompletedProcess[str] = self.push(url="http://venture.example.invalid")
		self.assertEqual(plain.returncode, 1)
		self.assertIn("plain http", plain.stderr)

	def test_token_sources_and_file_permissions(self) -> None:
		self.fake.answers["/api/v1/feeds/7/push"] = (202, {"status": "queued", "push_id": "p"}, {})
		env: dict[str, str] = dict(self.env)
		env.pop("TSMCTL_VENTURE_TOKEN")
		missing: subprocess.CompletedProcess[str] = self.push(env=env)
		self.assertEqual(missing.returncode, 1)
		self.assertIn("no VENTURE API token", missing.stderr)
		token_file: Path = self.home / "config" / "tsmctl" / "venture-token"
		token_file.parent.mkdir(parents=True)
		token_file.write_text(self.TOKEN + "\n", encoding="utf-8")
		token_file.chmod(0o644)
		loose: subprocess.CompletedProcess[str] = self.push(env=env)
		self.assertEqual(loose.returncode, 1)
		self.assertIn("chmod 600", loose.stderr)
		self.assertEqual(self.fake.requests, [], "nothing is sent with a token from a readable file")
		token_file.chmod(0o600)
		ok: subprocess.CompletedProcess[str] = self.push(env=env)
		self.assertEqual(ok.returncode, 0, ok.stderr)
		self.assertEqual(self.fake.requests[-1]["headers"]["Authorization"], f"Bearer {self.TOKEN}")
		venturectl_env: dict[str, str] = {**env, "VENTURE_TOKEN": "vt_other_token_value"}
		token_file.unlink()
		self.assertEqual(self.push(env=venturectl_env).returncode, 0)
		self.assertEqual(self.fake.requests[-1]["headers"]["Authorization"], "Bearer vt_other_token_value", "venturectl's VENTURE_TOKEN is the last resort")

	def test_lock_serialises_pushes(self) -> None:
		self.fake.answers["/api/v1/feeds/7/push"] = (202, {"status": "queued", "push_id": "p"}, {})
		holder: Any = tsmctl.venture_lock(self.home / "cache" / "tsmctl" / "venture" / "push.lock", 0)
		try:
			busy: subprocess.CompletedProcess[str] = self.push("--lock-timeout", "1")
			self.assertEqual(busy.returncode, 1)
			self.assertIn("holds", busy.stderr)
		finally:
			holder.close()
		self.assertEqual(self.push("--lock-timeout", "1").returncode, 0)

	def test_status(self) -> None:
		self.fake.answers["/api/v1/health"] = (200, {"status": "ok", "version": "0.6.0"}, {})
		self.fake.answers["/api/v1/feeds/7/runs"] = (200, {"data_source_id": 7, "runs": [{"id": 9, "trigger": "push", "status": "ok", "started_at": "2026-10-04T18:00:00Z", "rows": 12, "refused": 0}]}, {})
		cmd: list[str] = [sys.executable, str(SCRIPT), "--no-config", "-o", "json", "venture", "status", "--url", self.fake.url, "--source", "7"]
		proc: subprocess.CompletedProcess[str] = subprocess.run(cmd, capture_output=True, text=True, env=self.env, timeout=60)
		self.assertEqual(proc.returncode, 0, proc.stderr)
		doc: dict[str, Any] = json.loads(proc.stdout)
		sections: dict[str, Any] = {s["title"]: s for s in doc["sections"]}
		self.assertEqual(sections["Settings"]["values"]["Token"], "$TSMCTL_VENTURE_TOKEN")
		self.assertEqual(sections["Server"]["values"]["Version"], "0.6.0")
		self.assertEqual(sections["Runs"]["tables"][0]["rows"][0]["id"], 9)
		health: dict[str, Any] = next(r for r in self.fake.requests if r["path"] == "/api/v1/health")
		self.assertNotIn("Authorization", health["headers"], "health needs no credentials and gets none")
		self.assertNotIn(self.TOKEN, proc.stdout)


@unittest.skipUnless(os.environ.get("TSMCTL_TEST_VENTURE_URL"), "set TSMCTL_TEST_VENTURE_URL, TSMCTL_TEST_VENTURE_SOURCE and TSMCTL_TEST_VENTURE_TOKEN_FILE to push to a real VENTURE")
class VentureIntegrationTests(unittest.TestCase):
	"""
	Optional: push the synthetic fixture to a running VENTURE (a scratch one:
	the fixture's accounts and ledger are written into the source) and
	require a clean run. The source must have provider push and currency
	GOLD with exponent 4.
	"""

	def test_real_push(self) -> None:
		with tempfile.TemporaryDirectory(prefix="tsmctl-real-") as temp:
			env: dict[str, str] = {**os.environ, "HOME": temp, "XDG_CACHE_HOME": str(Path(temp) / "cache"), "NO_COLOR": "1", "TSMCTL_VENTURE_TOKEN_FILE": os.environ["TSMCTL_TEST_VENTURE_TOKEN_FILE"]}
			env.pop("TSMCTL_VENTURE_TOKEN", None)
			env.pop("VENTURE_TOKEN", None)
			# The operations fixture (accounts, auctions, mail), then the TSM
			# one (ledger, AuctionDB) with every option that adds lines.
			for name, writer, extra in (("ops", write_ops_fixture, []), ("tsm", write_fixture, ["--include-internal", "--market", "all"])):
				with self.subTest(fixture=name):
					wow: Path = Path(temp) / name
					writer(wow)
					cmd: list[str] = [sys.executable, str(SCRIPT), "--no-config", "--local", "--wow-dir", str(wow), "-q", "-o", "json", "venture", "push", "--wait",
						"--url", os.environ["TSMCTL_TEST_VENTURE_URL"], "--source", os.environ["TSMCTL_TEST_VENTURE_SOURCE"], *extra]
					proc: subprocess.CompletedProcess[str] = subprocess.run(cmd, capture_output=True, text=True, env=env, timeout=300)
					self.assertEqual(proc.returncode, 0, proc.stdout + proc.stderr)
					run: dict[str, Any] = next(s for s in json.loads(proc.stdout)["sections"] if s["title"] == "Run")["values"]
					self.assertEqual((run["Status"], run["Refused"]), ("ok", 0))


if __name__ == "__main__":
	unittest.main(verbosity=2)
