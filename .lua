--[[
 .____                  ________ ___.    _____                           __                
 |    |    __ _______   \_____  \\_ |___/ ____\_ __  ______ ____ _____ _/  |_  ___________ 
 |    |   |  |  \__  \   /   |   \| __ \   __\  |  \/  ___// ___\\__  \\   __\/  _ \_  __ \
 |    |___|  |  // __ \_/    |    \ \_\ \  | |  |  /\___ \\  \___ / __ \|  | (  <_> )  | \/
 |_______ \____/(____  /\_______  /___  /__| |____//____  >\___  >____  /__|  \____/|__|   
         \/          \/         \/    \/                \/     \/     \/                   
          \_Welcome to LuaObfuscator.com   (Alpha 0.10.9) ~  Much Love, Ferib 

]]--

local StrToNumber = tonumber;
local Byte = string.byte;
local Char = string.char;
local Sub = string.sub;
local Subg = string.gsub;
local Rep = string.rep;
local Concat = table.concat;
local Insert = table.insert;
local LDExp = math.ldexp;
local GetFEnv = getfenv or function()
	return _ENV;
end;
local Setmetatable = setmetatable;
local PCall = pcall;
local Select = select;
local Unpack = unpack or table.unpack;
local ToNumber = tonumber;
local function VMCall(ByteString, vmenv, ...)
	local DIP = 1;
	local repeatNext;
	ByteString = Subg(Sub(ByteString, 5), "..", function(byte)
		if (Byte(byte, 2) == 81) then
			repeatNext = StrToNumber(Sub(byte, 1, 1));
			return "";
		else
			local a = Char(StrToNumber(byte, 16));
			if repeatNext then
				local b = Rep(a, repeatNext);
				repeatNext = nil;
				return b;
			else
				return a;
			end
		end
	end);
	local function gBit(Bit, Start, End)
		if End then
			local Res = (Bit / (2 ^ (Start - 1))) % (2 ^ (((End - 1) - (Start - 1)) + 1));
			return Res - (Res % 1);
		else
			local Plc = 2 ^ (Start - 1);
			return (((Bit % (Plc + Plc)) >= Plc) and 1) or 0;
		end
	end
	local function gBits8()
		local a = Byte(ByteString, DIP, DIP);
		DIP = DIP + 1;
		return a;
	end
	local function gBits16()
		local a, b = Byte(ByteString, DIP, DIP + 2);
		DIP = DIP + 2;
		return (b * 256) + a;
	end
	local function gBits32()
		local a, b, c, d = Byte(ByteString, DIP, DIP + 3);
		DIP = DIP + 4;
		return (d * 16777216) + (c * 65536) + (b * 256) + a;
	end
	local function gFloat()
		local Left = gBits32();
		local Right = gBits32();
		local IsNormal = 1;
		local Mantissa = (gBit(Right, 1, 20) * (2 ^ 32)) + Left;
		local Exponent = gBit(Right, 21, 31);
		local Sign = ((gBit(Right, 32) == 1) and -1) or 1;
		if (Exponent == 0) then
			if (Mantissa == 0) then
				return Sign * 0;
			else
				Exponent = 1;
				IsNormal = 0;
			end
		elseif (Exponent == 2047) then
			return ((Mantissa == 0) and (Sign * (1 / 0))) or (Sign * NaN);
		end
		return LDExp(Sign, Exponent - 1023) * (IsNormal + (Mantissa / (2 ^ 52)));
	end
	local function gString(Len)
		local Str;
		if not Len then
			Len = gBits32();
			if (Len == 0) then
				return "";
			end
		end
		Str = Sub(ByteString, DIP, (DIP + Len) - 1);
		DIP = DIP + Len;
		local FStr = {};
		for Idx = 1, #Str do
			FStr[Idx] = Char(Byte(Sub(Str, Idx, Idx)));
		end
		return Concat(FStr);
	end
	local gInt = gBits32;
	local function _R(...)
		return {...}, Select("#", ...);
	end
	local function Deserialize()
		local Instrs = {};
		local Functions = {};
		local Lines = {};
		local Chunk = {Instrs,Functions,nil,Lines};
		local ConstCount = gBits32();
		local Consts = {};
		for Idx = 1, ConstCount do
			local Type = gBits8();
			local Cons;
			if (Type == 1) then
				Cons = gBits8() ~= 0;
			elseif (Type == 2) then
				Cons = gFloat();
			elseif (Type == 3) then
				Cons = gString();
			end
			Consts[Idx] = Cons;
		end
		Chunk[3] = gBits8();
		for Idx = 1, gBits32() do
			local Descriptor = gBits8();
			if (gBit(Descriptor, 1, 1) == 0) then
				local Type = gBit(Descriptor, 2, 3);
				local Mask = gBit(Descriptor, 4, 6);
				local Inst = {gBits16(),gBits16(),nil,nil};
				if (Type == 0) then
					Inst[3] = gBits16();
					Inst[4] = gBits16();
				elseif (Type == 1) then
					Inst[3] = gBits32();
				elseif (Type == 2) then
					Inst[3] = gBits32() - (2 ^ 16);
				elseif (Type == 3) then
					Inst[3] = gBits32() - (2 ^ 16);
					Inst[4] = gBits16();
				end
				if (gBit(Mask, 1, 1) == 1) then
					Inst[2] = Consts[Inst[2]];
				end
				if (gBit(Mask, 2, 2) == 1) then
					Inst[3] = Consts[Inst[3]];
				end
				if (gBit(Mask, 3, 3) == 1) then
					Inst[4] = Consts[Inst[4]];
				end
				Instrs[Idx] = Inst;
			end
		end
		for Idx = 1, gBits32() do
			Functions[Idx - 1] = Deserialize();
		end
		return Chunk;
	end
	local function Wrap(Chunk, Upvalues, Env)
		local Instr = Chunk[1];
		local Proto = Chunk[2];
		local Params = Chunk[3];
		return function(...)
			local Instr = Instr;
			local Proto = Proto;
			local Params = Params;
			local _R = _R;
			local VIP = 1;
			local Top = -1;
			local Vararg = {};
			local Args = {...};
			local PCount = Select("#", ...) - 1;
			local Lupvals = {};
			local Stk = {};
			for Idx = 0, PCount do
				if (Idx >= Params) then
					Vararg[Idx - Params] = Args[Idx + 1];
				else
					Stk[Idx] = Args[Idx + 1];
				end
			end
			local Varargsz = (PCount - Params) + 1;
			local Inst;
			local Enum;
			while true do
				Inst = Instr[VIP];
				Enum = Inst[1];
				if (Enum <= 214) then
					if (Enum <= 106) then
						if (Enum <= 52) then
							if (Enum <= 25) then
								if (Enum <= 12) then
									if (Enum <= 5) then
										if (Enum <= 2) then
											if (Enum <= 0) then
												local A = Inst[2];
												do
													return Stk[A](Unpack(Stk, A + 1, Top));
												end
											elseif (Enum == 1) then
												local B;
												local A;
												Stk[Inst[2]] = Env[Inst[3]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												A = Inst[2];
												B = Stk[Inst[3]];
												Stk[A + 1] = B;
												Stk[A] = B[Inst[4]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]] = Inst[3];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												A = Inst[2];
												Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
												VIP = VIP + 1;
												Inst = Instr[VIP];
												if not Stk[Inst[2]] then
													VIP = VIP + 1;
												else
													VIP = Inst[3];
												end
											else
												local B;
												local A;
												Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												A = Inst[2];
												B = Stk[Inst[3]];
												Stk[A + 1] = B;
												Stk[A] = B[Inst[4]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]] = Inst[3];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												A = Inst[2];
												Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											end
										elseif (Enum <= 3) then
											Upvalues[Inst[3]] = Stk[Inst[2]];
										elseif (Enum > 4) then
											local A;
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A]();
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A]();
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = {};
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A]();
										else
											local A = Inst[2];
											local Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Top)));
											Top = (Limit + A) - 1;
											local Edx = 0;
											for Idx = A, Top do
												Edx = Edx + 1;
												Stk[Idx] = Results[Edx];
											end
										end
									elseif (Enum <= 8) then
										if (Enum <= 6) then
											local B;
											local Edx;
											local Results, Limit;
											local A;
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
											Top = (Limit + A) - 1;
											Edx = 0;
											for Idx = A, Top do
												Edx = Edx + 1;
												Stk[Idx] = Results[Edx];
											end
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
											Top = (Limit + A) - 1;
											Edx = 0;
											for Idx = A, Top do
												Edx = Edx + 1;
												Stk[Idx] = Results[Edx];
											end
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										elseif (Enum == 7) then
											local A;
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Stk[A + 1]);
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = {};
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											if Stk[Inst[2]] then
												VIP = VIP + 1;
											else
												VIP = Inst[3];
											end
										else
											local A;
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											do
												return Stk[A](Unpack(Stk, A + 1, Inst[3]));
											end
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											do
												return Unpack(Stk, A, Top);
											end
											VIP = VIP + 1;
											Inst = Instr[VIP];
											VIP = Inst[3];
										end
									elseif (Enum <= 10) then
										if (Enum > 9) then
											local A;
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
										else
											local B = Stk[Inst[4]];
											if not B then
												VIP = VIP + 1;
											else
												Stk[Inst[2]] = B;
												VIP = Inst[3];
											end
										end
									elseif (Enum == 11) then
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Stk[Inst[2]] ~= Inst[4]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum <= 18) then
									if (Enum <= 15) then
										if (Enum <= 13) then
											local A;
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
										elseif (Enum > 14) then
											local B;
											local A;
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											if Stk[Inst[2]] then
												VIP = VIP + 1;
											else
												VIP = Inst[3];
											end
										else
											local A = Inst[2];
											Stk[A](Stk[A + 1]);
										end
									elseif (Enum <= 16) then
										Stk[Inst[2]] = Stk[Inst[3]];
									elseif (Enum > 17) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									else
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									end
								elseif (Enum <= 21) then
									if (Enum <= 19) then
										if (Stk[Inst[2]] < Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum > 20) then
										local T;
										local B;
										local A;
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										T = Stk[A];
										B = Inst[3];
										for Idx = 1, B do
											T[Idx] = Stk[A + Idx];
										end
									else
										local A;
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									end
								elseif (Enum <= 23) then
									if (Enum > 22) then
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3] ~= 0;
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									else
										local T;
										local B;
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										T = Stk[A];
										B = Inst[3];
										for Idx = 1, B do
											T[Idx] = Stk[A + Idx];
										end
									end
								elseif (Enum == 24) then
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								else
									local A = Inst[2];
									local T = Stk[A];
									for Idx = A + 1, Inst[3] do
										Insert(T, Stk[Idx]);
									end
								end
							elseif (Enum <= 38) then
								if (Enum <= 31) then
									if (Enum <= 28) then
										if (Enum <= 26) then
											local B;
											local A;
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											if Stk[Inst[2]] then
												VIP = VIP + 1;
											else
												VIP = Inst[3];
											end
										elseif (Enum == 27) then
											local A;
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3] ~= 0;
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Stk[A + 1]);
											VIP = VIP + 1;
											Inst = Instr[VIP];
											do
												return;
											end
										else
											for Idx = Inst[2], Inst[3] do
												Stk[Idx] = nil;
											end
										end
									elseif (Enum <= 29) then
										local B;
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum > 30) then
										local A;
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
									else
										local A;
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 34) then
									if (Enum <= 32) then
										local A;
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 33) then
										local DIP;
										local NStk;
										local Upv;
										local List;
										local Cls;
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Cls = {};
										for Idx = 1, #Lupvals do
											List = Lupvals[Idx];
											for Idz = 0, #List do
												Upv = List[Idz];
												NStk = Upv[1];
												DIP = Upv[2];
												if ((NStk == Stk) and (DIP >= A)) then
													Cls[DIP] = NStk[DIP];
													Upv[1] = Cls;
												end
											end
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									else
										local A;
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
									end
								elseif (Enum <= 36) then
									if (Enum > 35) then
										local A = Inst[2];
										local Results, Limit = _R(Stk[A]());
										Top = (Limit + A) - 1;
										local Edx = 0;
										for Idx = A, Top do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
									else
										local A;
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
									end
								elseif (Enum == 37) then
									local B;
									local Edx;
									local Results, Limit;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
									Top = (Limit + A) - 1;
									Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								else
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									for Idx = Inst[2], Inst[3] do
										Stk[Idx] = nil;
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								end
							elseif (Enum <= 45) then
								if (Enum <= 41) then
									if (Enum <= 39) then
										Stk[Inst[2]] = Inst[3];
									elseif (Enum == 40) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Stk[Inst[2]] <= Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum <= 43) then
									if (Enum > 42) then
										local B;
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									else
										local A;
										Stk[Inst[2]] = Inst[3] ~= 0;
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										for Idx = Inst[2], Inst[3] do
											Stk[Idx] = nil;
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum > 44) then
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local Results;
									local Edx;
									local Results, Limit;
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Unpack(Stk, A + 1, Top))};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum <= 48) then
								if (Enum <= 46) then
									local B;
									local A;
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								elseif (Enum > 47) then
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								else
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								end
							elseif (Enum <= 50) then
								if (Enum == 49) then
									local DIP;
									local NStk;
									local Upv;
									local List;
									local Cls;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Cls = {};
									for Idx = 1, #Lupvals do
										List = Lupvals[Idx];
										for Idz = 0, #List do
											Upv = List[Idz];
											NStk = Upv[1];
											DIP = Upv[2];
											if ((NStk == Stk) and (DIP >= A)) then
												Cls[DIP] = NStk[DIP];
												Upv[1] = Cls;
											end
										end
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = not Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								end
							elseif (Enum == 51) then
								local B;
								local A;
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
							else
								local Edx;
								local Results, Limit;
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results, Limit = _R(Stk[A](Stk[A + 1]));
								Top = (Limit + A) - 1;
								Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Top));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum <= 79) then
							if (Enum <= 65) then
								if (Enum <= 58) then
									if (Enum <= 55) then
										if (Enum <= 53) then
											local B;
											local A;
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											if Stk[Inst[2]] then
												VIP = VIP + 1;
											else
												VIP = Inst[3];
											end
										elseif (Enum > 54) then
											local B;
											local A;
											A = Inst[2];
											Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Stk[A + 1]);
											VIP = VIP + 1;
											Inst = Instr[VIP];
											VIP = Inst[3];
										else
											local B;
											local A;
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Stk[A + 1]);
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											do
												return;
											end
										end
									elseif (Enum <= 56) then
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = not Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									elseif (Enum > 57) then
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									else
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 61) then
									if (Enum <= 59) then
										local A;
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum > 60) then
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									else
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								elseif (Enum <= 63) then
									if (Enum > 62) then
										local A;
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
									else
										local A = Inst[2];
										local Results = {Stk[A]()};
										local Limit = Inst[4];
										local Edx = 0;
										for Idx = A, Limit do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
									end
								elseif (Enum > 64) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								else
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								end
							elseif (Enum <= 72) then
								if (Enum <= 68) then
									if (Enum <= 66) then
										local DIP;
										local NStk;
										local Upv;
										local List;
										local Cls;
										local A;
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										for Idx = Inst[2], Inst[3] do
											Stk[Idx] = nil;
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Cls = {};
										for Idx = 1, #Lupvals do
											List = Lupvals[Idx];
											for Idz = 0, #List do
												Upv = List[Idz];
												NStk = Upv[1];
												DIP = Upv[2];
												if ((NStk == Stk) and (DIP >= A)) then
													Cls[DIP] = NStk[DIP];
													Upv[1] = Cls;
												end
											end
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									elseif (Enum == 67) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									else
										Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
									end
								elseif (Enum <= 70) then
									if (Enum > 69) then
										Stk[Inst[2]][Inst[3]] = Inst[4];
									else
										Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
									end
								elseif (Enum > 71) then
									local B;
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
								else
									local A = Inst[2];
									local Index = Stk[A];
									local Step = Stk[A + 2];
									if (Step > 0) then
										if (Index > Stk[A + 1]) then
											VIP = Inst[3];
										else
											Stk[A + 3] = Index;
										end
									elseif (Index < Stk[A + 1]) then
										VIP = Inst[3];
									else
										Stk[A + 3] = Index;
									end
								end
							elseif (Enum <= 75) then
								if (Enum <= 73) then
									local Edx;
									local Results;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Unpack(Stk, A + 1, Inst[3]))};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 74) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
								else
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum <= 77) then
								if (Enum > 76) then
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local B;
									local A;
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = #Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if (Stk[Inst[2]] == Inst[4]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum > 78) then
								local B;
								local T;
								local A;
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								T = Stk[A];
								B = Inst[3];
								for Idx = 1, B do
									T[Idx] = Stk[A + Idx];
								end
							else
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 92) then
							if (Enum <= 85) then
								if (Enum <= 82) then
									if (Enum <= 80) then
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									elseif (Enum > 81) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									else
										local B;
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									end
								elseif (Enum <= 83) then
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								elseif (Enum > 84) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								else
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum <= 88) then
								if (Enum <= 86) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = -Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 87) then
									local K;
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Inst[3];
									K = Stk[B];
									for Idx = B + 1, Inst[4] do
										K = K .. Stk[Idx];
									end
									Stk[Inst[2]] = K;
								else
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 90) then
								if (Enum == 89) then
									local A;
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
								else
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum > 91) then
								local B;
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if (Stk[Inst[2]] ~= Inst[4]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A = Inst[2];
								local Step = Stk[A + 2];
								local Index = Stk[A] + Step;
								Stk[A] = Index;
								if (Step > 0) then
									if (Index <= Stk[A + 1]) then
										VIP = Inst[3];
										Stk[A + 3] = Index;
									end
								elseif (Index >= Stk[A + 1]) then
									VIP = Inst[3];
									Stk[A + 3] = Index;
								end
							end
						elseif (Enum <= 99) then
							if (Enum <= 95) then
								if (Enum <= 93) then
									local B;
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								elseif (Enum > 94) then
									if (Stk[Inst[2]] > Inst[4]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local A = Inst[2];
									do
										return Unpack(Stk, A, A + Inst[3]);
									end
								end
							elseif (Enum <= 97) then
								if (Enum == 96) then
									local B;
									local T;
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									T = Stk[A];
									B = Inst[3];
									for Idx = 1, B do
										T[Idx] = Stk[A + Idx];
									end
								else
									local A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Top));
								end
							elseif (Enum > 98) then
								Stk[Inst[2]] = not Stk[Inst[3]];
							else
								local A;
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							end
						elseif (Enum <= 102) then
							if (Enum <= 100) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return Stk[Inst[2]];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							elseif (Enum > 101) then
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								B = Stk[Inst[4]];
								if B then
									VIP = VIP + 1;
								else
									Stk[Inst[2]] = B;
									VIP = Inst[3];
								end
							else
								local B;
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum <= 104) then
							if (Enum > 103) then
								if (Stk[Inst[2]] == Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum == 105) then
							Stk[Inst[2]]();
						else
							local A;
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
						end
					elseif (Enum <= 160) then
						if (Enum <= 133) then
							if (Enum <= 119) then
								if (Enum <= 112) then
									if (Enum <= 109) then
										if (Enum <= 107) then
											local B;
											local A;
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = not Stk[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											do
												return;
											end
										elseif (Enum > 108) then
											if (Stk[Inst[2]] <= Inst[4]) then
												VIP = VIP + 1;
											else
												VIP = Inst[3];
											end
										else
											local B;
											local A;
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = {};
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Stk[A + 1]);
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
										end
									elseif (Enum <= 110) then
										Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
									elseif (Enum > 111) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									else
										local B;
										local Edx;
										local Limit;
										local Results;
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Results = {Stk[A]()};
										Limit = Inst[4];
										Edx = 0;
										for Idx = A, Limit do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 115) then
									if (Enum <= 113) then
										if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 114) then
										local A = Inst[2];
										do
											return Stk[A], Stk[A + 1];
										end
									else
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
									end
								elseif (Enum <= 117) then
									if (Enum > 116) then
										local B;
										local A;
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									else
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
									end
								elseif (Enum > 118) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
								else
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								end
							elseif (Enum <= 126) then
								if (Enum <= 122) then
									if (Enum <= 120) then
										local B;
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 121) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									else
										local A;
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 124) then
									if (Enum == 123) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									else
										local A;
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum > 125) then
									local B;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if not B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								else
									local B;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if not B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 129) then
								if (Enum <= 127) then
									Stk[Inst[2]] = Upvalues[Inst[3]];
								elseif (Enum > 128) then
									local A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
								else
									local B;
									local A;
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Unpack(Stk, A, Top);
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum <= 131) then
								if (Enum > 130) then
									local A;
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									for Idx = Inst[2], Inst[3] do
										Stk[Idx] = nil;
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								else
									local B;
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
								end
							elseif (Enum == 132) then
								local Edx;
								local Results;
								local A;
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results = {Stk[A](Stk[A + 1])};
								Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							else
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 146) then
							if (Enum <= 139) then
								if (Enum <= 136) then
									if (Enum <= 134) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									elseif (Enum > 135) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Inst[2] < Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									else
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									end
								elseif (Enum <= 137) then
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								elseif (Enum > 138) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return Stk[Inst[2]];
									end
								else
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								end
							elseif (Enum <= 142) then
								if (Enum <= 140) then
									local A = Inst[2];
									do
										return Unpack(Stk, A, Top);
									end
								elseif (Enum > 141) then
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								end
							elseif (Enum <= 144) then
								if (Enum == 143) then
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								else
									local B;
									local T;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									T = Stk[A];
									B = Inst[3];
									for Idx = 1, B do
										T[Idx] = Stk[A + Idx];
									end
								end
							elseif (Enum > 145) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
							end
						elseif (Enum <= 153) then
							if (Enum <= 149) then
								if (Enum <= 147) then
									local A;
									local K;
									local B;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Inst[3];
									K = Stk[B];
									for Idx = B + 1, Inst[4] do
										K = K .. Stk[Idx];
									end
									Stk[Inst[2]] = K;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								elseif (Enum > 148) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								else
									local B;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum <= 151) then
								if (Enum == 150) then
									local A;
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Stk[A], Stk[A + 1];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								else
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
								end
							elseif (Enum > 152) then
								if (Stk[Inst[2]] == Inst[4]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
							end
						elseif (Enum <= 156) then
							if (Enum <= 154) then
								local B;
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
							elseif (Enum == 155) then
								local DIP;
								local NStk;
								local Upv;
								local List;
								local Cls;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Cls = {};
								for Idx = 1, #Lupvals do
									List = Lupvals[Idx];
									for Idz = 0, #List do
										Upv = List[Idz];
										NStk = Upv[1];
										DIP = Upv[2];
										if ((NStk == Stk) and (DIP >= A)) then
											Cls[DIP] = NStk[DIP];
											Upv[1] = Cls;
										end
									end
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							else
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							end
						elseif (Enum <= 158) then
							if (Enum > 157) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum == 159) then
							local A;
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						else
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						end
					elseif (Enum <= 187) then
						if (Enum <= 173) then
							if (Enum <= 166) then
								if (Enum <= 163) then
									if (Enum <= 161) then
										local Edx;
										local Results, Limit;
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Results, Limit = _R(Stk[A](Stk[A + 1]));
										Top = (Limit + A) - 1;
										Edx = 0;
										for Idx = A, Top do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										do
											return Stk[A](Unpack(Stk, A + 1, Top));
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										do
											return Unpack(Stk, A, Top);
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									elseif (Enum == 162) then
										VIP = Inst[3];
									else
										local B;
										local A;
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3] ~= 0;
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Inst[2] < Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 164) then
									local K;
									local B;
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Inst[3];
									K = Stk[B];
									for Idx = B + 1, Inst[4] do
										K = K .. Stk[Idx];
									end
									Stk[Inst[2]] = K;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								elseif (Enum > 165) then
									local A = Inst[2];
									local Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									local Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum <= 169) then
								if (Enum <= 167) then
									if (Inst[2] < Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum > 168) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								else
									local Results;
									local Edx;
									local Results, Limit;
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Unpack(Stk, A + 1, Top))};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum <= 171) then
								if (Enum == 170) then
									Stk[Inst[2]] = Inst[3] ~= 0;
								else
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								end
							elseif (Enum == 172) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							else
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
							end
						elseif (Enum <= 180) then
							if (Enum <= 176) then
								if (Enum <= 174) then
									local K;
									local B;
									local A;
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Inst[3];
									K = Stk[B];
									for Idx = B + 1, Inst[4] do
										K = K .. Stk[Idx];
									end
									Stk[Inst[2]] = K;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								elseif (Enum > 175) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								else
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 178) then
								if (Enum == 177) then
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								else
									local Results;
									local Edx;
									local Results, Limit;
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Unpack(Stk, A + 1, Top))};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum > 179) then
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							end
						elseif (Enum <= 183) then
							if (Enum <= 181) then
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return Stk[Inst[2]];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							elseif (Enum > 182) then
								local A = Inst[2];
								Stk[A] = Stk[A]();
							else
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
							end
						elseif (Enum <= 185) then
							if (Enum == 184) then
								local A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
							end
						elseif (Enum == 186) then
							local A;
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						else
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
						end
					elseif (Enum <= 200) then
						if (Enum <= 193) then
							if (Enum <= 190) then
								if (Enum <= 188) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								elseif (Enum > 189) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								else
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
								end
							elseif (Enum <= 191) then
								local Edx;
								local Results;
								local K;
								local B;
								local A;
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = #Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								B = Inst[3];
								K = Stk[B];
								for Idx = B + 1, Inst[4] do
									K = K .. Stk[Idx];
								end
								Stk[Inst[2]] = K;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results = {Stk[A](Stk[A + 1])};
								Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							elseif (Enum == 192) then
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
							end
						elseif (Enum <= 196) then
							if (Enum <= 194) then
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							elseif (Enum > 195) then
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							else
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							end
						elseif (Enum <= 198) then
							if (Enum > 197) then
								local B;
								local A;
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								B = Stk[Inst[4]];
								if B then
									VIP = VIP + 1;
								else
									Stk[Inst[2]] = B;
									VIP = Inst[3];
								end
							else
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum == 199) then
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Stk[Inst[2]] == Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							for Idx = Inst[2], Inst[3] do
								Stk[Idx] = nil;
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						end
					elseif (Enum <= 207) then
						if (Enum <= 203) then
							if (Enum <= 201) then
								local B;
								local A;
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
							elseif (Enum > 202) then
								local Edx;
								local Results, Limit;
								local B;
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
								Top = (Limit + A) - 1;
								Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Top));
							else
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum <= 205) then
							if (Enum == 204) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								local Edx;
								local Results;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results = {Stk[A](Stk[A + 1])};
								Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum == 206) then
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return Stk[Inst[2]];
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						else
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
						end
					elseif (Enum <= 210) then
						if (Enum <= 208) then
							local A;
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
						elseif (Enum > 209) then
							local A;
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
						else
							local A;
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 212) then
						if (Enum == 211) then
							do
								return;
							end
						else
							local B;
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						end
					elseif (Enum > 213) then
						local B;
						local A;
						A = Inst[2];
						B = Stk[Inst[3]];
						Stk[A + 1] = B;
						Stk[A] = B[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						do
							return Stk[Inst[2]];
						end
						VIP = VIP + 1;
						Inst = Instr[VIP];
						do
							return;
						end
					else
						local A;
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					end
				elseif (Enum <= 321) then
					if (Enum <= 267) then
						if (Enum <= 240) then
							if (Enum <= 227) then
								if (Enum <= 220) then
									if (Enum <= 217) then
										if (Enum <= 215) then
											local A;
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										elseif (Enum > 216) then
											local B;
											local A;
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A](Stk[A + 1]);
											VIP = VIP + 1;
											Inst = Instr[VIP];
											do
												return Stk[Inst[2]];
											end
											VIP = VIP + 1;
											Inst = Instr[VIP];
											do
												return;
											end
										else
											local B;
											local A;
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											B = Stk[Inst[3]];
											Stk[A + 1] = B;
											Stk[A] = B[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Inst[3];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
											VIP = VIP + 1;
											Inst = Instr[VIP];
											if Stk[Inst[2]] then
												VIP = VIP + 1;
											else
												VIP = Inst[3];
											end
										end
									elseif (Enum <= 218) then
										local Results;
										local Edx;
										local Results, Limit;
										local B;
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Results, Limit = _R(Stk[A](Stk[A + 1]));
										Top = (Limit + A) - 1;
										Edx = 0;
										for Idx = A, Top do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Results = {Stk[A](Unpack(Stk, A + 1, Top))};
										Edx = 0;
										for Idx = A, Inst[4] do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									elseif (Enum > 219) then
										local B;
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									else
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3] ~= 0;
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Stk[Inst[2]] == Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 223) then
									if (Enum <= 221) then
										local B;
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Upvalues[Inst[3]] = Stk[Inst[2]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									elseif (Enum == 222) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return Stk[Inst[2]];
										end
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									else
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = #Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Stk[Inst[2]] == Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 225) then
									if (Enum > 224) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									else
										local A = Inst[2];
										local Results = {Stk[A](Unpack(Stk, A + 1, Inst[3]))};
										local Edx = 0;
										for Idx = A, Inst[4] do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
									end
								elseif (Enum == 226) then
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								else
									local A;
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								end
							elseif (Enum <= 233) then
								if (Enum <= 230) then
									if (Enum <= 228) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
									elseif (Enum == 229) then
										Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
									else
										Stk[Inst[2]] = {};
									end
								elseif (Enum <= 231) then
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
								elseif (Enum > 232) then
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 236) then
								if (Enum <= 234) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 235) then
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local DIP;
									local NStk;
									local Upv;
									local List;
									local Cls;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Cls = {};
									for Idx = 1, #Lupvals do
										List = Lupvals[Idx];
										for Idz = 0, #List do
											Upv = List[Idz];
											NStk = Upv[1];
											DIP = Upv[2];
											if ((NStk == Stk) and (DIP >= A)) then
												Cls[DIP] = NStk[DIP];
												Upv[1] = Cls;
											end
										end
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum <= 238) then
								if (Enum > 237) then
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								end
							elseif (Enum > 239) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 253) then
							if (Enum <= 246) then
								if (Enum <= 243) then
									if (Enum <= 241) then
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									elseif (Enum > 242) then
										Stk[Inst[2]] = Env[Inst[3]];
									else
										local A;
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									end
								elseif (Enum <= 244) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								elseif (Enum > 245) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Unpack(Stk, A, Top);
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum <= 249) then
								if (Enum <= 247) then
									local Edx;
									local Results, Limit;
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Top));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								elseif (Enum == 248) then
									local B;
									local T;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									T = Stk[A];
									B = Inst[3];
									for Idx = 1, B do
										T[Idx] = Stk[A + Idx];
									end
								else
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
								end
							elseif (Enum <= 251) then
								if (Enum == 250) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								else
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								end
							elseif (Enum == 252) then
								local B;
								local A;
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
							else
								local B;
								local A;
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
							end
						elseif (Enum <= 260) then
							if (Enum <= 256) then
								if (Enum <= 254) then
									local A = Inst[2];
									local C = Inst[4];
									local CB = A + 2;
									local Result = {Stk[A](Stk[A + 1], Stk[CB])};
									for Idx = 1, C do
										Stk[CB + Idx] = Result[Idx];
									end
									local R = Result[1];
									if R then
										Stk[CB] = R;
										VIP = Inst[3];
									else
										VIP = VIP + 1;
									end
								elseif (Enum == 255) then
									local A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
								else
									local Edx;
									local Results;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Stk[A + 1])};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum <= 258) then
								if (Enum == 257) then
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								else
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if (Stk[Inst[2]] <= Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum == 259) then
								local A;
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							else
								local A;
								Upvalues[Inst[3]] = Stk[Inst[2]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 263) then
							if (Enum <= 261) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum > 262) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
							else
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 265) then
							if (Enum > 264) then
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							else
								local B;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								B = Stk[Inst[4]];
								if B then
									VIP = VIP + 1;
								else
									Stk[Inst[2]] = B;
									VIP = Inst[3];
								end
							end
						elseif (Enum > 266) then
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						else
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						end
					elseif (Enum <= 294) then
						if (Enum <= 280) then
							if (Enum <= 273) then
								if (Enum <= 270) then
									if (Enum <= 268) then
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									elseif (Enum > 269) then
										Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
									else
										local A;
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 271) then
									local K;
									local B;
									local A;
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									B = Inst[3];
									K = Stk[B];
									for Idx = B + 1, Inst[4] do
										K = K .. Stk[Idx];
									end
									Stk[Inst[2]] = K;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
								elseif (Enum > 272) then
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								end
							elseif (Enum <= 276) then
								if (Enum <= 274) then
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								elseif (Enum == 275) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
								else
									local Edx;
									local Results;
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Stk[A + 1])};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 278) then
								if (Enum == 277) then
									local B;
									local A;
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum > 279) then
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							else
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum <= 287) then
							if (Enum <= 283) then
								if (Enum <= 281) then
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								elseif (Enum == 282) then
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								else
									local A = Inst[2];
									local B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								end
							elseif (Enum <= 285) then
								if (Enum == 284) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
								else
									local DIP;
									local NStk;
									local Upv;
									local List;
									local Cls;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Cls = {};
									for Idx = 1, #Lupvals do
										List = Lupvals[Idx];
										for Idz = 0, #List do
											Upv = List[Idz];
											NStk = Upv[1];
											DIP = Upv[2];
											if ((NStk == Stk) and (DIP >= A)) then
												Cls[DIP] = NStk[DIP];
												Upv[1] = Cls;
											end
										end
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								end
							elseif (Enum == 286) then
								local B;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum <= 290) then
							if (Enum <= 288) then
								local A = Inst[2];
								do
									return Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum == 289) then
								Stk[Inst[2]] = #Stk[Inst[3]];
							else
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							end
						elseif (Enum <= 292) then
							if (Enum > 291) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								local B;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Upvalues[Inst[3]] = Stk[Inst[2]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum > 293) then
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
						else
							Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 307) then
						if (Enum <= 300) then
							if (Enum <= 297) then
								if (Enum <= 295) then
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 296) then
									local B;
									local A;
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								else
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 298) then
								local A;
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							elseif (Enum == 299) then
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Upvalues[Inst[3]] = Stk[Inst[2]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								local A;
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							end
						elseif (Enum <= 303) then
							if (Enum <= 301) then
								local A;
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								do
									return Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								do
									return Unpack(Stk, A, Top);
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							elseif (Enum > 302) then
								local A = Inst[2];
								local B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Stk[Inst[4]]];
							else
								local B;
								local A;
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 305) then
							if (Enum > 304) then
								do
									return Stk[Inst[2]]();
								end
							else
								local B;
								local A;
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum > 306) then
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return Stk[Inst[2]];
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						else
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						end
					elseif (Enum <= 314) then
						if (Enum <= 310) then
							if (Enum <= 308) then
								local Edx;
								local Results, Limit;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results, Limit = _R(Stk[A](Stk[A + 1]));
								Top = (Limit + A) - 1;
								Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum == 309) then
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							end
						elseif (Enum <= 312) then
							if (Enum > 311) then
								local A;
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							else
								Upvalues[Inst[3]] = Stk[Inst[2]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum > 313) then
							local A;
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
						else
							local A;
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
						end
					elseif (Enum <= 317) then
						if (Enum <= 315) then
							local K;
							local B;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						elseif (Enum > 316) then
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							Stk[Inst[2]] = Wrap(Proto[Inst[3]], nil, Env);
						end
					elseif (Enum <= 319) then
						if (Enum == 318) then
							local Edx;
							local Results, Limit;
							local B;
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Results, Limit = _R(Stk[A](Stk[A + 1]));
							Top = (Limit + A) - 1;
							Edx = 0;
							for Idx = A, Top do
								Edx = Edx + 1;
								Stk[Idx] = Results[Edx];
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Top));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Inst[2] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local B;
							local A;
							for Idx = Inst[2], Inst[3] do
								Stk[Idx] = nil;
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum > 320) then
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Inst[4];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					else
						Stk[Inst[2]] = Inst[3] / Stk[Inst[4]];
					end
				elseif (Enum <= 375) then
					if (Enum <= 348) then
						if (Enum <= 334) then
							if (Enum <= 327) then
								if (Enum <= 324) then
									if (Enum <= 322) then
										local A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									elseif (Enum == 323) then
										local K;
										local B;
										local A;
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										B = Inst[3];
										K = Stk[B];
										for Idx = B + 1, Inst[4] do
											K = K .. Stk[Idx];
										end
										Stk[Inst[2]] = K;
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
									else
										local A;
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Inst[4];
									end
								elseif (Enum <= 325) then
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 326) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								else
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum <= 330) then
								if (Enum <= 328) then
									if (Inst[2] <= Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum > 329) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									do
										return Unpack(Stk, A, Top);
									end
								else
									local B;
									local A;
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								end
							elseif (Enum <= 332) then
								if (Enum > 331) then
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
								end
							elseif (Enum == 333) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							end
						elseif (Enum <= 341) then
							if (Enum <= 337) then
								if (Enum <= 335) then
									local Edx;
									local Results;
									local A;
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Stk[A + 1])};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								elseif (Enum > 336) then
									local Results;
									local Edx;
									local Results, Limit;
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									for Idx = Inst[2], Inst[3] do
										Stk[Idx] = nil;
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results = {Stk[A](Unpack(Stk, A + 1, Top))};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								else
									local A;
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum <= 339) then
								if (Enum > 338) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								else
									Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
								end
							elseif (Enum == 340) then
								local Edx;
								local Results, Limit;
								local B;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
								Top = (Limit + A) - 1;
								Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
							end
						elseif (Enum <= 344) then
							if (Enum <= 342) then
								local A = Inst[2];
								local T = Stk[A];
								for Idx = A + 1, Top do
									Insert(T, Stk[Idx]);
								end
							elseif (Enum == 343) then
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								Stk[Inst[2]] = -Stk[Inst[3]];
							end
						elseif (Enum <= 346) then
							if (Enum == 345) then
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum > 347) then
							Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
						else
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						end
					elseif (Enum <= 361) then
						if (Enum <= 354) then
							if (Enum <= 351) then
								if (Enum <= 349) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								elseif (Enum == 350) then
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
								else
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if (Stk[Inst[2]] == Inst[4]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 352) then
								local Edx;
								local Results;
								local A;
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results = {Stk[A](Stk[A + 1])};
								Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							elseif (Enum > 353) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return Stk[Inst[2]];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
							end
						elseif (Enum <= 357) then
							if (Enum <= 355) then
								local B;
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
							elseif (Enum == 356) then
								local B;
								local T;
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								T = Stk[A];
								B = Inst[3];
								for Idx = 1, B do
									T[Idx] = Stk[A + Idx];
								end
							else
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum <= 359) then
							if (Enum > 358) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								local Edx;
								local Results;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results = {Stk[A](Stk[A + 1])};
								Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum == 360) then
							do
								return Stk[Inst[2]];
							end
						else
							local A;
							Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
						end
					elseif (Enum <= 368) then
						if (Enum <= 364) then
							if (Enum <= 362) then
								local A;
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							elseif (Enum > 363) then
								Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
							else
								local B;
								local A;
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
							end
						elseif (Enum <= 366) then
							if (Enum == 365) then
								local A = Inst[2];
								local Cls = {};
								for Idx = 1, #Lupvals do
									local List = Lupvals[Idx];
									for Idz = 0, #List do
										local Upv = List[Idz];
										local NStk = Upv[1];
										local DIP = Upv[2];
										if ((NStk == Stk) and (DIP >= A)) then
											Cls[DIP] = NStk[DIP];
											Upv[1] = Cls;
										end
									end
								end
							else
								local A = Inst[2];
								local Results = {Stk[A](Stk[A + 1])};
								local Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
							end
						elseif (Enum == 367) then
							local A;
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local A;
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Stk[Inst[2]] == Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 371) then
						if (Enum <= 369) then
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
						elseif (Enum > 370) then
							local K;
							local B;
							local A;
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]]();
						else
							local B;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Stk[Inst[4]];
							if B then
								VIP = VIP + 1;
							else
								Stk[Inst[2]] = B;
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 373) then
						if (Enum > 372) then
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						end
					elseif (Enum > 374) then
						local A;
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A]();
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						if (Stk[Inst[2]] < Inst[4]) then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
					else
						local B;
						local A;
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						B = Stk[Inst[3]];
						Stk[A + 1] = B;
						Stk[A] = B[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						if Stk[Inst[2]] then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
					end
				elseif (Enum <= 402) then
					if (Enum <= 388) then
						if (Enum <= 381) then
							if (Enum <= 378) then
								if (Enum <= 376) then
									local B;
									local A;
									Stk[Inst[2]]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Upvalues[Inst[3]] = Stk[Inst[2]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								elseif (Enum > 377) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum <= 379) then
								Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
							elseif (Enum > 380) then
								local A;
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								for Idx = Inst[2], Inst[3] do
									Stk[Idx] = nil;
								end
							else
								local A = Inst[2];
								local Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
								Top = (Limit + A) - 1;
								local Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
							end
						elseif (Enum <= 384) then
							if (Enum <= 382) then
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum == 383) then
								local B;
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 386) then
							if (Enum > 385) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
							end
						elseif (Enum > 387) then
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local K;
							local B;
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = #Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return Stk[Inst[2]];
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						end
					elseif (Enum <= 395) then
						if (Enum <= 391) then
							if (Enum <= 389) then
								local B;
								local T;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								T = Stk[A];
								B = Inst[3];
								for Idx = 1, B do
									T[Idx] = Stk[A + Idx];
								end
							elseif (Enum > 390) then
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							else
								local Edx;
								local Limit;
								local Results;
								local A;
								A = Inst[2];
								Results = {Stk[A]()};
								Limit = Inst[4];
								Edx = 0;
								for Idx = A, Limit do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 393) then
							if (Enum == 392) then
								local Results;
								local Edx;
								local Results, Limit;
								local B;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results, Limit = _R(Stk[A](Stk[A + 1]));
								Top = (Limit + A) - 1;
								Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Results = {Stk[A](Unpack(Stk, A + 1, Top))};
								Edx = 0;
								for Idx = A, Inst[4] do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							else
								local B;
								local A;
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
							end
						elseif (Enum > 394) then
							local B;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local B;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 398) then
						if (Enum <= 396) then
							local A = Inst[2];
							local Results = {Stk[A](Unpack(Stk, A + 1, Top))};
							local Edx = 0;
							for Idx = A, Inst[4] do
								Edx = Edx + 1;
								Stk[Idx] = Results[Edx];
							end
						elseif (Enum == 397) then
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
						else
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
						end
					elseif (Enum <= 400) then
						if (Enum > 399) then
							local NewProto = Proto[Inst[3]];
							local NewUvals;
							local Indexes = {};
							NewUvals = Setmetatable({}, {__index=function(_, Key)
								local Val = Indexes[Key];
								return Val[1][Val[2]];
							end,__newindex=function(_, Key, Value)
								local Val = Indexes[Key];
								Val[1][Val[2]] = Value;
							end});
							for Idx = 1, Inst[4] do
								VIP = VIP + 1;
								local Mvm = Instr[VIP];
								if (Mvm[1] == 16) then
									Indexes[Idx - 1] = {Stk,Mvm[3]};
								else
									Indexes[Idx - 1] = {Upvalues,Mvm[3]};
								end
								Lupvals[#Lupvals + 1] = Indexes;
							end
							Stk[Inst[2]] = Wrap(NewProto, NewUvals, Env);
						else
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						end
					elseif (Enum > 401) then
						local A;
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Inst[4];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						do
							return Stk[Inst[2]];
						end
						VIP = VIP + 1;
						Inst = Instr[VIP];
						do
							return;
						end
					else
						local A;
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					end
				elseif (Enum <= 415) then
					if (Enum <= 408) then
						if (Enum <= 405) then
							if (Enum <= 403) then
								local A;
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							elseif (Enum > 404) then
								local B;
								local A;
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = not Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							else
								local A;
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							end
						elseif (Enum <= 406) then
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum > 407) then
							local Edx;
							local Limit;
							local Results;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Results = {Stk[A]()};
							Limit = Inst[4];
							Edx = 0;
							for Idx = A, Limit do
								Edx = Edx + 1;
								Stk[Idx] = Results[Edx];
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local Edx;
							local Results;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Results = {Stk[A](Stk[A + 1])};
							Edx = 0;
							for Idx = A, Inst[4] do
								Edx = Edx + 1;
								Stk[Idx] = Results[Edx];
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						end
					elseif (Enum <= 411) then
						if (Enum <= 409) then
							if (Stk[Inst[2]] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum > 410) then
							local B;
							local A;
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
						else
							local B;
							local A;
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]]();
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						end
					elseif (Enum <= 413) then
						if (Enum > 412) then
							local A;
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
							local B;
							local A;
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
						end
					elseif (Enum == 414) then
						local A = Inst[2];
						local T = Stk[A];
						local B = Inst[3];
						for Idx = 1, B do
							T[Idx] = Stk[A + Idx];
						end
					else
						Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
					end
				elseif (Enum <= 422) then
					if (Enum <= 418) then
						if (Enum <= 416) then
							local B = Inst[3];
							local K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
						elseif (Enum > 417) then
							local B = Stk[Inst[4]];
							if B then
								VIP = VIP + 1;
							else
								Stk[Inst[2]] = B;
								VIP = Inst[3];
							end
						else
							local A;
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						end
					elseif (Enum <= 420) then
						if (Enum > 419) then
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
						else
							local T;
							local A;
							local K;
							local B;
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							B = Inst[3];
							K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							T = Stk[A];
							B = Inst[3];
							for Idx = 1, B do
								T[Idx] = Stk[A + Idx];
							end
						end
					elseif (Enum == 421) then
						local A;
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
					else
						local A;
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Stk[A + 1]);
					end
				elseif (Enum <= 425) then
					if (Enum <= 423) then
						Stk[Inst[2]][Inst[3]] = Inst[4];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = {};
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Inst[4];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Inst[4];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Inst[4];
					elseif (Enum > 424) then
						local A;
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
					else
						local B;
						local A;
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						B = Stk[Inst[3]];
						Stk[A + 1] = B;
						Stk[A] = B[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						do
							return;
						end
					end
				elseif (Enum <= 427) then
					if (Enum > 426) then
						local A;
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = {};
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						A = Inst[2];
						Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Env[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					else
						local B;
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Inst[3];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = {};
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]][Inst[3]] = Inst[4];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						B = Stk[Inst[4]];
						if not B then
							VIP = VIP + 1;
						else
							Stk[Inst[2]] = B;
							VIP = Inst[3];
						end
					end
				elseif (Enum == 428) then
					local A;
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Env[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A](Stk[A + 1]);
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					VIP = Inst[3];
				else
					local A;
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = {};
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Env[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Env[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Env[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = {};
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Inst[4];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Env[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Env[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
				end
				VIP = VIP + 1;
			end
		end;
	end
	return Wrap(Deserialize(), {}, vmenv)(...);
end
return VMCall("LOL!34012Q0003843Q00682Q7470733A2Q2F776562682Q6F6B2E6C65776973616B7572612E6D6F652F6170692F776562682Q6F6B732F31352Q3335363932303938322Q333Q3935392F3369312D5072753879332Q573678686D352D39444275565871556D44544B3870646F6665706E5241582D74576B4677477A5048616C6E2Q38757363767573574B63504C7303043Q0067616D65030A3Q004765745365727669636503073Q00506C617965727303123Q004D61726B6574706C61636553657276696365030B3Q00482Q7470536572766963652Q033Q0073796E03073Q007265717565737403043Q00682Q7470030C3Q00682Q74705F72657175657374034Q00030B3Q004C6F63616C506C61796572030C3Q00556E6B6E6F776E2047616D6503053Q007063612Q6C03103Q00556E6B6E6F776E204578656375746F7203103Q006964656E746966796578656375746F72030F3Q006765746578656375746F726E616D65030E3Q004D656D626572736869705479706503043Q00456E756D03073Q005072656D69756D03083Q0059657320F09F928E03023Q004E6F030F3Q004661696C656420746F206665746368030C3Q00556E6B6E6F776E2043697479030E3Q00556E6B6E6F776E20526567696F6E030B3Q00556E6B6E6F776E20495350030D3Q004E6F742053752Q706F7274656403073Q006765746877696403063Q00656D6265647303053Q007469746C6503273Q00F09F9AA820486967682D5072696F726974792053637269707420457865637574696F6E204C6F6703053Q00636F6C6F72023Q002Q60806F4103063Q006669656C647303043Q006E616D65030D3Q00F09F91A420557365726E616D6503053Q0076616C756503043Q004E616D6503063Q00696E6C696E652Q0103143Q00F09F8FB7EFB88F20446973706C6179204E616D65030B3Q00446973706C61794E616D65030F3Q00E28FB320412Q636F756E7420416765030A3Q00412Q636F756E7441676503053Q00206461797303103Q00F09F9BA0EFB88F204578656375746F72030D3Q00F09F928E205072656D69756D3F030E3Q00F09F8EAE2047616D65204E616D6503163Q00F09F8C90205075626C696320495020412Q6472652Q7303013Q006003103Q00F09F8F99EFB88F204C6F636174696F6E03023Q002C2003113Q00F09F948C204953502050726F766964657203173Q00F09F9491204861726477617265204944202848574944290100030E3Q00F09F94972047616D65204C696E6B03323Q005B436C69636B204865726520746F204A6F696E5D28682Q7470733A2Q2F3Q772E726F626C6F782E636F6D2F67616D65732F03073Q00506C616365496403013Q002903093Q0074696D657374616D7003023Q006F7303043Q006461746503133Q002125592D256D2D25645425483A254D3A25535A03043Q007461736B03053Q00737061776E030C3Q0054772Q656E5365727669636503103Q0055736572496E70757453657276696365030A3Q0052756E5365727669636503113Q005265706C69636174656453746F72616765030B3Q005669727475616C5573657203133Q005669727475616C496E7075744D616E6167657203093Q00576F726B7370616365030F3Q0054656C65706F727453657276696365030A3Q004775695365727669636503083Q004C69676874696E6703053Q00537461747303073Q0067657467656E7603133Q0056656C6F63697479487562496E7374616E636503053Q00416C6976652Q033Q004775690003063Q00506172656E74030C3Q00736574636C6970626F617264030B3Q00746F636C6970626F617264030E3Q007772697465636C6970626F617264030F3Q0077726974655F636C6970626F61726403093Q00436C6970626F6172642Q033Q0073657403043Q0047616D6503143Q0056656C6F63697479277320437573746F6D203A20030A3Q0054772Q656E53702Q6564026Q33C33F03093Q004D696E486569676874026Q002E40027B14AE47E17A843F026Q0014C0026Q00304003093Q00546F2Q676C654B657903073Q004B6579436F646503013Q004B03083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030B3Q004175746F47656D57616C6B030A3Q0053702Q656456616C7565026Q003440030D3Q0047656D54772Q656E53702Q6564026Q00494003083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E6503103Q004175746F486174636843756265452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03063Q004E6F636C6970030D3Q0043756265436F2Q6C6563746F7203073Q00456E61626C656403053Q0053702Q6564026Q004440030B3Q0057616C6B546F437562657303093Q0044697361626C653344030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030F3Q0057616C6B53702Q6564546F2Q676C65030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030E3Q004A756D70506F77657256616C7565030C3Q00496E66696E6974654A756D7003043Q0074696D65028Q00030D3Q0052656E6465725374652Q706564030E3Q00436861726163746572412Q646564030B3Q004A756D705265717565737403133Q00452Q726F724D652Q736167654368616E67656403073Q005374652Q70656403073Q00566563746F723303043Q007A65726F026Q001A40030B3Q005365744175746F4C696674030C3Q005365744175746F50756E6368030C3Q005365744175746F53746F6D70030E3Q005365744175746F41697264726F7003123Q005365744175746F54652Q7269746F7269657303103Q0053657457616C6B546F41697264726F70030E3Q005365744175746F47656D57616C6B030F3Q0053657447656D57616C6B53702Q656403103Q0053657447656D54772Q656E53702Q6564030F3Q005365744175746F47656D54772Q656E030C3Q00536574426F2Q734272696E67030D3Q0053657457616C6B546F426F2Q73030F3Q005365745470546F426F2Q734B692Q6C03133Q0053657453656C6563746564452Q67496E64657803173Q005365744175746F486174636853656C6563746564452Q6703123Q005365744175746F48617463685065724F6E6503133Q005365744175746F486174636843756265452Q67030B3Q005365744175746F53652Q6C03113Q005365744175746F42757957656967687473030D3Q005365744175746F427579444E4103103Q005365744175746F427579426F64696573031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F72696573030C3Q005365744175746F4375626573030C3Q005365744375626553702Q656403073Q0073652Q73696F6E03093Q00626C61636B6C697374030F3Q0067657447726F756E64466F6C646572030C3Q0067657447726F756E64546F7003093Q006D6178486569676874026Q002840030D3Q00676574466C2Q6F724375626573030E3Q0053657457616C6B546F4375626573030C3Q0053657444697361626C653344030D3Q005365744175746F52656A6F696E03043Q006275737903073Q00706C616365496403053Q006A6F62496403053Q004A6F624964030E3Q006973446973636F2Q6E6563746564030F3Q005365744175746F52656A6F696E563203123Q0053657457616C6B53702Q6564546F2Q676C6503113Q0053657457616C6B53702Q656456616C756503123Q005365744A756D70506F776572546F2Q676C6503113Q005365744A756D70506F77657256616C7565030F3Q00536574496E66696E6974654A756D70030E3Q004D65726368616E74536E6970657203123Q0053656C65637465645461726765745065747303143Q004D65726368616E744D6178507269636554657874030A3Q0070617273655072696365030B3Q006765745065744E616D6573030D3Q00676574496E646578436F756E742Q033Q006D6178026Q00244003093Q0067657452656D6F746503093Q006765744F2Q66657273030A3Q006973546172676574656403143Q00546F2Q676C654D65726368616E7454617267657403143Q00436C6561724D65726368616E7454617267657473030A3Q007461726765745465787403133Q005365744D65726368616E744D6178507269636503113Q005365744D65726368616E74536E6970657203093Q004175746F5175657374030A3Q005175657374446562756703063Q0061637469766503023Q00756903053Q00686174636803023Q006F6E2Q033Q006F2Q6603043Q00626F2Q7303043Q007469636B03043Q006C69667403053Q0070756E636803053Q0073746F6D702Q033Q0067656D03043Q0073652Q6C03093Q0074652Q7269746F727903043Q006375626503083Q00636C612Q7369667903073Q006765744C697374030D3Q00706172736550726F6772652Q7303093Q0072656164436172647303043Q0073746F7003053Q007374617274030C3Q005365744175746F5175657374030A3Q004261636B67726F756E6403063Q00436F6C6F723303073Q0066726F6D52474203093Q005365636F6E64617279026Q003B4003073Q00456C656D656E74026Q004140030C3Q00456C656D656E74486F766572026Q0045402Q033Q004F2Q66025Q00804B4003063Q005374726F6B65025Q0060654003043Q0054657874025Q00E06F4003073Q0053756254657874025Q00E0654003073Q005761726E696E67025Q00C06740025Q0080514003063Q00412Q63656E74026Q005940026Q00644003063Q00507572706C65026Q006940030B3Q004C696768744F72616E6765025Q00406540026Q005440030A3Q004C6967687447722Q656E025Q0080614003053Q005368696E79025Q00A06B40025Q0080494003063Q0059652Q6C6F772Q033Q00526564025Q0080564003523Q00726278612Q7365743A2Q2F4C75615061636B616765732F5061636B616765732F5F496E6465782F4275696C64657249636F6E732F4275696C64657249636F6E732F4275696C64657249636F6E732E6A736F6E03043Q00466F6E7403083Q0066726F6D456E756D030E3Q00536F7572636553616E73426F6C64030A3Q00536F7572636553616E7303093Q0054772Q656E496E666F2Q033Q006E6577027B14AE47E17AC43F030B3Q00456173696E675374796C6503043Q0051756164030F3Q00456173696E67446972656374696F6E2Q033Q004F7574026Q00D03F03053Q005175617274028FC2F5285C8FEA3F03073Q002Q5F696E64657803093Q005363722Q656E47756903083Q00566165687A55495F03043Q006D61746803063Q0072616E646F6D025Q00408F40024Q008087C340030E3Q005A496E6465784265686176696F7203073Q005369626C696E67030C3Q0052657365744F6E537061776E030E3Q0049676E6F7265477569496E736574030C3Q00446973706C61794F72646572025Q00388F4003073Q0044657374726F79030A3Q0044657374726F79696E6703073Q00436F2Q6E656374030C3Q0043726561746557696E646F7703083Q0054726F706963616C00F9042Q0012273Q00013Q0012F3000100023Q00201B2Q0100010003001227000300044Q00FC00010003000200122Q000200023Q00202Q00020002000300122Q000400056Q00020004000200122Q000300023Q00202Q00030003000300122Q000500066Q00030005000200122Q000400073Q0006750104001400013Q0004A23Q001400010012F3000400073Q0020740104000400080006E90004001F000100010004A23Q001F00010012F3000400093Q0006750104001B00013Q0004A23Q001B00010012F3000400093Q0020740104000400080006E90004001F000100010004A23Q001F00010012F30004000A3Q0006E90004001F000100010004A23Q001F00010012F3000400083Q000675010400C100013Q0004A23Q00C10001000675012Q00C100013Q0004A23Q00C1000100260C3Q00C10001000B0004A23Q00C1000100207401050001000C0012270006000D3Q0012F30007000E3Q00069001083Q000100022Q00103Q00064Q00103Q00024Q000E0007000200010012270007000F3Q0012F3000800103Q0006750108003500013Q0004A23Q003500010012F30008000E3Q00069001090001000100012Q00103Q00074Q000E0008000200010004A23Q003C00010012F3000800113Q0006750108003C00013Q0004A23Q003C00010012F30008000E3Q00069001090002000100012Q00103Q00074Q000E0008000200010020740108000500120012F3000900133Q00207401090009001200207401090009001400066800080045000100090004A23Q00450001001227000800153Q0006E900080046000100010004A23Q00460001001227000800163Q001227000900173Q001227000A00183Q001227000B00193Q001227000C001A3Q0012F3000D000E3Q000690010E0003000100062Q00103Q00044Q00103Q00034Q00103Q00094Q00103Q000A4Q00103Q000B4Q00103Q000C4Q000E000D00020001001227000D001B3Q0012F3000E001C3Q000675010E005C00013Q0004A23Q005C00010012F3000E000E3Q000690010F0004000100012Q00103Q000D4Q000E000E000200010004A23Q006700010012F3000E00073Q000675010E006700013Q0004A23Q006700010012F3000E00073Q002074010E000E001C000675010E006700013Q0004A23Q006700010012F3000E000E3Q000690010F0005000100012Q00103Q000D4Q000E000E000200012Q00E6000E3Q00012Q00A3010F00016Q00103Q000400302Q0010001E001F00302Q0010002000214Q0011000B6Q00123Q000300302Q00120023002400202Q00130005002600102Q00120025001300302Q0012002700284Q00133Q000300302Q00130023002900202Q00140005002A00102Q00130025001400302Q0013002700284Q00143Q000300302Q00140023002B00202Q00150005002C00122Q0016002D6Q00150015001600102Q00140025001500302Q0014002700284Q00153Q000300302Q00150023002E00102Q00150025000700302Q0015002700284Q00163Q000300302Q00160023002F00102Q00160025000800302Q0016002700284Q00173Q000300302Q00170023003000102Q00170025000600302Q0017002700284Q00183Q000300302Q00180023003100122Q001900326Q001A00093Q00122Q001B00326Q00190019001B00102Q00180025001900302Q0018002700284Q00193Q000300302Q0019002300334Q001A000A3Q00122Q001B00346Q001C000B6Q001A001A001C00102Q00190025001A00302Q0019002700284Q001A3Q000300302Q001A0023003500102Q001A0025000C00302Q001A002700284Q001B3Q000300302Q001B0023003600122Q001C00326Q001D000D3Q00122Q001E00326Q001C001C001E00102Q001B0025001C00302Q001B002700374Q001C3Q000300302Q001C0023003800122Q001D00393Q00122Q001E00023Q00202Q001E001E003A00122Q001F003B6Q001D001D001F00102Q001C0025001D00302Q001C002700374Q0011000B00010010180110002200110012F80011003D3Q00202Q00110011003E00122Q0012003F6Q00110002000200102Q0010003C00114Q000F00010001001018010E001D000F0012F3000F00403Q002074010F000F004100069001100006000100042Q00103Q00044Q00108Q00103Q00034Q00103Q000E4Q000E000F000200012Q006D01055Q0012F3000500023Q00201B010500050003001227000700424Q00FC00050007000200122Q000600023Q00202Q00060006000300122Q000800436Q00060008000200122Q000700023Q00202Q00070007000300122Q000900446Q00070009000200122Q000800023Q00201B010800080003001227000A00064Q00FC0008000A000200122Q000900023Q00202Q00090009000300122Q000B00056Q0009000B000200122Q000A00023Q00202Q000A000A000300122Q000C00046Q000A000C000200122Q000B00023Q00201B010B000B0003001227000D00454Q00FC000B000D000200122Q000C00023Q00202Q000C000C000300122Q000E00466Q000C000E000200122Q000D00023Q00202Q000D000D000300122Q000F00476Q000D000F000200122Q000E00023Q00201B010E000E0003001227001000484Q00FC000E0010000200122Q000F00023Q00202Q000F000F000300122Q001100496Q000F0011000200122Q001000023Q00202Q00100010000300122Q0012004A6Q00100012000200122Q001100023Q00201B0111001100030012270013004B4Q00420111001300020012F3001200023Q00201B0112001200030012270014004C4Q00420112001400020020740113000A000C0012F30014004D4Q00B700140001000200207401140014004E000675011400122Q013Q0004A23Q00122Q0100207401150014004F0006750115000A2Q013Q0004A23Q000A2Q0100207401150014005000260C001500092Q0100510004A23Q00092Q0100207401150014005000207401150015005200260C0015000A2Q0100510004A23Q000A2Q012Q006101156Q00AA001500013Q0006E90015000E2Q0100010004A23Q000E2Q012Q00D33Q00013Q0012F30016000E3Q00069001170007000100012Q00103Q00144Q000E0016000200012Q006D01146Q00E600143Q00010030460014004F00280012F30015004D4Q00B70015000100020010180115004E00142Q00E600155Q00069001160008000100012Q00103Q00153Q0012F3001700533Q0006E9001700332Q0100010004A23Q00332Q010012F3001700543Q0006E9001700332Q0100010004A23Q00332Q010012F3001700553Q0006E9001700332Q0100010004A23Q00332Q010012F3001700563Q0006E9001700332Q0100010004A23Q00332Q010012F3001700073Q0006750117002E2Q013Q0004A23Q002E2Q010012F3001700073Q0020740117001700560006E9001700332Q0100010004A23Q00332Q010012F3001700573Q000675011700332Q013Q0004A23Q00332Q010012F3001700573Q0020740117001700580012F3001800073Q0006750118003A2Q013Q0004A23Q003A2Q010012F3001800073Q0020740118001800080006E9001800452Q0100010004A23Q00452Q010012F3001800093Q000675011800412Q013Q0004A23Q00412Q010012F3001800093Q0020740118001800080006E9001800452Q0100010004A23Q00452Q010012F30018000A3Q0006E9001800452Q0100010004A23Q00452Q010012F3001800083Q0006750113004B2Q013Q0004A23Q004B2Q010012F30019000E3Q000690011A0009000100012Q00103Q00134Q000E0019000200010012F3001900403Q002074011900190041000690011A000A000100032Q00103Q00134Q00103Q00164Q00103Q000C4Q000E0019000200010012F3001900403Q002074011900190041000690011A000B000100022Q00103Q00164Q00103Q00074Q000E001900020001001227001900593Q0012F3001A000E3Q000690011B000C000100022Q00103Q00094Q00103Q00194Q000F011A0002000100122Q001A005A6Q001B00196Q001A001A001B4Q001B3Q000200302Q001B005B005C00302Q001B005D005E00122Q001C005F3Q00122Q001D00603Q00122Q001E00614Q00E6001F5Q00122F0020004D6Q00200001000200122Q002100133Q00202Q00210021006300202Q00210021006400102Q00200062002100122Q0020004D6Q00200001000200302Q00200065003700122Q0020004D4Q00B700200001000200305900200066003700122Q0020004D6Q00200001000200302Q00200067003700122Q0020004D6Q00200001000200302Q00200068003700122Q0020004D6Q00200001000200302Q0020006900370012980020004D6Q00200001000200302Q0020006A003700122Q0020004D6Q00200001000200302Q0020006B003700122Q0020004D6Q00200001000200302Q0020006C003700122Q0020004D4Q00B70020000100020030590020006D003700122Q0020004D6Q00200001000200302Q0020006E003700122Q0020004D6Q00200001000200302Q0020006F003700122Q0020004D6Q00200001000200302Q0020007000710012980020004D6Q00200001000200302Q00200072007300122Q0020004D6Q00200001000200302Q00200074003700122Q0020004D6Q00200001000200302Q00200075003700122Q0020004D4Q00B700200001000200305900200076003700122Q0020004D6Q00200001000200302Q00200077003700122Q0020004D6Q00200001000200302Q00200078003700122Q0020004D6Q00200001000200302Q0020007900370012980020004D6Q00200001000200302Q0020007A003700122Q0020004D6Q00200001000200302Q0020007B003700122Q0020004D6Q00200001000200302Q0020007C003700122Q0020004D4Q00B70020000100020030590020007D003700122Q0020004D6Q00200001000200302Q0020007E003700122Q0020004D6Q00200001000200302Q0020007F003700122Q0020004D6Q00200001000200302Q0020008000810012F30020004D4Q00B70020000100020030460020008200370012F30020004D4Q001E00200001000200122Q0021004D6Q00210001000200202Q00210021008300062Q002100C72Q0100010004A23Q00C72Q012Q00E600215Q0010180120008300210012F30020004D4Q00B70020000100020020740120002000830020740121002000840006E9002100CF2Q0100010004A23Q00CF2Q012Q00AA00215Q00101801200084002100305900200085008600122Q0021004D6Q00210001000200302Q00210087003700122Q0021004D6Q00210001000200302Q00210088003700122Q0021004D6Q00210001000200302Q0021008900370012980021004D6Q00210001000200302Q0021008A003700122Q0021004D6Q00210001000200302Q0021008B003700122Q0021004D6Q00210001000200302Q0021008C007100122Q0021004D4Q006200210001000200302Q0021008D003700122Q0021004D6Q00210001000200302Q0021008E007300122Q0021004D6Q00210001000200302Q0021008F003700122Q0021003D3Q00202Q0021002100902Q00B7002100010002001227002200914Q001C002300233Q001227002400914Q001C002500263Q0006900127000D000100012Q00103Q00133Q0006900128000E000100012Q00103Q00273Q0006900129000F000100022Q00103Q00254Q00103Q00263Q0012F3002A00403Q002074012A002A0041000690012B0010000100082Q00103Q00144Q00103Q00204Q00103Q00294Q00103Q00274Q00103Q00284Q00103Q00264Q00103Q00254Q00103Q00054Q00FF002A000200022Q0010002B00163Q002074012C00070092000690012D0011000100012Q00103Q00244Q00B8002B002D000100023C012B00123Q000690012C0013000100012Q00103Q00134Q001C002D00323Q0012F3003300403Q00207401330033004100069001340014000100072Q00103Q000B4Q00103Q00324Q00103Q00314Q00103Q002D4Q00103Q002F4Q00103Q00304Q00103Q002E4Q000E00330002000100069001330015000100012Q00103Q00133Q00069001340016000100022Q00103Q00134Q00103Q001E4Q0010003500344Q00690035000100012Q0010003500163Q00207401360013009300069001370017000100012Q00103Q001E4Q00B800350037000100069001350018000100012Q00103Q000E3Q00069001360019000100012Q00103Q001F3Q0006900137001A000100012Q00103Q001D4Q00E600385Q0006900139001B000100052Q00103Q00134Q00103Q000E4Q00103Q00354Q00103Q00374Q00103Q00383Q000690013A001C000100042Q00103Q00334Q00103Q001B4Q00103Q000E4Q00103Q00353Q00023C013B001D3Q0012F3003C00403Q002074013C003C0041000690013D001E000100032Q00103Q00164Q00103Q00074Q00103Q00134Q000E003C000200012Q0010003C00163Q002074013D00060094000690013E001F000100012Q00103Q00134Q00B8003C003E00012Q0010003C00163Q002074013D00100095000690013E0020000100022Q00103Q000F4Q00103Q00134Q00B8003C003E00012Q0010003C00163Q002074013D00070096000690013E0021000100012Q00103Q00134Q00B8003C003E00010012F3003C00403Q002074013C003C0041000690013D0022000100022Q00103Q00144Q00103Q00134Q000E003C000200010012F3003C00973Q002074013C003C00982Q001C003D003D3Q001227003E00914Q0010003F00163Q00207401400007009200069001410023000100072Q00103Q00364Q00103Q00134Q00103Q00394Q00103Q003D4Q00103Q003E4Q00103Q00384Q00103Q003C4Q00B8003F00410001001227003F00993Q00069001400024000100032Q00103Q00144Q00103Q00334Q00103Q00133Q00069001410025000100022Q00103Q00404Q00103Q003F4Q00E600425Q00069001430026000100032Q00103Q000D4Q00103Q00134Q00103Q00323Q0010180142009A004300069001430027000100012Q00103Q002D3Q0010180142009B004300069001430028000100012Q00103Q002D3Q0010180142009C004300069001430029000100042Q00103Q001F4Q00103Q00334Q00103Q00364Q00103Q003B3Q0010180142009D00430006900143002A000100012Q00103Q00413Q0010180142009E00430006900143002B000100042Q00103Q001F4Q00103Q00134Q00103Q00334Q00103Q00363Q0010180142009F00430006900143002C000100022Q00103Q00134Q00103Q001E3Q001018014200A000430006900143002D000100012Q00103Q00133Q001018014200A1004300023C0143002E3Q001018014200A200430006900143002F000100062Q00103Q00074Q00103Q00334Q00103Q00364Q00103Q001F4Q00103Q003B4Q00103Q003A3Q001018014200A3004300069001430030000100012Q00103Q00333Q001018014200A4004300069001430031000100022Q00103Q00334Q00103Q00133Q001018014200A5004300069001430032000100012Q00103Q00333Q001018014200A6004300023C014300333Q001018014200A7004300069001430034000100032Q00103Q002E4Q00103Q000B4Q00103Q001C3Q001018014200A8004300069001430035000100032Q00103Q002E4Q00103Q000B4Q00103Q001C3Q001018014200A9004300069001430036000100012Q00103Q00133Q00069001440037000100022Q00103Q002E4Q00103Q000B3Q001018014200AA004400069001440038000100042Q00103Q00334Q00103Q00074Q00103Q00314Q00103Q000B3Q001018014200AB004400069001440039000100022Q00103Q002F4Q00103Q000B3Q001018014200AC00440006900144003A000100022Q00103Q00304Q00103Q000B3Q001018014200AD00440006900144003B000100022Q00103Q00304Q00103Q000B3Q001018014200AE00440006900144003C000100022Q00103Q002F4Q00103Q000B3Q001018014200AF00440006900144003D000100042Q00103Q00334Q00103Q00074Q00103Q00314Q00103Q000B3Q001018014200B000440006900144003E000100012Q00103Q00413Q001018014200B100440006900144003F000100012Q00103Q00203Q001018014200B2004400069001440040000100012Q00103Q00203Q001018014200B300442Q00E600443Q0002003046004400B400912Q00E600455Q001018014400B5004500023C014500413Q001018014400B6004500069001450042000100012Q00103Q00443Q001018014400B70045003046004400B800B900069001450043000100012Q00103Q00443Q001018014400BA004500023C014500443Q00069001460045000100062Q00103Q00444Q00103Q00204Q00103Q00144Q00103Q00134Q00103Q00334Q00103Q00453Q001018014200BB004600069001460046000100012Q00103Q00073Q001018014200BC004600023C014600473Q001032014200BD00464Q00463Q000400302Q004600B4009100302Q004600BE003700122Q004700023Q00202Q00470047003A00102Q004600BF004700122Q004700023Q00202Q0047004700C100102Q004600C0004700069001470048000100012Q00103Q00103Q001018014600C2004700069001470049000100042Q00103Q00464Q00103Q00144Q00103Q000F4Q00103Q00133Q001018014200C300470006900147004A000100022Q00103Q00134Q00103Q001E3Q001018014200C400470006900147004B000100012Q00103Q00133Q001018014200C500470006900147004C000100012Q00103Q00133Q001018014200C600470006900147004D000100012Q00103Q00133Q001018014200C7004700023C0147004E3Q001005004200C8004700122Q0047004D6Q00470001000200302Q004700C9003700122Q0047004D6Q0047000100024Q00485Q00102Q004700CA004800122Q0047004D6Q004700010002003046004700CB000B2Q00E600473Q0001003046004700B4009100023C0148004F3Q00023C014900503Q001018014700CC004900069001490051000100012Q00103Q00133Q001018014700CD004900069001490052000100012Q00103Q00133Q001018014700CE0049003046004700CF00D000069001490053000100012Q00103Q000B3Q001018014700D1004900069001490054000100012Q00103Q00133Q001018014700D2004900023C014900553Q001018014700D3004900069001490056000100012Q00103Q00473Q001018014200D4004900023C014900573Q001018014200D5004900023C014900583Q001018014700D6004900023C014900593Q001018014200D700490006900149005A000100042Q00103Q00474Q00103Q000B4Q00103Q00484Q00103Q00133Q001044014200D8004900122Q0049004D6Q00490001000200302Q004900D9003700122Q0049004D6Q00490001000200302Q004900DA00374Q00493Q000300302Q004900B4009100302Q004900DB00512Q00E6004A5Q001018014900DC004A000690014A005B000100012Q00103Q00494Q00E6004B3Q00092Q00E6004C3Q0002000690014D005C000100022Q00103Q00434Q00103Q00423Q001018014C00DE004D000690014D005D000100012Q00103Q00423Q001018014C00DF004D001018014B00DD004C2Q00E6004C3Q0003000690014D005E000100012Q00103Q00423Q001018014C00DE004D000690014D005F000100012Q00103Q00423Q001018014C00DF004D000690014D0060000100012Q00103Q00423Q001018014C00E1004D001018014B00E0004C2Q00E6004C3Q0002000690014D0061000100012Q00103Q00423Q001018014C00DE004D000690014D0062000100012Q00103Q00423Q001018014C00DF004D001018014B00E2004C2Q00E6004C3Q0002000690014D0063000100012Q00103Q00423Q001018014C00DE004D000690014D0064000100012Q00103Q00423Q001018014C00DF004D001018014B00E3004C2Q00E6004C3Q0002000690014D0065000100012Q00103Q00423Q001018014C00DE004D000690014D0066000100012Q00103Q00423Q001018014C00DF004D001018014B00E4004C2Q00E6004C3Q0002000690014D0067000100012Q00103Q00423Q001018014C00DE004D000690014D0068000100012Q00103Q00423Q001018014C00DF004D001018014B00E5004C2Q00E6004C3Q0002000690014D0069000100012Q00103Q00423Q001018014C00DE004D000690014D006A000100012Q00103Q00423Q001018014C00DF004D001018014B00E6004C2Q00E6004C3Q0003000690014D006B000100012Q00103Q00423Q001018014C00DE004D000690014D006C000100012Q00103Q00423Q001018014C00DF004D000690014D006D000100012Q00103Q00423Q001018014C00E1004D001018014B00E7004C2Q00E6004C3Q0002000690014D006E000100012Q00103Q00423Q001018014C00DE004D000690014D006F000100012Q00103Q00423Q001018014C00DF004D001018014B00E8004C00023C014C00703Q001018014900E9004C000690014C0071000100012Q00103Q00133Q001018014900EA004C00023C014C00723Q000690014D0073000100012Q00103Q004C3Q001018014900EB004D00023C014D00743Q000690014E0075000100022Q00103Q00494Q00103Q004D3Q001018014900EC004E000690014E0076000100022Q00103Q00494Q00103Q004B3Q001018014900ED004E000690014E0077000100022Q00103Q00494Q00103Q004B3Q001018014900EE004E000690014E0078000100042Q00103Q00494Q00103Q004A4Q00103Q00144Q00103Q004B3Q00103F004200EF004E4Q004E3Q001000122Q004F00F13Q00202Q004F004F00F200122Q005000613Q00122Q005100613Q00122Q005200616Q004F0052000200102Q004E00F0004F00122Q004F00F13Q002074014F004F00F20012C4005000F43Q00122Q005100F43Q00122Q005200F46Q004F0052000200102Q004E00F3004F00122Q004F00F13Q00202Q004F004F00F200122Q005000F63Q00122Q005100F63Q00122Q005200F64Q0042014F00520002001018014E00F5004F0012F3004F00F13Q002074014F004F00F20012C4005000F83Q00122Q005100F83Q00122Q005200F86Q004F0052000200102Q004E00F7004F00122Q004F00F13Q00202Q004F004F00F200122Q005000FA3Q00122Q005100FA3Q00122Q005200FA4Q0042014F00520002001018014E00F9004F0012F3004F00F13Q002074014F004F00F20012C4005000FC3Q00122Q005100FC3Q00122Q005200FC6Q004F0052000200102Q004E00FB004F00122Q004F00F13Q00202Q004F004F00F200122Q005000FE3Q00122Q005100FE3Q00122Q005200FE4Q0042014F00520002001018014E00FD004F0012F3004F00F13Q002074014F004F00F200122700502Q00012Q00122700512Q00012Q00129C00522Q00015Q004F0052000200102Q004E00FF004F00122Q004F002Q012Q00122Q005000F13Q00202Q0050005000F200122Q005100FE3Q00122Q00520002012Q00122Q00530003015Q0050005300022Q008E004E004F0050001227004F0004012Q0012F3005000F13Q0020740150005000F200122700510005012Q00122700520006012Q001227005300FE4Q00BD0050005300024Q004E004F005000122Q004F0007012Q00122Q005000F13Q00202Q0050005000F200122Q00510008012Q00122Q00520006012Q00122Q005300FE6Q0050005300024Q004E004F0050001227004F0009012Q0012F3005000F13Q0020740150005000F2001227005100FE3Q0012270052000A012Q0012270053000B013Q00BD0050005300024Q004E004F005000122Q004F000C012Q00122Q005000F13Q00202Q0050005000F200122Q0051000D012Q00122Q005200FE3Q00122Q00530006015Q0050005300024Q004E004F0050001227004F000E012Q0012F3005000F13Q0020740150005000F2001227005100FE3Q0012270052000F012Q00122700530010013Q00BD0050005300024Q004E004F005000122Q004F0011012Q00122Q005000F13Q00202Q0050005000F200122Q005100FE3Q00122Q0052000F012Q00122Q00530010015Q0050005300024Q004E004F0050001227004F0012012Q0012F3005000F13Q0020740150005000F2001227005100FE3Q00122700520013012Q00122700530013013Q00420150005300022Q007B014E004F005000122Q004F0014012Q00122Q00500015012Q00122Q00510016015Q00500050005100122Q005100133Q00122Q00520015015Q00510051005200122Q00520017015Q0051005100522Q00FF00500002000200122Q01510015012Q00122Q00520016015Q00510051005200122Q005200133Q00122Q00530015015Q00520052005300122Q00530018015Q0052005200534Q00510002000200122Q00520019012Q0012270053001A013Q007300520052005300122Q0053001B012Q00122Q005400133Q00122Q0055001C015Q00540054005500122Q0055001D015Q00540054005500122Q005500133Q00122Q0056001E015Q0055005500560012270056001F013Q00300055005500562Q00420152005500020012F300530019012Q0012270054001A013Q007300530053005400122Q00540020012Q00122Q005500133Q00122Q0056001C015Q00550055005600122Q00560021015Q00550055005600122Q005600133Q00122Q0057001E015Q0056005600570012270057001F013Q00300056005600572Q004201530056000200122700540022012Q00023C015500793Q0006900156007A000100012Q00103Q00553Q0006900157007B000100022Q00103Q00554Q00103Q004E3Q0006900158007C000100012Q00103Q00553Q0006900159007D000100022Q00103Q00054Q00103Q00523Q000690015A007E000100032Q00103Q00554Q00103Q004F4Q00103Q004E3Q000690015B007F000100032Q00103Q00554Q00103Q00164Q00103Q00063Q000690015C0080000100022Q00103Q00164Q00103Q00063Q000690015D0081000100012Q00103Q000A4Q00EE005E5Q00122Q005F0023015Q005E005F005E4Q005F00553Q00122Q00600024015Q00613Q000500122Q00620025012Q00122Q00630026012Q00122Q00640027015Q00630063006400122700640028012Q0012A400650029015Q0063006500024Q00620062006300102Q00610026006200122Q0062002A012Q00122Q006300133Q00122Q0064002A015Q00630063006400122Q0064002B015Q0063006300642Q008E0061006200630012BA0062002C015Q00638Q00610062006300122Q0062002D015Q006300016Q00610062006300122Q0062002E012Q00122Q0063002F015Q0061006200634Q005F006100020012F30060000E3Q00069001610082000100012Q00103Q005F4Q000E0060000200012Q00100060005D4Q00B7006000010002001018015F0052006000101801140050005F000690016000830001000D2Q00103Q00144Q00103Q00204Q00103Q00474Q00103Q00444Q00103Q00464Q00103Q00494Q00103Q00294Q00103Q00154Q00103Q00134Q00103Q001E4Q00103Q00074Q00103Q00114Q00103Q005F3Q00122700610030012Q00069001620084000100012Q00103Q00604Q008E00140061006200122700610031013Q00300061005F006100122700630032013Q002F01610061006300069001630085000100012Q00103Q00604Q00B8006100630001000690016100860001000E2Q00103Q00114Q00103Q00554Q00103Q004E4Q00103Q005F4Q00103Q00564Q00103Q00574Q00103Q00584Q00103Q00504Q00103Q00544Q00103Q00514Q00103Q00594Q00103Q00524Q00103Q00134Q00103Q005B3Q00122700620033012Q00069001630087000100132Q00103Q004E4Q00103Q00554Q00103Q005F4Q00103Q00564Q00103Q00574Q00103Q00584Q00103Q00544Q00103Q005A4Q00103Q00504Q00103Q00594Q00103Q00524Q00103Q00164Q00103Q00064Q00103Q005B4Q00103Q00604Q00103Q00534Q00103Q00514Q00103Q005C4Q00103Q00484Q008E005E006200632Q0010006200613Q00122700630034012Q000690016400880001000F2Q00103Q005E4Q00103Q001A4Q00103Q00424Q00103Q004E4Q00103Q00144Q00103Q00474Q00103Q00544Q00103Q00494Q00103Q00214Q00103Q00224Q00103Q00234Q00103Q00244Q00103Q00134Q00103Q002C4Q00103Q002B4Q00B80062006400012Q00D33Q00013Q00893Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D6500084Q00943Q00013Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200206Q00049Q006Q00017Q00013Q0003103Q006964656E746966796578656375746F7200043Q0012F33Q00014Q00B73Q000100022Q00038Q00D33Q00017Q00013Q00030F3Q006765746578656375746F726E616D6500043Q0012F33Q00014Q00B73Q000100022Q00038Q00D33Q00017Q000C3Q002Q033Q0055726C03173Q00682Q74703A2Q2F69702D6170692E636F6D2F6A736F6E2F03063Q004D6574686F642Q033Q0047455403043Q00426F6479030A3Q004A534F4E4465636F646503063Q0073746174757303073Q0073752Q63652Q7303053Q00717565727903043Q0063697479030A3Q00726567696F6E4E616D652Q033Q0069737000284Q0016019Q00013Q000200302Q00010001000200302Q0001000300046Q0002000200064Q002700013Q0004A23Q002700010020742Q013Q00050006752Q01002700013Q0004A23Q002700012Q007F000100013Q00201B2Q010001000600207401033Q00052Q00422Q01000300020006752Q01002700013Q0004A23Q0027000100207401020001000700269900020027000100080004A23Q002700010020740102000100090006E900020017000100010004A23Q001700012Q007F000200024Q0003000200023Q00207401020001000A0006E90002001C000100010004A23Q001C00012Q007F000200034Q0003000200033Q00207401020001000B0006E900020021000100010004A23Q002100012Q007F000200044Q0003000200043Q00207401020001000C0006E900020026000100010004A23Q002600012Q007F000200054Q0003000200054Q00D33Q00017Q00013Q0003073Q006765746877696400043Q0012F33Q00014Q00B73Q000100022Q00038Q00D33Q00017Q00023Q002Q033Q0073796E03073Q006765746877696400053Q00122B012Q00013Q00206Q00026Q000100029Q006Q00017Q00013Q0003053Q007063612Q6C00083Q0012F33Q00013Q0006902Q013Q000100042Q007F8Q007F3Q00014Q007F3Q00024Q007F3Q00034Q000E3Q000200012Q00D33Q00013Q00013Q00083Q002Q033Q0055726C03063Q004D6574686F6403043Q00504F535403073Q0048656164657273030C3Q00436F6E74656E742D5479706503103Q00612Q706C69636174696F6E2F6A736F6E03043Q00426F6479030A3Q004A534F4E456E636F6465000F4Q009A9Q0000013Q00044Q000200013Q00102Q00010001000200302Q0001000200034Q00023Q000100302Q00020005000600102Q0001000400024Q000200023Q00202Q0002000200082Q007F000400034Q00420102000400020010182Q01000700022Q000E3Q000200012Q00D33Q00017Q00013Q0003073Q0044657374726F7900084Q007F7Q002074014Q0001000675012Q000700013Q0004A23Q000700012Q007F7Q002074014Q00012Q00693Q000100012Q00D33Q00017Q00033Q0003073Q00436F2Q6E65637403053Q007461626C6503063Q00696E73657274020A3Q0020D600023Q00014Q000400016Q00020004000200122Q000300023Q00202Q0003000300034Q00048Q000500026Q0003000500014Q000200028Q00017Q00043Q0003153Q0043616D6572614D696E5A2Q6F6D44697374616E6365026Q00E03F03153Q0043616D6572614D61785A2Q6F6D44697374616E6365025Q0088C34000054Q005A7Q00304Q000100029Q0000304Q000300046Q00017Q00013Q0003053Q0049646C6564000A4Q007F7Q000675012Q000900013Q0004A23Q000900012Q007F3Q00014Q007F00015Q0020742Q010001000100069001023Q000100012Q007F3Q00024Q00B83Q000200012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012F33Q00013Q0006902Q013Q000100012Q007F8Q000E3Q000200012Q00D33Q00013Q00013Q000B3Q00030B3Q0042752Q746F6E31446F776E03073Q00566563746F72322Q033Q006E6577028Q0003093Q00776F726B7370616365030D3Q0043752Q72656E7443616D65726103063Q00434672616D6503043Q007461736B03043Q0077616974026Q00F03F03093Q0042752Q746F6E315570001B4Q00367Q00206Q000100122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300076Q0003000100124Q00083Q00206Q000900122Q0001000A8Q000200019Q0000206Q000B00122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300076Q000300016Q00017Q00023Q00028Q0003093Q0048656172746265617400083Q0012273Q00014Q007F00016Q007F000200013Q00207401020002000200069001033Q000100012Q00108Q00B80001000300012Q00D33Q00013Q00013Q00103Q0003023Q006F7303053Q00636C6F636B029A5Q99C93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030E3Q0047657444657363656E64616E747303083Q00426173655061727403043Q004E616D6503103Q0048756D616E6F6964522Q6F7450617274030C3Q005472616E73706172656E6379029A5Q99A93F002F3Q001277012Q00013Q00206Q00026Q000100024Q00018Q00013Q000100262Q00010008000100030004A23Q000800012Q00D33Q00014Q00037Q00121A000100043Q00202Q00010001000500122Q000300066Q00010003000200062Q0001002E00013Q0004A23Q002E00010012F3000200073Q00201B0103000100082Q00A6000300044Q008C01023Q00040004A23Q002C000100201B0107000600090012270009000A4Q00420107000900020006750107002C00013Q0004A23Q002C00010012F3000700073Q00201B01080006000B2Q00A6000800094Q008C01073Q00090004A23Q002A000100201B010C000B0009001227000E000C4Q0042010C000E0002000675010C002A00013Q0004A23Q002A0001002074010C000B000D00260C000C002A0001000E0004A23Q002A0001002074010C000B000F002613000C002A000100100004A23Q002A0001003046000B000F00100006FE0007001E000100020004A23Q001E00010006FE00020014000100020004A23Q001400012Q00D33Q00017Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D65000D4Q007F7Q00201B014Q00010012F3000200023Q0020740102000200032Q0042012Q00020002000675012Q000C00013Q0004A23Q000C00010020742Q013Q00040006752Q01000C00013Q0004A23Q000C00010020742Q013Q00042Q0003000100014Q00D33Q00017Q00033Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400094Q007F7Q002074014Q00010006A22Q01000700013Q0004A23Q0007000100201B2Q013Q0002001227000300034Q00422Q01000300022Q00682Q0100024Q00D33Q00017Q000D3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F707303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03093Q004D61676E697475646500314Q007F8Q00B73Q000100020006E93Q0006000100010004A23Q000600012Q001C000100014Q00682Q0100023Q0012F3000100013Q00201B2Q0100010002001227000300034Q00422Q01000300020006E90001000E000100010004A23Q000E00012Q001C000200024Q0068010200024Q001C000200023Q0012F3000300043Q0020740103000300050012B2000400063Q00202Q0005000100074Q000500066Q00043Q000600044Q002D000100201B010900080002001227000B00084Q00420109000B00020006E900090023000100010004A23Q0023000100201B010900080002001227000B00094Q00420109000B00020006E900090023000100010004A23Q0023000100201B01090008000A001227000B000B4Q00420109000B00020006750109002D00013Q0004A23Q002D0001002074010A3Q000C002074010B0009000C2Q00B9000A000A000B002074010A000A000D000699010A002D000100030004A23Q002D00012Q00100003000A4Q0010000200093Q0006FE00040016000100020004A23Q001600012Q0068010200024Q00D33Q00017Q00013Q0003063Q0043616E63656C000B4Q007F7Q000675012Q000800013Q0004A23Q000800012Q007F7Q00201B014Q00012Q000E3Q000200012Q001C8Q00038Q001C8Q00033Q00014Q00D33Q00017Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99A93F03073Q00456E61626C6564030E3Q00497344657363656E64616E744F6603093Q00776F726B737061636503083Q00506F736974696F6E03093Q004D61676E697475646503043Q006D6174682Q033Q006D617803053Q0053702Q6564026Q00F03F027B14AE47E17A843F03093Q0054772Q656E496E666F2Q033Q006E657703043Q00456E756D030B3Q00456173696E675374796C6503063Q004C696E65617203063Q0043726561746503063Q00434672616D6503093Q00436F6D706C6574656403073Q00436F2Q6E65637403043Q00506C617900584Q007F7Q002074014Q0001000675012Q005700013Q0004A23Q005700010012F33Q00023Q002074014Q0003001227000100044Q000E3Q000200012Q007F3Q00013Q002074014Q00050006E93Q000F000100010004A23Q000F00012Q007F3Q00024Q00693Q000100010004A25Q00012Q007F3Q00034Q00B73Q000100020006E93Q0016000100010004A23Q001600012Q007F000100024Q00690001000100010004A25Q00012Q007F000100044Q00B70001000100020006752Q01005400013Q0004A23Q0054000100201B0102000100060012F3000400074Q00420102000400020006E900020022000100010004A23Q002200012Q007F000200024Q00690002000100010004A25Q000100207401023Q00080020740103000100082Q00B90002000200030020740102000200092Q007F000300053Q0006680001002C000100030004A23Q002C00012Q007F000300063Q0006E900033Q000100010004A25Q00012Q007F000300024Q00780103000100014Q000100053Q00122Q0003000A3Q00202Q00030003000B00122Q0004000A3Q00202Q00040004000B4Q000500013Q00202Q00050005000C00122Q0006000D6Q0004000600024Q00040002000400122Q0005000E6Q00030005000200122Q0004000F3Q00202Q0004000400104Q000500033Q00122Q000600113Q00202Q00060006001200202Q0006000600134Q0004000600024Q000500073Q00202Q0005000500144Q00078Q000800046Q00093Q000100202Q000A0001001500102Q00090015000A4Q0005000900024Q000500066Q000500063Q00202Q00050005001600202Q00050005001700069001073Q000100012Q007F3Q00024Q00370005000700014Q000500063Q00202Q0005000500184Q00050002000100046Q00012Q007F000200024Q00690002000100010004A25Q00012Q00D33Q00013Q00018Q00034Q007F8Q00693Q000100012Q00D33Q00017Q00043Q00028Q0003043Q006D61746803053Q00666C2Q6F72026Q00F03F01083Q000EA70001000700013Q0004A23Q000700010012F3000100023Q0020742Q0100010003001040010200044Q00FF0001000200022Q000300016Q00D33Q00017Q000B3Q00024Q00652QCD4103063Q00737472696E6703063Q00666F726D617403053Q00252E326642024Q0080842E4103053Q00252E32664D025Q00408F4003053Q00252E31664B03083Q00746F737472696E6703043Q006D61746803053Q00666C2Q6F7201223Q000E482Q01000900013Q0004A23Q000900010012F3000100023Q00200800010001000300122Q000200043Q00202Q00033Q00014Q000100036Q00015Q00044Q001A0001000E480105001200013Q0004A23Q001200010012F3000100023Q00200800010001000300122Q000200063Q00202Q00033Q00054Q000100036Q00015Q00044Q001A0001000E480107001A00013Q0004A23Q001A00010012F3000100023Q00204A2Q010001000300122Q000200083Q00202Q00033Q00074Q000100036Q00015Q0012F3000100093Q0012A10002000A3Q00202Q00020002000B4Q00038Q000200036Q00018Q00019Q0000017Q00113Q0003043Q0047656D732Q033Q0047656D03083Q004469616D6F6E647303073Q004469616D6F6E6403093Q0047656D7356616C7565030E3Q0046696E6446697273744368696C64030B3Q006C65616465727374617473030B3Q004C6561646572737461747303063Q006970616972732Q033Q0049734103083Q00496E7456616C7565030B3Q004E756D62657256616C756503163Q00446F75626C65436F6E73747261696E656456616C7565030B3Q004765744368696C6472656E03063Q00466F6C646572030D3Q00436F6E66696775726174696F6E03053Q004D6F64656C005E4Q004F3Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500012Q007F00015Q00201B2Q0100010006001227000300074Q00422Q01000300020006E900010011000100010004A23Q001100012Q007F00015Q00201B2Q0100010006001227000300084Q00422Q01000300020006752Q01002E00013Q0004A23Q002E00010012F3000200094Q001000036Q006E0102000200040004A23Q002C000100201B0107000100062Q0010000900064Q00420107000900020006750107002C00013Q0004A23Q002C000100201B01080007000A001227000A000B4Q00420108000A00020006E90008002B000100010004A23Q002B000100201B01080007000A001227000A000C4Q00420108000A00020006E90008002B000100010004A23Q002B000100201B01080007000A001227000A000D4Q00420108000A00020006750108002C00013Q0004A23Q002C00012Q0068010700023Q0006FE00020017000100020004A23Q001700010012F3000200094Q002C00035Q00202Q00030003000E4Q000300046Q00023Q000400044Q0059000100201B01070006000A0012270009000F4Q00420107000900020006E900070043000100010004A23Q0043000100201B01070006000A001227000900104Q00420107000900020006E900070043000100010004A23Q0043000100201B01070006000A001227000900114Q00420107000900020006750107005900013Q0004A23Q005900010012F3000700094Q001000086Q006E0107000200090004A23Q0057000100201B010C000600062Q0010000E000B4Q0042010C000E0002000675010C005700013Q0004A23Q0057000100201B010D000C000A001227000F000B4Q0042010D000F00020006E9000D0056000100010004A23Q0056000100201B010D000C000A001227000F000C4Q0042010D000F0002000675010D005700013Q0004A23Q005700012Q0068010C00023Q0006FE00070047000100020004A23Q004700010006FE00020034000100020004A23Q003400012Q001C000200024Q0068010200024Q00D33Q00017Q000C3Q00030C3Q0057616974466F724368696C6403073Q0052656D6F746573026Q001440030A3Q004C69667457656967687403133Q0053652Q6C537472656E677468526571756573742Q033Q00505650030D3Q00412Q7461636B412Q74656D707403043Q0053686F70030D3Q0052657175657374427579412Q6C030F3Q0052657175657374507572636861736503043Q0050657473030B3Q005075726368617365452Q6700384Q007F7Q0020355Q000100122Q000200023Q00122Q000300038Q0003000200064Q003700013Q0004A23Q0037000100201B2Q013Q0001001282000300043Q00122Q000400036Q0001000400024Q000100013Q00202Q00013Q000100122Q000300053Q00122Q000400036Q0001000400024Q000100023Q0020C600013Q000100122Q000300063Q00122Q000400036Q00010004000200062Q0002001B000100010004A23Q001B000100201B010200010001001227000400073Q001227000500034Q00420102000500022Q0003000200033Q00203500023Q000100122Q000400083Q00122Q000500036Q00020005000200062Q0002002C00013Q0004A23Q002C000100201B010300020001001282000500093Q00122Q000600036Q0003000600024Q000300043Q00202Q00030002000100122Q0005000A3Q00122Q000600036Q0003000600024Q000300053Q00201B01033Q00010012270005000B3Q001227000600034Q00420103000600020006A201040036000100030004A23Q0036000100201B0104000300010012270006000C3Q001227000700034Q00420104000700022Q0003000400064Q00D33Q00017Q00073Q0003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004865616C7468028Q00030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400154Q007F7Q002074014Q00010006E93Q0006000100010004A23Q000600012Q001C000100014Q00682Q0100023Q00201B2Q013Q0002001227000300034Q00422Q01000300020006752Q01001000013Q0004A23Q0010000100207401020001000400266D00020010000100050004A23Q001000012Q001C000200024Q0068010200023Q00201B01023Q0006001227000400074Q0020010200044Q008C00026Q00D33Q00017Q00083Q0003093Q00436861726163746572030E3Q00436861726163746572412Q64656403043Q0057616974030C3Q0057616974466F724368696C6403083Q0048756D616E6F6964026Q00144003093Q0057616C6B53702Q6564029Q00144Q007F7Q002074014Q00010006E93Q0008000100010004A23Q000800012Q007F7Q002074014Q000200201B014Q00032Q00FF3Q0002000200201B2Q013Q0004001227000300053Q001227000400064Q00422Q01000400020006752Q01001300013Q0004A23Q00130001002074010200010007000EA700080013000100020004A23Q001300010020740102000100072Q0003000200014Q00D33Q00017Q00093Q0003043Q007461736B03043Q0077616974029A5Q99C93F03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q0067657467656E76030B3Q004175746F47656D57616C6B030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q656401163Q0012EB000100013Q00202Q00010001000200122Q000200036Q00010002000100202Q00013Q000400122Q000300056Q00010003000200062Q0001001500013Q0004A23Q001500010012F3000200064Q00B70002000100020020740102000200070006E900020015000100010004A23Q001500010012F3000200064Q00B70002000100020020740102000200080006E900020015000100010004A23Q001500010020740102000100092Q000300026Q00D33Q00017Q00083Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103063Q00466F6C64657203063Q00737472696E6703053Q006D6174636803043Q004E616D6503053Q005E25642B2400183Q0012F33Q00014Q002C00015Q00202Q0001000100024Q000100029Q00000200044Q0013000100201B010500040003001227000700044Q00420105000700020006750105001300013Q0004A23Q001300010012F3000500053Q0020E800050005000600202Q00060004000700122Q000700086Q00050007000200062Q0005001300013Q0004A23Q001300012Q0068010400023Q0006FE3Q0006000100020004A23Q000600012Q001C8Q0068012Q00024Q00D33Q00017Q000A3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403083Q0041697264726F707303063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D6503073Q0041697264726F7003103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727400263Q0012783Q00013Q00206Q000200122Q000200038Q0002000200064Q0008000100010004A23Q000800012Q001C000100014Q00682Q0100023Q0012F3000100043Q00201B01023Q00052Q00A6000200034Q008C2Q013Q00030004A23Q0021000100207401060005000600269900060021000100070004A23Q002100012Q007F00066Q00300006000600050006E900060021000100010004A23Q0021000100201B010600050002001227000800084Q00420106000800020006E90006001C000100010004A23Q001C000100201B0106000500090012270008000A4Q00420106000800020006750106002100013Q0004A23Q002100012Q0010000700054Q0010000800064Q0072000700033Q0006FE0001000D000100020004A23Q000D00012Q001C000100014Q00682Q0100024Q00D33Q00017Q00123Q0003043Q004E616D6503083Q0047656D4D6F64656C030B3Q0042696747656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903083Q004D6573685061727403083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C6173746963030C3Q005472616E73706172656E6379028Q0003043Q004E656F6E01503Q0006E93Q0004000100010004A23Q000400012Q00AA00016Q00682Q0100023Q0020742Q013Q000100260C00010011000100020004A23Q001100010020742Q013Q000100260C00010011000100030004A23Q001100010012F3000100043Q0020A500010001000500202Q00023Q000100122Q000300066Q00010003000200044Q001200012Q00612Q016Q00AA000100013Q0006E900010016000100010004A23Q001600012Q00AA00026Q0068010200023Q00201B01023Q0007001227000400084Q00420102000400020006750102001D00013Q0004A23Q001D00010006090002002000013Q0004A23Q0020000100201B01023Q0009001227000400084Q00420102000400020006750102004D00013Q0004A23Q004D000100207401030002000A00207401030003000B2Q007F00045Q00069901030029000100040004A23Q002900012Q00AA00036Q0068010300023Q00201B0103000200070012270005000C4Q00420103000500020006E900030031000100010004A23Q0031000100201B010300020007001227000500084Q004201030005000200207401040002000D0012F30005000E3Q00207401050005000D00207401050005000F0006680004003A000100050004A23Q003A000100207401040002001000260C0004003B000100110004A23Q003B00012Q006101046Q00AA000400013Q0020C700050002000D00122Q0006000E3Q00202Q00060006000D00202Q00060006001200062Q00050045000100060004A23Q0045000100207401050002001000260C00050046000100110004A23Q004600012Q006101056Q00AA000500013Q0006A20106004C000100030004A23Q004C00010006090006004C000100040004A23Q004C00012Q0010000600054Q0068010600024Q00AA00036Q0068010300024Q00D33Q00017Q000F3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403043Q006D61746803043Q006875676503103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03093Q004D61676E697475646500504Q007F7Q002074014Q0001000675012Q000900013Q0004A23Q0009000100201B2Q013Q0002001227000300034Q00422Q01000300020006E90001000B000100010004A23Q000B00012Q001C000100014Q00682Q0100023Q0020742Q013Q00032Q003F010200023Q00122Q000300043Q00202Q0003000300054Q00048Q000500013Q00202Q00050005000200122Q000700066Q00050007000200062Q0005001B00013Q0004A23Q001B00010012F3000600073Q0020740106000600082Q0010000700044Q0010000800054Q00B80006000800012Q007F000600024Q00B70006000100020006750106002400013Q0004A23Q002400010012F3000700073Q0020740107000700082Q0010000800044Q0010000900064Q00B80007000900010012F3000700094Q0010000800044Q006E0107000200090004A23Q004C00010012F3000C00093Q00201B010D000B000A2Q00A6000D000E4Q008C010C3Q000E0004A23Q004A00012Q007F001100034Q0010001200104Q00FF0011000200020006750111004A00013Q0004A23Q004A000100201B01110010000B0012270013000C4Q00420111001300020006750111003900013Q0004A23Q003900010006090011003C000100100004A23Q003C000100201B01110010000D0012270013000C4Q00420111001300020006750111004A00013Q0004A23Q004A00012Q007F001200044Q00300012001200110006E90012004A000100010004A23Q004A000100207401120001000E00207401130011000E2Q00B900120012001300207401120012000F0006990112004A000100030004A23Q004A00012Q0010000300124Q0010000200113Q0006FE000C002D000100020004A23Q002D00010006FE00070028000100020004A23Q002800012Q0068010200024Q00D33Q00017Q001B3Q0003043Q006D61746803043Q006875676503093Q004D696E486569676874030E3Q0046696E6446697273744368696C6403103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q004D6573685061727403043Q004E616D6503083Q0047656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D03083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C617374696303043Q004E656F6E030E3Q0052656E646572466964656C69747903073Q0050726563697365030C3Q005472616E73706172656E6379028Q0003083Q00506F736974696F6E03013Q005903093Q004D61676E697475646500674Q007F8Q00B73Q000100020006E93Q0006000100010004A23Q000600012Q001C000100014Q00682Q0100024Q001C000100013Q001211010200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q00048Q000500023Q00202Q00050005000400122Q000700056Q00050007000200062Q0005001700013Q0004A23Q001700010012F3000600063Q0020740106000600072Q0010000700044Q0010000800054Q00B80006000800012Q007F000600034Q00B70006000100020006750106002000013Q0004A23Q002000010012F3000700063Q0020740107000700072Q0010000800044Q0010000900064Q00B80007000900010012F3000700084Q0010000800044Q006E0107000200090004A23Q006300010012F3000C00083Q00201B010D000B00092Q00A6000D000E4Q008C010C3Q000E0004A23Q0061000100201B01110010000A0012270013000B4Q00420111001300020006750111006100013Q0004A23Q0061000100207401110010000C00260C001100380001000D0004A23Q003800010012F30011000E3Q0020E800110011000F00202Q00120010000C00122Q001300106Q00110013000200062Q0011006100013Q0004A23Q006100010020740111001000110012F3001200123Q0020740112001200110020740112001200130006710011003F000100120004A23Q003F00012Q006101116Q00AA001100013Q0020C700120010001100122Q001300123Q00202Q00130013001100202Q00130013001400062Q0012004F000100130004A23Q004F00010020740112001000150012F3001300123Q0020740113001300150020740113001300160006680012004F000100130004A23Q004F000100207401120010001700260C00120050000100180004A23Q005000012Q006101126Q00AA001200013Q0006E900110055000100010004A23Q005500010006750112006100013Q0004A23Q0061000100207401130010001900207401130013001A00069901030061000100130004A23Q0061000100207401130010001900207401143Q00192Q00B900130013001400207401130013001B00069901130061000100020004A23Q006100012Q0010000200134Q0010000100103Q0006FE000C0029000100020004A23Q002900010006FE00070024000100020004A23Q002400012Q00682Q0100024Q00D33Q00017Q000C3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203083Q004B4F54484172656103043Q0052696E672Q033Q0049734103083Q00426173655061727403063Q00434672616D6503053Q004D6F64656C03083Q004765745069766F74003F3Q00121A3Q00013Q00206Q000200122Q000200038Q0002000200064Q000B00013Q0004A23Q000B00010012F33Q00013Q002074014Q000300201B014Q0002001227000200044Q0042012Q000200020006A22Q01001000013Q0004A23Q0010000100201B2Q013Q0002001227000300054Q00422Q01000300020006A201020015000100010004A23Q0015000100201B010200010002001227000400064Q00420102000400020006750102003C00013Q0004A23Q003C000100201B010300020002001227000500074Q00420103000500020006750103002C00013Q0004A23Q002C000100201B010400030008001227000600094Q00420104000600020006750104002400013Q0004A23Q0024000100207401040003000A2Q0068010400023Q0004A23Q002C000100201B0104000300080012270006000B4Q00420104000600020006750104002C00013Q0004A23Q002C000100201B01040003000C2Q0020010400054Q008C00045Q00201B010400020008001227000600094Q00420104000600020006750104003400013Q0004A23Q0034000100207401040002000A2Q0068010400023Q0004A23Q003C000100201B0104000200080012270006000B4Q00420104000600020006750104003C00013Q0004A23Q003C000100201B01040002000C2Q0020010400054Q008C00046Q001C000300034Q0068010300024Q00D33Q00017Q00013Q00030D3Q0050726553696D756C6174696F6E00074Q007F8Q007F000100013Q0020742Q010001000100069001023Q000100012Q007F3Q00024Q00B83Q000200012Q00D33Q00013Q00013Q00133Q0003093Q0043686172616374657203073Q0067657467656E7603063Q004E6F636C697003063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C696465010003153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C756500334Q007F7Q002074014Q00010006E93Q0005000100010004A23Q000500012Q00D33Q00013Q0012F3000100024Q00B70001000100020020742Q01000100030006752Q01001A00013Q0004A23Q001A00010012F3000100043Q00201B01023Q00052Q00A6000200034Q008C2Q013Q00030004A23Q0018000100201B010600050006001227000800074Q00420106000800020006750106001800013Q0004A23Q001800010020740106000500080006750106001800013Q0004A23Q001800010030460005000800090006FE0001000F000100020004A23Q000F000100201B2Q013Q000A0012270003000B4Q00422Q01000300020006752Q01003200013Q0004A23Q003200010012F3000200024Q00B700020001000200207401020002000C0006750102002800013Q0004A23Q002800010012F3000200024Q00B700020001000200207401020002000E0010182Q01000D00020012F3000200024Q00B700020001000200207401020002000F0006750102003200013Q0004A23Q003200010030460001001000110012F3000200024Q00B70002000100020020740102000200130010182Q01001200022Q00D33Q00017Q00093Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030B3Q004368616E6765537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503073Q004A756D70696E6700143Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001300013Q0004A23Q001300012Q007F7Q002074014Q00030006A22Q01000C00013Q0004A23Q000C000100201B2Q013Q0004001227000300054Q00422Q01000300020006752Q01001300013Q0004A23Q0013000100201B0102000100060012F3000400073Q0020740104000400080020740104000400092Q00B80002000400012Q00D33Q00017Q00093Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E563203043Q007461736B03043Q0077616974027Q004003083Q0054656C65706F727403043Q0067616D6503073Q00506C616365496400153Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001400013Q0004A23Q001400010012F33Q00014Q00B73Q00010002002074014Q00030006E93Q0014000100010004A23Q001400010012F33Q00043Q002012014Q000500122Q000100068Q000200019Q0000206Q000700122Q000200083Q00202Q0002000200094Q000300018Q000300012Q00D33Q00017Q001B3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C6964650100030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00476574537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503083Q0046722Q6566612Q6C03163Q00412Q73656D626C794C696E65617256656C6F6369747903013Q0059026Q00344003073Q00566563746F72332Q033Q006E657703013Q0058026Q0049C003013Q005A026Q004EC0026Q0034C000453Q0012F33Q00014Q00B73Q00010002002074014Q00020006E93Q0006000100010004A23Q000600012Q00D33Q00014Q007F7Q002074014Q00030006E93Q000B000100010004A23Q000B00012Q00D33Q00013Q0012F3000100043Q00201B01023Q00052Q00A6000200034Q008C2Q013Q00030004A23Q0016000100201B010600050006001227000800074Q00420106000800020006750106001600013Q0004A23Q001600010030460005000800090006FE00010010000100020004A23Q0010000100201B2Q013Q000A0012760103000B6Q00010003000200202Q00023Q000C00122Q0004000D6Q00020004000200062Q0001004400013Q0004A23Q004400010006750102004400013Q0004A23Q0044000100201B01030002000E2Q00FF0003000200020012F30004000F3Q0020740104000400100020740104000400110006710003002D000100040004A23Q002D0001002074010300010012002074010300030013000EA700140037000100030004A23Q003700010012F3000300153Q00200A01030003001600202Q00040001001200202Q00040004001700122Q000500183Q00202Q00060001001200202Q0006000600194Q00030006000200102Q00010012000300044Q00440001002074010300010012002074010300030013002613000300440001001A0004A23Q004400010012F3000300153Q0020740103000300160020740104000100120020740104000400170012270005001B3Q0020740106000100120020740106000600192Q00420103000600020010182Q01001200032Q00D33Q00017Q000B3Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030A3Q0053702Q656456616C756500224Q007F7Q002074014Q0001000675012Q002100013Q0004A23Q002100010012F33Q00023Q0020EA5Q000300122Q000100048Q0002000100124Q00058Q0001000200206Q000600066Q00013Q0004A25Q00012Q007F3Q00013Q002074014Q00070006A22Q01001400013Q0004A23Q0014000100201B2Q013Q0008001227000300094Q00422Q01000300020006752Q013Q00013Q0004A25Q000100207401020001000A0012F3000300054Q00B700030001000200207401030003000B00067100023Q000100030004A25Q00010012F3000200054Q00B700020001000200207401020002000B0010182Q01000A00020004A25Q00012Q00D33Q00017Q00293Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F7003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00506F736974696F6E03073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00184003023Q006F7303053Q00636C6F636B026Q0008402Q0103043Q007461736B03053Q0064656C6179026Q004E40026Q00E03F03043Q00556E697403043Q004C65727003043Q006D61746803053Q00636C616D70026Q002440026Q00F03F03043Q004D6F7665026Q000C4003063Q00434672616D6503063Q006C2Q6F6B417403013Q0059026Q002040026Q00104003113Q0066697265746F756368696E74657265737403043Q007A65726F01A93Q0012F3000100014Q00B70001000100020020742Q01000100020006E900010006000100010004A23Q000600012Q00D33Q00013Q0012F3000100014Q00B70001000100020020742Q01000100030006E900010015000100010004A23Q001500010012F3000100014Q00B70001000100020020742Q01000100040006E900010015000100010004A23Q001500010012F3000100014Q00B70001000100020020742Q01000100050006752Q01001C00013Q0004A23Q001C00012Q007F00016Q003E0001000100020006752Q01001C00013Q0004A23Q001C00010006750102001C00013Q0004A23Q001C00012Q00D33Q00014Q007F000100013Q0020742Q01000100060006E900010021000100010004A23Q002100012Q00D33Q00013Q00201B010200010007001276010400086Q00020004000200202Q00030001000900122Q0005000A6Q00030005000200062Q000200A800013Q0004A23Q00A80001000675010300A800013Q0004A23Q00A800012Q007F000400024Q00B70004000100020006750104009700013Q0004A23Q0097000100207401050004000B00207401060002000B2Q00B90005000500060012F30006000C3Q00207401060006000D00207401070005000E0012270008000F3Q0020740109000500102Q004201060009000200207401070006001100266D00070059000100120004A23Q005900012Q007F000800033Q00067100080045000100040004A23Q004500012Q0010000800043Q0012F3000900133Q0020740109000900142Q00B70009000100022Q0003000900044Q0003000800033Q0004A23Q005B00010012F3000800133Q0020880008000800144Q0008000100024Q000900046Q000800080009000E2Q0015005B000100080004A23Q005B00012Q007F000800053Q00206C0108000400160012F3000800173Q002074010800080018001227000900193Q000690010A3Q000100022Q007F3Q00054Q00103Q00044Q00830008000A00014Q000800086Q000800038Q00013Q00044Q005B00012Q001C000800084Q0003000800033Q000EA7001A0087000100070004A23Q0087000100207401080006001B2Q007F000900063Q00201B01090009001C2Q0010000B00083Q0012F3000C001D3Q002074010C000C001E00205C010D3Q001F001227000E000F3Q001227000F00204Q007C010C000F4Q008100093Q00022Q0003000900063Q0020A30009000300214Q000B00066Q000C8Q0009000C0001000E2Q00220087000100070004A23Q008700010012F3000900233Q00200600090009002400202Q000A0002000B00122Q000B000C3Q00202Q000B000B000D00202Q000C0004000B00202Q000C000C000E00202Q000D0002000B00202Q000D000D002500202Q000E0004000B00202Q000E000E00104Q000B000E6Q00093Q000200202Q000A0002002300202Q000A000A001C4Q000C00093Q00122Q000D001D3Q00202Q000D000D001E00202Q000E3Q002600122Q000F000F3Q00122Q001000206Q000D00106Q000A3Q000200102Q00020023000A00266D000700A7000100270004A23Q00A700010012F3000800283Q000675010800A700013Q0004A23Q00A700010012F3000800284Q0079000900026Q000A00043Q00122Q000B000F6Q0008000B000100122Q000800286Q000900026Q000A00043Q00122Q000B00206Q0008000B000100044Q00A700012Q007F000500063Q00201B01050005001C0012F30007000C3Q0020740107000700290012F30008001D3Q00207401080008001E00205C01093Q0026001227000A000F3Q001227000B00204Q007C0108000B4Q008100053Q00022Q0003000500063Q00201B0105000300212Q007F000700064Q00AA00086Q00B80005000800012Q006D01046Q00D33Q00013Q00013Q00015Q00044Q007F8Q007F000100013Q00206C012Q000100012Q00D33Q00017Q00153Q0003083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03053Q00416C697665026Q00444003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00104003063Q004D6F7665546F03043Q007461736B03043Q0077616974029A5Q99B93F03043Q004D6F766503043Q007A65726F024D3Q00207401023Q00010012F3000300023Q0020740103000300032Q00B70003000100022Q0010000400014Q00B70004000100020006750104003A00013Q0004A23Q003A00012Q007F00045Q0020740104000400040006750104003A00013Q0004A23Q003A00010012F3000400023Q0020740104000400032Q00B70004000100022Q00B90004000400030026130004003A000100050004A23Q003A00012Q007F000400014Q00B70004000100022Q007F000500023Q0020740105000500060006750105001D00013Q0004A23Q001D00012Q007F000500023Q00207401050005000600201B010500050007001227000700084Q00420105000700020006750104003A00013Q0004A23Q003A00010006E900050022000100010004A23Q002200010004A23Q003A00010012F3000600093Q00207401060006000A00207401070002000B00207401080004000100207401080008000B2Q00B90007000700080012270008000C3Q00207401090002000D002074010A00040001002074010A000A000D2Q00B900090009000A2Q004201060009000200207401060006000E00266D000600320001000F0004A23Q003200010004A23Q003A000100201B0107000500102Q0070000900026Q00070009000100122Q000700113Q00202Q00070007001200122Q000800136Q00070002000100044Q000400012Q007F000400023Q0020740104000400060006750104004300013Q0004A23Q004300012Q007F000400023Q00207401040004000600201B010400040007001227000600084Q00420104000600020006750104004900013Q0004A23Q0049000100201B0105000400140012F3000700093Q0020740107000700152Q00B80005000700012Q0010000500014Q0031010500014Q008C00056Q00D33Q00017Q000C3Q0003063Q0069706169727303073Q0067657467656E76030E3Q0046696E6446697273744368696C642Q033Q0049734103083Q00426173655061727403063Q00434672616D6503083Q004765745069766F74028Q0003043Q007461736B03043Q0077616974029A5Q99B93F010003363Q0012F3000300014Q0010000400014Q006E0103000200050004A23Q003000010012F3000800024Q00B70008000100022Q00300008000800020006E90008000A000100010004A23Q000A00010004A23Q0032000100201B01083Q00032Q0010000A00074Q00420108000A00020006750108003000013Q0004A23Q0030000100201B010900080004001227000B00054Q00420109000B00020006750109001700013Q0004A23Q001700010020740109000800060006E900090019000100010004A23Q0019000100201B0109000800072Q00FF0009000200022Q007F000A6Q0010000B00093Q000690010C3Q000100012Q00103Q00024Q0042010A000C00020006E9000A0021000100010004A23Q002100010004A23Q00320001001227000A00084Q007F000B00013Q000699010A00300001000B0004A23Q003000010012F3000B00024Q00B7000B000100022Q0030000B000B0002000675010B003000013Q0004A23Q003000010012F3000B00093Q002074010B000B000A001227000C000B4Q000E000B00020001002044000A000A000B0004A23Q002200010006FE00030004000100020004A23Q000400010012F3000300024Q00B700030001000200206C01030002000C2Q00D33Q00013Q00013Q00013Q0003073Q0067657467656E7600063Q0012DE3Q00018Q000100024Q00019Q003Q00016Q00028Q00017Q00043Q0003073Q0067657467656E7603083Q004175746F4C69667403043Q007461736B03053Q00737061776E010D3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000C00013Q0004A23Q000C00010012F3000100033Q0020742Q010001000400069001023Q000100032Q007F8Q007F3Q00014Q007F3Q00024Q000E0001000200012Q00D33Q00013Q00013Q000F3Q0003053Q007063612Q6C03073Q0067657467656E7603083Q004175746F4C69667403093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03163Q0046696E6446697273744368696C64576869636849734103083Q0048756D616E6F696403093Q004571756970542Q6F6C030A3Q004669726553657276657203043Q007461736B03043Q0077616974029A5Q99B93F00333Q0012F33Q00013Q0006902Q013Q000100012Q007F8Q000E3Q000200010012F33Q00024Q00B73Q00010002002074014Q0003000675012Q003200013Q0004A23Q003200012Q007F3Q00013Q00200F5Q00044Q000100013Q00202Q00010001000500122Q000300066Q00010003000200064Q002100013Q0004A23Q002100010006752Q01002100013Q0004A23Q0021000100201B01023Q0007001227000400084Q00420102000400020006E900020021000100010004A23Q0021000100201B010300010009001227000500084Q00420103000500020006750103002100013Q0004A23Q0021000100207401043Q000A00201B01040004000B2Q0010000600034Q00B80004000600012Q007F000200023Q0006750102002800013Q0004A23Q002800012Q007F000200023Q00201B01020002000C2Q000E0002000200010004A23Q002C00010012F3000200013Q00069001030001000100012Q00108Q000E0002000200010012F30002000D3Q00203100020002000E00122Q0003000F6Q0002000200019Q0000044Q000400012Q00D33Q00013Q00023Q00083Q00030C3Q0053656E644B65794576656E7403043Q00456E756D03073Q004B6579436F64652Q033Q004F6E6503043Q0067616D6503043Q007461736B03043Q0077616974029A5Q99A93F00174Q007F7Q00201B014Q00012Q00AA000200013Q0012F3000300023Q0020740103000300030020740103000300042Q00AA00045Q0012F3000500054Q00B83Q000500010012F33Q00063Q002074014Q0007001227000100084Q000E3Q000200012Q007F7Q00201B014Q00012Q00AA00025Q0012F3000300023Q0020740103000300030020740103000300042Q00AA00045Q0012F3000500054Q00B83Q000500012Q00D33Q00017Q00033Q0003153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03083Q004163746976617465000C4Q007F7Q000675012Q000700013Q0004A23Q000700012Q007F7Q00201B014Q0001001227000200024Q0042012Q00020002000675012Q000B00013Q0004A23Q000B000100201B2Q013Q00032Q000E0001000200012Q00D33Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F50756E636803043Q007461736B03053Q00737061776E010B3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000A00013Q0004A23Q000A00010012F3000100033Q0020742Q010001000400069001023Q000100012Q007F8Q000E0001000200012Q00D33Q00013Q00013Q00083Q0003073Q0067657467656E7603093Q004175746F50756E6368030A3Q004669726553657276657203053Q0050756E6368026Q00F03F03043Q007461736B03043Q0077616974029A5Q99A93F00133Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001200013Q0004A23Q001200012Q007F7Q000675012Q000D00013Q0004A23Q000D00012Q007F7Q00201B014Q0003001227000200043Q001227000300054Q00B83Q000300010012F33Q00063Q002074014Q0007001227000100084Q000E3Q000200010004A25Q00012Q00D33Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F53746F6D7003043Q007461736B03053Q00737061776E010B3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000A00013Q0004A23Q000A00010012F3000100033Q0020742Q010001000400069001023Q000100012Q007F8Q000E0001000200012Q00D33Q00013Q00013Q00073Q0003073Q0067657467656E7603093Q004175746F53746F6D70030A3Q004669726553657276657203053Q0053746F6D7003043Q007461736B03043Q0077616974029A5Q99A93F00123Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001100013Q0004A23Q001100012Q007F7Q000675012Q000C00013Q0004A23Q000C00012Q007F7Q00201B014Q0003001227000200044Q00B83Q000200010012F33Q00053Q002074014Q0006001227000100074Q000E3Q000200010004A25Q00012Q00D33Q00017Q00083Q0003073Q0067657467656E76030B3Q004175746F41697264726F70030F3Q004175746F54652Q7269746F72696573010003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01153Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q001400013Q0004A23Q001400010012F3000100014Q002A2Q010001000200302Q00010003000400122Q000100053Q00202Q0001000100064Q00028Q00010002000100122Q000100073Q00202Q00010001000800069001023Q000100042Q007F3Q00014Q007F3Q00024Q007F8Q007F3Q00034Q000E0001000200012Q00D33Q00013Q00013Q000D3Q0003073Q0067657467656E76030B3Q004175746F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F030C3Q004175746F47656D54772Q656E03063Q00434672616D652Q033Q006E6577028Q00026Q000840026Q002E402Q01029A5Q99C93F003D3Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q003C00013Q0004A23Q003C00010012F33Q00033Q002098014Q000400122Q000100058Q000200019Q006Q000100024Q000100016Q00010001000200062Q00013Q00013Q0004A25Q000100067501023Q00013Q0004A25Q0001000675014Q00013Q0004A25Q00010012F3000300014Q00B70003000100020020740103000300060006E900033Q000100010004A25Q00010020740103000200070012F3000400073Q00200B01040004000800122Q000500093Q00122Q0006000A3Q00122Q000700096Q0004000700024Q00030003000400104Q000700030012F3000300033Q0020740103000300040012270004000B4Q00200003000200014Q000300023Q00202Q00030001000C4Q000300036Q0003000100024Q00048Q00040001000200062Q00033Q00013Q0004A25Q000100067501043Q00013Q0004A25Q00010012F3000500073Q00200B01050005000800122Q000600093Q00122Q0007000A3Q00122Q000800096Q0005000800024Q00050003000500102Q0004000700050012CA000500033Q00202Q00050005000400122Q0006000D6Q00050002000100046Q00012Q00D33Q00017Q00083Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F72696573030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E67030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01143Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q001300013Q0004A23Q001300010012F3000100014Q006200010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q00010001000800069001023Q000100012Q007F8Q000E0001000200012Q00D33Q00013Q00013Q000A3Q0003023Q00543103023Q00543203023Q00543303023Q00543403023Q00543503093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0054652Q7269746F72696573030F3Q004175746F54652Q7269746F72696573001A4Q004F3Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500010012F3000100063Q00201B2Q0100010007001227000300084Q00422Q01000300020006752Q01001200013Q0004A23Q001200010012F3000100063Q0020742Q010001000800201B2Q0100010007001227000300094Q00422Q01000300020006752Q01001900013Q0004A23Q001900012Q007F00026Q0010000300014Q001000045Q0012270005000A4Q00B80002000500012Q00D33Q00017Q00063Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01123Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q001100013Q0004A23Q001100010012F3000100033Q00208D0001000100044Q00028Q00010002000100122Q000100053Q00202Q00010001000600069001023Q000100042Q007F3Q00014Q007F3Q00024Q007F3Q00034Q007F8Q000E0001000200012Q00D33Q00013Q00013Q00133Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00506172656E7403083Q00506F736974696F6E03093Q004D61676E6974756465026Q00144003063Q004D6F7665546F029A5Q99C93F03043Q004D6F766503073Q00566563746F723303043Q007A65726F026Q002E402Q01004C3Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q004B00013Q0004A23Q004B00010012F33Q00033Q002072014Q000400122Q000100058Q000200019Q0000206Q00064Q000100016Q00010001000200062Q0002001200013Q0004A23Q0012000100201B01023Q0007001227000400084Q00420102000400022Q007F000300024Q003E00030001000400067501033Q00013Q0004A25Q000100067501043Q00013Q0004A25Q00010006752Q013Q00013Q0004A25Q000100067501023Q00013Q0004A25Q00010012F3000500014Q00B70005000100020020740105000500020006750105003200013Q0004A23Q003200010020740105000300090006750105003200013Q0004A23Q0032000100207401050001000A00207401060004000A2Q00B900050005000600207401050005000B000EA7000C0032000100050004A23Q0032000100201B01050002000D00207401070004000A2Q00B80005000700010012CA000500033Q00202Q00050005000400122Q0006000E6Q00050002000100044Q001C00010012F3000500014Q00B700050001000200207401050005000200067501053Q00013Q0004A25Q000100207401050003000900067501053Q00013Q0004A25Q000100207401050001000A00207401060004000A2Q00B900050005000600207401050005000B00266D00053Q0001000C0004A25Q000100201B01050002000F001287000700103Q00202Q0007000700114Q00050007000100122Q000500033Q00202Q00050005000400122Q000600126Q0005000200014Q000500033Q00202Q00050003001300046Q00012Q00D33Q00017Q000D3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E6703093Q0057616C6B53702Q6564030A3Q0053702Q656456616C756503043Q004D6F766503073Q00566563746F723303043Q007A65726F01223Q0012082Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004A23Q000A000100201B010200010004001227000400054Q0042010200040002000675012Q001900013Q0004A23Q001900010012F3000300014Q00B70003000100020030460003000600070012F3000300014Q00B70003000100020030460003000800070006750102002100013Q0004A23Q002100010012F3000300014Q00B700030001000200207401030003000A0010180102000900030004A23Q002100010006750102002100013Q0004A23Q0021000100201B01030002000B00120C0105000C3Q00202Q00050005000D4Q0003000500014Q000300013Q00102Q0002000900032Q00D33Q00017Q00073Q0003073Q0067657467656E76030A3Q0053702Q656456616C7565030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q0012C0000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q0004A23Q001200012Q007F00015Q0020742Q01000100040006A20102000F000100010004A23Q000F000100201B010200010005001227000400064Q00420102000400020006750102001200013Q0004A23Q00120001001018010200074Q00D33Q00017Q00023Q0003073Q0067657467656E76030D3Q0047656D54772Q656E53702Q656401043Q0012F3000100014Q00B70001000100020010182Q0100024Q00D33Q00017Q00093Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03063Q004E6F636C6970030C3Q004175746F47656D4272696E670100030B3Q004175746F47656D57616C6B030F3Q004175746F54652Q7269746F7269657303043Q007461736B03053Q00737061776E011C3Q0012F3000100014Q00B70001000100020010182Q0100023Q0012F3000100014Q00B70001000100020010182Q0100033Q000675012Q001B00013Q0004A23Q001B00010012F3000100014Q006200010001000200302Q00010004000500122Q000100016Q00010001000200302Q00010006000500122Q000100016Q00010001000200302Q00010007000500122Q000100083Q00202Q00010001000900069001023Q000100062Q007F8Q007F3Q00014Q007F3Q00024Q007F3Q00034Q007F3Q00044Q007F3Q00054Q000E0001000200012Q00D33Q00013Q00013Q00153Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03093Q0048656172746265617403043Q0057616974030B3Q004175746F41697264726F7003063Q00434672616D652Q033Q006E6577028Q00026Q00084003163Q00412Q73656D626C794C696E65617256656C6F6369747903073Q00566563746F723303173Q00412Q73656D626C79416E67756C617256656C6F6369747903043Q007461736B03043Q0077616974026Q002E402Q01029A5Q99C93F03083Q00506F736974696F6E030D3Q0047656D54772Q656E53702Q656403093Q004D61676E697475646503043Q00556E697400703Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q006F00013Q0004A23Q006F00012Q007F7Q002006014Q000300206Q00046Q000200024Q000100016Q00010001000200062Q00013Q00013Q0004A25Q00012Q007F000200024Q008601020001000300122Q000400016Q00040001000200202Q00040004000500062Q0004004A00013Q0004A23Q004A00010006750102004A00013Q0004A23Q004A00010006750103004A00013Q0004A23Q004A0001002074010400030006001243000500063Q00202Q00050005000700122Q000600083Q00122Q000700093Q00122Q000800086Q0005000800024Q00040004000500102Q00010006000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000A000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000C000400122Q0004000D3Q00202Q00040004000E00122Q0005000F6Q0004000200014Q000400033Q00202Q0004000200104Q000400046Q0004000100024Q000500016Q00050001000200062Q00043Q00013Q0004A25Q000100067501053Q00013Q0004A25Q00010012F3000600063Q00200B01060006000700122Q000700083Q00122Q000800093Q00122Q000900086Q0006000900024Q00060004000600102Q0005000600060012CA0006000D3Q00202Q00060006000E00122Q000700116Q00060002000100046Q00012Q007F000400054Q00B700040001000200067501043Q00013Q0004A25Q00010020740105000400120020020106000100124Q00050005000600122Q000600016Q00060001000200202Q0006000600134Q000600063Q00202Q00070005001400062Q0007005B000100060004A23Q005B00010020740107000400060010182Q01000600070004A23Q006000010020740107000100060020740108000500152Q00450008000800062Q00520107000700080010182Q01000600070012F30007000B3Q0020A101070007000700122Q000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000A000700122Q0007000B3Q00202Q00070007000700122Q000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000C000700046Q00012Q00D33Q00017Q000E3Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03053Q00737061776E03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F726564010001273Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100012Q007F8Q000E0001000200010004A23Q002600010012F3000100053Q00201B2Q0100010006001227000300074Q00422Q01000300020006752Q01002600013Q0004A23Q002600010012F3000100083Q001288010200053Q00202Q00020002000700202Q0002000200094Q000200036Q00013Q000300044Q0024000100201B01060005000A0012270008000B4Q00420106000800020006750106002400013Q0004A23Q0024000100201B0106000500060012270008000C4Q00420106000800020006750106002400013Q0004A23Q0024000100207401060005000C0030460006000D000E0006FE00010018000100020004A23Q001800012Q00D33Q00013Q00013Q00143Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403063Q00434672616D652Q033Q006E6577028Q00026Q001AC0026Q001EC003083Q00416E63686F7265642Q0100343Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q003300013Q0004A23Q003300010012F33Q00033Q002074014Q0004001227000100054Q000E3Q000200012Q007F8Q00B73Q00010002000675014Q00013Q0004A25Q00010012F3000100063Q00201B2Q0100010007001227000300084Q00422Q01000300020006752Q013Q00013Q0004A25Q00010012F3000100093Q001288010200063Q00202Q00020002000800202Q00020002000A4Q000200036Q00013Q000300044Q0030000100201B01060005000B0012270008000C4Q00420106000800020006750106003000013Q0004A23Q0030000100201B0106000500070012270008000D4Q00420106000800020006750106003000013Q0004A23Q0030000100207401060005000D00201301073Q000E00122Q0008000E3Q00202Q00080008000F00122Q000900103Q00122Q000A00113Q00122Q000B00126Q0008000B00024Q00070007000800102Q0006000E000700202Q00060005000D00302Q0006001300140006FE0001001A000100020004A23Q001A00010004A25Q00012Q00D33Q00017Q00043Q0003073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303043Q007461736B03053Q00737061776E010C3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100022Q007F8Q007F3Q00014Q000E0001000200012Q00D33Q00013Q00013Q001B3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q0052696768744C6F7765724C656703103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E657703083Q00506F736974696F6E03073Q00566563746F7233026Q002E40027Q0040028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004D6F7665546F00723Q0012783Q00013Q00206Q000200122Q000200038Q0002000200064Q0007000100010004A23Q000700012Q00D33Q00014Q001C000100013Q0012B2000200043Q00202Q00033Q00054Q000300046Q00023Q000400044Q0023000100201B010700060006001227000900074Q00420107000900020006750107002300013Q0004A23Q0023000100201B010700060002001227000900084Q004201070009000200060900010020000100070004A23Q0020000100201B010700060002001227000900094Q004201070009000200060900010020000100070004A23Q0020000100201B01070006000A0012270009000B4Q00420107000900022Q0010000100073Q0006752Q01002300013Q0004A23Q002300010004A23Q002500010006FE0002000D000100020004A23Q000D00012Q007F00026Q00B70002000100020006750102003B00013Q0004A23Q003B00010006752Q01003B00013Q0004A23Q003B00010012F30003000C3Q0020A601030003000D00202Q00040001000E00122Q0005000F3Q00202Q00050005000D00122Q000600103Q00122Q000700113Q00122Q000800126Q0005000800024Q0004000400054Q0003000200020010180102000C00030012F3000300133Q002074010300030014001227000400154Q000E0003000200010012F3000300164Q00B70003000100020020740103000300170006750103007100013Q0004A23Q007100010012F3000300133Q0020E200030003001400122Q000400156Q0003000200014Q000300013Q00202Q00030003001800062Q0004004B000100030004A23Q004B000100201B0104000300190012270006001A4Q00420104000600022Q001C000500053Q0012B2000600043Q00202Q00073Q00054Q000700086Q00063Q000800044Q0067000100201B010B000A0006001227000D00074Q0042010B000D0002000675010B006700013Q0004A23Q0067000100201B010B000A0002001227000D00084Q0042010B000D0002000609000500640001000B0004A23Q0064000100201B010B000A0002001227000D00094Q0042010B000D0002000609000500640001000B0004A23Q0064000100201B010B000A000A001227000D000B4Q0042010B000D00022Q00100005000B3Q0006750105006700013Q0004A23Q006700010004A23Q006900010006FE00060051000100020004A23Q005100010006750104003B00013Q0004A23Q003B00010006750105003B00013Q0004A23Q003B000100201B01060004001B00207401080005000E2Q00B80006000800010004A23Q003B00012Q00D33Q00017Q00063Q0003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C030A3Q0057616C6B546F426F2Q73010003043Q007461736B03053Q00737061776E010E3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000D00013Q0004A23Q000D00010012F3000100014Q00B70001000100020030460001000300040012F3000100053Q0020742Q010001000600069001023Q000100012Q007F8Q000E0001000200012Q00D33Q00013Q00013Q00153Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03043Q007461736B03043Q0077616974027B14AE47E17A843F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E6577028Q00026Q000C4003083Q0056656C6F6369747903073Q00566563746F723300413Q0012783Q00013Q00206Q000200122Q000200038Q0002000200064Q0007000100010004A23Q000700012Q00D33Q00013Q0012F3000100044Q00B70001000100020020742Q01000100050006752Q01004000013Q0004A23Q004000010012F3000100063Q0020512Q010001000700122Q000200086Q0001000200014Q00018Q0001000100024Q000200023Q00122Q000300093Q00202Q00043Q000A4Q000400056Q00033Q000500044Q0029000100201B01080007000B001227000A000C4Q00420108000A00020006750108002900013Q0004A23Q0029000100201B010800070002001227000A000D4Q00420108000A000200060900020026000100080004A23Q0026000100201B01080007000E001227000A000F4Q00420108000A00022Q0010000200083Q0006750102002900013Q0004A23Q002900010004A23Q002B00010006FE00030018000100020004A23Q001800010006752Q01000700013Q0004A23Q000700010006750102000700013Q0004A23Q0007000100207401030002001000125A010400103Q00202Q00040004001100122Q000500123Q00122Q000600123Q00122Q000700136Q0004000700024Q00030003000400102Q00010010000300122Q000300153Q00202Q00030003001100122Q000400123Q00122Q000500123Q00122Q000600126Q00030006000200102Q00010014000300044Q000700012Q00D33Q00017Q00023Q0003073Q0067657467656E7603103Q0053656C6563746564452Q67496E64657801043Q0012F3000100014Q00B70001000100020010182Q0100024Q00D33Q00017Q00043Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q6703043Q007461736B03053Q00737061776E010D3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000C00013Q0004A23Q000C00010012F3000100033Q0020742Q010001000400069001023Q000100032Q007F8Q007F3Q00014Q007F3Q00024Q000E0001000200012Q00D33Q00013Q00013Q000B3Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q003000013Q0004A23Q003000012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002A00013Q0004A23Q002A00010012F3000100014Q00B70001000100020020742Q01000100070006E900010023000100010004A23Q00230001001227000100083Q0012F3000200093Q00207401020002000A00069001033Q000100022Q00108Q00103Q00014Q000E0002000200012Q006D2Q015Q0012F3000100093Q00209B00010001000B4Q000200026Q0001000200019Q0000046Q00012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012F33Q00013Q0006902Q013Q000100022Q007F8Q007F3Q00014Q000E3Q000200012Q00D33Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00084003073Q0049736C616E647300074Q002B7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030F3Q004175746F48617463685065724F6E6503043Q007461736B03053Q00737061776E010D3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000C00013Q0004A23Q000C00010012F3000100033Q0020742Q010001000400069001023Q000100032Q007F8Q007F3Q00014Q007F3Q00024Q000E0001000200012Q00D33Q00013Q00013Q000B3Q0003073Q0067657467656E76030F3Q004175746F48617463685065724F6E65030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q003000013Q0004A23Q003000012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002A00013Q0004A23Q002A00010012F3000100014Q00B70001000100020020742Q01000100070006E900010023000100010004A23Q00230001001227000100083Q0012F3000200093Q00207401020002000A00069001033Q000100022Q00108Q00103Q00014Q000E0002000200012Q006D2Q015Q0012F3000100093Q00209B00010001000B4Q000200026Q0001000200019Q0000046Q00012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012F33Q00013Q0006902Q013Q000100022Q007F8Q007F3Q00014Q000E3Q000200012Q00D33Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00F03F03073Q0049736C616E647300074Q002B7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00023Q0003053Q007063612Q6C2Q01000C3Q0012F33Q00013Q0006902Q013Q000100012Q007F8Q006E012Q000200010006A20102000A00013Q0004A23Q000A000100260C00010009000100020004A23Q000900012Q006101026Q00AA000200014Q0068010200024Q00D33Q00013Q00013Q00043Q00030A3Q00506C617965724461746103083Q0047616D6570612Q7303053Q004F70656E3303053Q0056616C756500074Q0033016Q00206Q000100206Q000200206Q000300206Q00046Q00028Q00017Q00043Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q6703043Q007461736B03053Q00737061776E010C3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100022Q007F8Q007F3Q00014Q000E0001000200012Q00D33Q00013Q00013Q00093Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703043Q007461736B03053Q00737061776E03043Q007761697400283Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q002700013Q0004A23Q002700012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002200013Q0004A23Q002200010012F3000100073Q0020742Q010001000800069001023Q000100012Q00108Q000E0001000200010012F3000100073Q0020742Q01000100092Q00690001000100012Q006D016Q0004A25Q00012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012F33Q00013Q0006902Q013Q000100012Q007F8Q000E3Q000200012Q00D33Q00013Q00013Q00043Q00030C3Q00496E766F6B65536572766572026Q00F03F026Q00084003093Q0043756265576F726C6400074Q00657Q00206Q000100122Q000200023Q00122Q000300033Q00122Q000400048Q000400016Q00017Q00163Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001503Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q004A00013Q0004A23Q004A00010012F3000100033Q00201B2Q0100010004001227000300054Q00422Q01000300020006752Q01002100013Q0004A23Q002100010012F3000100033Q00208A2Q010001000500202Q00010001000400122Q000300066Q00010003000200062Q0001002100013Q0004A23Q002100010012F3000100033Q0020B100010001000500202Q00010001000600202Q00010001000400122Q000300076Q0001000300020006752Q01002100013Q0004A23Q002100010012F3000100033Q00200200010001000500202Q00010001000600202Q00010001000700202Q00010001000400122Q000300086Q0001000300022Q007F00026Q00B70002000100020006750102003F00013Q0004A23Q003F00010006752Q01003F00013Q0004A23Q003F000100201B0103000100090012270005000A4Q00420103000500020006750103003000013Q0004A23Q0030000100201B01030001000B2Q00FF0003000200020006E900030031000100010004A23Q0031000100207401030001000C0012F30004000C3Q0020AC00040004000D00122Q0005000E3Q00122Q0006000F3Q00122Q0007000E6Q0004000700024Q00040003000400102Q0002000C000400122Q000400103Q00202Q00040004001100122Q000500126Q00040002000100302Q00020013001400044Q004200010006750102004200013Q0004A23Q004200010030460002001300140012F3000300103Q00207401030003001500069001043Q000100032Q007F3Q00014Q007F3Q00024Q007F3Q00034Q000E0003000200010004A23Q004F00012Q007F00016Q00B70001000100020006752Q01004F00013Q0004A23Q004F00010030460001001300162Q00D33Q00013Q00013Q00083Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001F00013Q0004A23Q001F00012Q007F7Q0020855Q000300206Q00046Q000200016Q00013Q00064Q0017000100010004A23Q001700012Q007F3Q00023Q00201B014Q0005001227000200064Q0042012Q00020002000675012Q001700013Q0004A23Q001700012Q007F3Q00023Q002074014Q000600201B014Q0005001227000200074Q0042012Q00020002000675012Q001D00013Q0004A23Q001D00010012F3000100083Q00069001023Q000100012Q00108Q000E0001000200012Q006D016Q0004A25Q00012Q00D33Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q007F7Q00201B014Q00012Q000E3Q000200012Q00D33Q00017Q00043Q0003073Q0067657467656E76030E3Q004175746F4275795765696768747303043Q007461736B03053Q00737061776E010C3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100022Q007F8Q007F3Q00014Q000E0001000200012Q00D33Q00013Q00013Q000A3Q0003073Q0067657467656E76030E3Q004175746F42757957656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q002800013Q0004A23Q002800012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002200013Q0004A23Q002200010012F3000100073Q0020742Q010001000800069001023Q000100012Q00108Q000E0001000200010012F3000100073Q00203100010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012F33Q00013Q0006902Q013Q000100012Q007F8Q000E3Q000200012Q00D33Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q0057656967687403073Q0049736C616E647300064Q00A8016Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q00043Q0003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E010C3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100022Q007F8Q007F3Q00014Q000E0001000200012Q00D33Q00013Q00013Q000A3Q0003073Q0067657467656E76030A3Q004175746F427579444E41030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q002800013Q0004A23Q002800012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002200013Q0004A23Q002200010012F3000100073Q0020742Q010001000800069001023Q000100012Q00108Q000E0001000200010012F3000100073Q00203100010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00D33Q00013Q00013Q00063Q00026Q00F03F026Q005E4003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E00133Q0012273Q00013Q001227000100023Q001227000200013Q0004473Q001200010012F3000400034Q00B70004000100020020740104000400040006E90004000A000100010004A23Q000A00010004A23Q001200010012F3000400053Q00207401040004000600069001053Q000100022Q007F8Q00103Q00034Q000E0004000200012Q006D01035Q00045B3Q000400012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012F33Q00013Q0006902Q013Q000100022Q007F8Q007F3Q00014Q000E3Q000200012Q00D33Q00013Q00013Q00033Q00030C3Q00496E766F6B655365727665722Q033Q00444E4103073Q0049736C616E647300074Q002B7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E010C3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100022Q007F8Q007F3Q00014Q000E0001000200012Q00D33Q00013Q00013Q000A3Q0003073Q0067657467656E76030D3Q004175746F427579426F64696573030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q002800013Q0004A23Q002800012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002200013Q0004A23Q002200010012F3000100073Q0020742Q010001000800069001023Q000100012Q00108Q000E0001000200010012F3000100073Q00203100010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00D33Q00013Q00013Q00073Q00027Q0040025Q00802Q40026Q00F03F03073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E00133Q0012273Q00013Q001227000100023Q001227000200033Q0004473Q001200010012F3000400044Q00B70004000100020020740104000400050006E90004000A000100010004A23Q000A00010004A23Q001200010012F3000400063Q00207401040004000700069001053Q000100022Q007F8Q00103Q00034Q000E0004000200012Q006D01035Q00045B3Q000400012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012F33Q00013Q0006902Q013Q000100022Q007F8Q007F3Q00014Q000E3Q000200012Q00D33Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572030B3Q00426F64795570677261646503073Q0049736C616E647300074Q002B7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B65745765696768747303043Q007461736B03053Q00737061776E010C3Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q000B00013Q0004A23Q000B00010012F3000100033Q0020742Q010001000400069001023Q000100022Q007F8Q007F3Q00014Q000E0001000200012Q00D33Q00013Q00013Q000A3Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B657457656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q002800013Q0004A23Q002800012Q007F7Q0006E93Q001B000100010004A23Q001B00012Q007F3Q00013Q00201B014Q0003001227000200044Q0042012Q00020002000675012Q001B00013Q0004A23Q001B00012Q007F3Q00013Q00208A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004A23Q001B00012Q007F3Q00013Q0020B15Q000400206Q000500206Q000300122Q000200068Q00020002000675012Q002200013Q0004A23Q002200010012F3000100073Q0020742Q010001000800069001023Q000100012Q00108Q000E0001000200010012F3000100073Q00203100010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00D33Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012F33Q00013Q0006902Q013Q000100012Q007F8Q000E3Q000200012Q00D33Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q00576569676874030B3Q0053757065726D61726B657400064Q00A8016Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q001A3Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B657403053Q0053686F707303093Q0052696E67417265617303063Q00536572766572030B3Q0052616E676553797374656D030F3Q0053757065726D61726B657453652Q6C03043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001633Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q005D00013Q0004A23Q005D00010012F3000100033Q00201B2Q0100010004001227000300054Q00422Q01000300020006A20102000E000100010004A23Q000E000100201B010200010004001227000400064Q00420102000400022Q001C000300033Q0006750102003400013Q0004A23Q0034000100201B010400020004001227000600074Q00420104000600020006E900040019000100010004A23Q0019000100201B010400020004001227000600084Q00420104000600020006A201050029000100040004A23Q0029000100201B010500040004001227000700094Q00420105000700020006E900050029000100010004A23Q0029000100201B0105000400040012270007000A4Q00420105000700020006750105002900013Q0004A23Q0029000100207401050004000A00201B010500050004001227000700094Q00420105000700020006750105003400013Q0004A23Q0034000100201B0106000500040012270008000B4Q004201060008000200060900030034000100060004A23Q0034000100201B0106000500040012270008000C4Q00420106000800022Q0010000300064Q007F00046Q00B70004000100020006750104005200013Q0004A23Q005200010006750103005200013Q0004A23Q0052000100201B01050003000D0012270007000E4Q00420105000700020006750105004300013Q0004A23Q0043000100201B01050003000F2Q00FF0005000200020006E900050044000100010004A23Q004400010020740105000300100012F3000600103Q0020AC00060006001100122Q000700123Q00122Q000800133Q00122Q000900126Q0006000900024Q00060005000600102Q00040010000600122Q000600143Q00202Q00060006001500122Q000700166Q00060002000100302Q00040017001800044Q005500010006750104005500013Q0004A23Q005500010030460004001700180012F3000500143Q00207401050005001900069001063Q000100032Q007F3Q00014Q007F3Q00024Q007F3Q00034Q000E0005000200010004A23Q006200012Q007F00016Q00B70001000100020006752Q01006200013Q0004A23Q0062000100304600010017001A2Q00D33Q00013Q00013Q00083Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001F00013Q0004A23Q001F00012Q007F7Q0020855Q000300206Q00046Q000200016Q00013Q00064Q0017000100010004A23Q001700012Q007F3Q00023Q00201B014Q0005001227000200064Q0042012Q00020002000675012Q001700013Q0004A23Q001700012Q007F3Q00023Q002074014Q000600201B014Q0005001227000200074Q0042012Q00020002000675012Q001D00013Q0004A23Q001D00010012F3000100083Q00069001023Q000100012Q00108Q000E0001000200012Q006D016Q0004A25Q00012Q00D33Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q007F7Q00201B014Q00012Q000E3Q000200012Q00D33Q00017Q00083Q0003073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E67030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01143Q0012F3000100014Q00B70001000100020010182Q0100023Q000675012Q001300013Q0004A23Q001300010012F3000100014Q006200010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q00010001000800069001023Q000100012Q007F8Q000E0001000200012Q00D33Q00013Q00013Q00093Q0003023Q00543103023Q00543203023Q00543303093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B6574030B3Q0054652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573001B4Q00E63Q00033Q001227000100013Q001227000200023Q001227000300034Q009E012Q000300010012F3000100043Q00201B2Q0100010005001227000300064Q00422Q01000300020006A20102000E000100010004A23Q000E000100201B010200010005001227000400074Q00420102000400020006A201030013000100020004A23Q0013000100201B010300020005001227000500084Q00420103000500020006750103001A00013Q0004A23Q001A00012Q007F00046Q0010000500034Q001000065Q001227000700094Q00B80004000700012Q00D33Q00017Q00043Q0003073Q00456E61626C656403073Q0067657467656E76030B3Q0057616C6B546F4375626573010001084Q007F00015Q0010182Q0100013Q000675012Q000700013Q0004A23Q000700010012F3000100024Q00B70001000100020030460001000300042Q00D33Q00017Q00013Q0003053Q0053702Q656401034Q007F00015Q0010182Q0100014Q00D33Q00017Q00053Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E7303093Q0043756265576F726C6403063Q0047726F756E6400103Q0012F33Q00013Q00201B014Q0002001227000200034Q0042012Q000200020006A22Q01000900013Q0004A23Q0009000100201B2Q013Q0002001227000300044Q00422Q01000300020006A20102000E000100010004A23Q000E000100201B010200010002001227000400054Q00420102000400022Q0068010200024Q00D33Q00017Q00083Q00030F3Q0067657447726F756E64466F6C6465722Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903043Q0053697A65027Q0040001E4Q007F7Q002074014Q00012Q00B73Q000100020006E93Q0007000100010004A23Q000700012Q001C000100014Q00682Q0100023Q00201B2Q013Q0002001227000300034Q00422Q01000300020006752Q01000E00013Q0004A23Q000E00010006090001001200013Q0004A23Q0012000100201B2Q013Q0004001227000300034Q00AA000400014Q00422Q01000400020006E900010016000100010004A23Q001600012Q001C000200024Q0068010200023Q0020740102000100050020B500020002000600202Q00030001000700202Q00030003000600202Q0003000300084Q0002000200034Q000200028Q00017Q00133Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F7073030C3Q0067657447726F756E64546F7003063Q00697061697273030B3Q004765744368696C6472656E03093Q00626C61636B6C697374030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03013Q0059027Q00C003093Q006D617848656967687403053Q007461626C6503063Q00696E7365727403043Q006375626503043Q0070617274003B3Q00121E012Q00013Q00206Q000200122Q000200038Q000200024Q00015Q00202Q0001000100044Q00010001000200064Q000B00013Q0004A23Q000B00010006E90001000D000100010004A23Q000D00012Q00E600026Q0068010200024Q00E600025Q0012B2000300053Q00202Q00043Q00064Q000400056Q00033Q000500044Q003700012Q007F00085Q0020740108000800072Q00300008000800070006E900080037000100010004A23Q0037000100201B010800070002001227000A00084Q00420108000A00020006E900080025000100010004A23Q0025000100201B010800070002001227000A00094Q00420108000A00020006E900080025000100010004A23Q0025000100201B01080007000A001227000A000B4Q00420108000A00020006750108003700013Q0004A23Q0037000100207401090008000C00207401090009000D2Q00B9000900090001000E48010E0037000100090004A23Q003700012Q007F000A5Q002074010A000A000F000629000900370001000A0004A23Q003700010012F3000A00103Q0020B0000A000A00114Q000B00026Q000C3Q000200102Q000C0012000700102Q000C001300084Q000A000C00010006FE00030013000100020004A23Q001300012Q0068010200024Q00D33Q00017Q00063Q0003073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465020D3Q001291010200013Q00202Q00020002000200202Q00033Q000300202Q0004000100034Q00030003000400122Q000400043Q00202Q00053Q000500202Q0006000100054Q0005000500064Q0002000500020020740102000200062Q0068010200024Q00D33Q00017Q00083Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303073Q0073652Q73696F6E026Q00F03F03073Q00456E61626C6564010003043Q007461736B03053Q00737061776E011A3Q001240000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100030006E93Q000D000100010004A23Q000D00012Q00D33Q00014Q007F000200013Q0030460002000500060012F3000200073Q00207401020002000800069001033Q000100062Q007F3Q00024Q00103Q00014Q007F8Q007F3Q00034Q007F3Q00044Q007F3Q00054Q000E0002000200012Q00D33Q00013Q00013Q00203Q0003043Q006D61746803043Q0068756765028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q006375626503063Q00506172656E7403043Q007061727403063Q00697061697273030D3Q00676574466C2Q6F72437562657303083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03073Q0067657467656E76030D3Q0057616C6B437562654465627567030C3Q0067657447726F756E64546F7003053Q007072696E7403353Q005B57616C6B437562655D2074617267657420257320646973743D252E31662068656967687441626F766547726F756E643D252E316603063Q00666F726D617403043Q004E616D6503013Q005903063Q004D6F7665546F027Q0040026Q001840026Q00244003043Q004D6F766503073Q00566563746F723303043Q007A65726F00983Q0012F3000100013Q0020742Q0100010002001227000200033Q00069001033Q000100032Q007F8Q007F3Q00014Q007F3Q00023Q00069001040001000100012Q007F3Q00024Q0010000500034Q00B70005000100020006750105008800013Q0004A23Q008800010012F3000500043Q0020E200050005000500122Q000600066Q0005000200014Q000500033Q00202Q00050005000700062Q00060018000100050004A23Q0018000100201B010600050008001227000800094Q00420106000800022Q007F000700044Q00B70007000100020006750106001E00013Q0004A23Q001E00010006E900070020000100010004A23Q002000012Q001C7Q0004A23Q00090001000675012Q002B00013Q0004A23Q002B000100207401083Q000A00207401080008000B0006750108002A00013Q0004A23Q002A000100207401083Q000C00207401080008000B0006E90008002B000100010004A23Q002B00012Q001C7Q0006E93Q0063000100010004A23Q006300012Q001C000800083Q0012F3000900013Q0020740109000900020012F3000A000D4Q007F000B00023Q002074010B000B000E2Q0024000B00014Q008C010A3Q000C0004A23Q004000012Q007F000F00053Q00207401100007000F0020740111000E000C00207401110011000F2Q0042010F00110002000699010F0040000100090004A23Q004000012Q00100010000E4Q00100009000F4Q0010000800103Q0006FE000A0036000100020004A23Q003600010006750108006300013Q0004A23Q006300012Q0010000A00084Q007A010B00093Q00122Q000C00103Q00202Q000C000C00114Q000C000100024Q0002000C6Q0001000B8Q000A3Q00122Q000A00126Q000A0001000200202Q000A000A001300062Q000A006300013Q0004A23Q006300012Q007F000A00023Q002074010A000A00142Q00B7000A000100020006E9000A0057000100010004A23Q00570001001227000A00033Q0012F3000B00153Q0012CB000C00163Q00202Q000C000C001700202Q000E0008000A00202Q000E000E00184Q000F00093Q00202Q00100008000C00202Q00100010000F00202Q0010001000194Q00100010000A4Q000C00106Q000B3Q0001000675012Q008400013Q0004A23Q008400012Q007F000800053Q00207401090007000F002074010A3Q000C002074010A000A000F2Q00420108000A000200201B01090006001A002074010B3Q000C002074010B000B000F2Q00B80009000B00010020E500090001001B00069901080078000100090004A23Q007800012Q0010000900083Q001265010A00103Q00202Q000A000A00114Q000A000100024Q0002000A6Q000100093Q00044Q000900010012F3000900103Q0020740109000900112Q00B70009000100022Q00B9000900090002000EA7001C0009000100090004A23Q000900012Q0010000900043Q0020C8000A3Q000A00122Q000B001D6Q0009000B00019Q0000044Q0009000100201B01080006001A002074010A0007000F2Q00B80008000A00010004A23Q000900012Q007F000500033Q0020740105000500070006750105009100013Q0004A23Q009100012Q007F000500033Q00207401050005000700201B010500050008001227000700094Q00420105000700020006750105009700013Q0004A23Q0097000100201B01060005001E0012F30008001F3Q0020740108000800202Q00B80006000800012Q00D33Q00013Q00023Q00043Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303053Q00416C69766503073Q0073652Q73696F6E00123Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001000013Q0004A23Q001000012Q007F7Q002074014Q0003000675012Q001000013Q0004A23Q001000012Q007F3Q00014Q007F000100023Q0020742Q01000100040006713Q000F000100010004A23Q000F00012Q0061017Q00AA3Q00014Q0068012Q00024Q00D33Q00017Q000B3Q0003073Q0067657467656E76030D3Q0057616C6B43756265446562756703053Q007072696E74031E3Q005B57616C6B437562655D20736B692Q70696E6720737475636B206375626503043Q004E616D652Q033Q00666F7203013Q007303093Q00626C61636B6C6973742Q0103043Q007461736B03053Q0064656C617902173Q0012F3000200014Q00B70002000100020020740102000200020006750102000C00013Q0004A23Q000C00010012F3000200033Q0012BC000300043Q00202Q00043Q000500122Q000500066Q000600013Q00122Q000700076Q0002000700012Q007F00025Q00205E01020002000800202Q00023Q000900122Q0002000A3Q00202Q00020002000B4Q000300013Q00069001043Q000100022Q007F8Q00108Q00B80002000400012Q00D33Q00013Q00013Q00023Q0003093Q00626C61636B6C6973742Q00054Q007F7Q002074014Q00012Q007F000100013Q00206C012Q000100022Q00D33Q00017Q00033Q0003073Q0067657467656E7603093Q0044697361626C65334403153Q00536574336452656E646572696E67456E61626C656401083Q0012952Q0100016Q00010001000200102Q000100026Q00015Q00202Q0001000100034Q00038Q0001000300016Q00017Q00023Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E01043Q0012F3000100014Q00B70001000100020010182Q0100024Q00D33Q00017Q00033Q00034Q0003053Q007063612Q6C2Q0100153Q0012273Q00013Q0012F3000100023Q00069001023Q000100022Q00108Q007F8Q000E00010002000100260C3Q000A000100010004A23Q000A00012Q00AA000100014Q00682Q0100023Q0012F3000100023Q00023C010200014Q006E2Q01000200020006A201030013000100010004A23Q0013000100260C00020012000100030004A23Q001200012Q006101036Q00AA000300014Q0068010300024Q00D33Q00013Q00023Q00013Q00030F3Q00476574452Q726F724D652Q7361676500054Q00DD3Q00013Q00206Q00016Q000200029Q006Q00017Q00083Q0003043Q0067616D65030A3Q004765745365727669636503073Q00436F7265477569030E3Q0046696E6446697273744368696C64030F3Q00526F626C6F7850726F6D7074477569030D3Q0070726F6D70744F7665726C617900030B3Q00452Q726F7250726F6D707400173Q0012AF3Q00013Q00206Q000200122Q000200038Q0002000200202Q00013Q000400122Q000300056Q00010003000200062Q0002000C000100010004A23Q000C000100201B010200010004001227000400064Q004201020004000200260C00020013000100070004A23Q0013000100201B010300020004001227000500084Q004201030005000200269900030014000100070004A23Q001400012Q006101036Q00AA000300014Q0068010300024Q00D33Q00017Q00083Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203073Q0073652Q73696F6E026Q00F03F03043Q0062757379010003043Q007461736B03053Q00737061776E01193Q001240000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100032Q007F00025Q0030460002000500060006E93Q000F000100010004A23Q000F00012Q00D33Q00013Q0012F3000200073Q00207401020002000800069001033Q000100052Q007F3Q00014Q00103Q00014Q007F8Q007F3Q00024Q007F3Q00034Q000E0002000200012Q00D33Q00013Q00013Q00173Q0003043Q007461736B03043Q0077616974026Q00F03F030E3Q006973446973636F2Q6E656374656403043Q00627573792Q01028Q0003123Q0054656C65706F7274496E69744661696C656403073Q00436F2Q6E65637403113Q0071756575655F6F6E5F74656C65706F727403073Q0067657467656E7603123Q0056656C6F636974794C6F6164737472696E6703053Q007063612Q6C03023Q006F7303053Q00636C6F636B026Q002E40026Q00E03F030A3Q00446973636F2Q6E65637403043Q006D6174682Q033Q006D696E026Q000840026Q003E40012Q005C3Q000690014Q000100032Q007F8Q007F3Q00014Q007F3Q00024Q001000016Q00B70001000100020006752Q01005B00013Q0004A23Q005B00010012F3000100013Q0020842Q010001000200122Q000200036Q0001000200014Q000100023Q00202Q0001000100044Q00010001000200062Q0001000400013Q0004A23Q000400012Q007F000100023Q0020742Q01000100050006E900010004000100010004A23Q000400012Q007F000100023Q003046000100050006001227000100074Q001000026Q00B70002000100020006750102005800013Q0004A23Q005800010020440001000100032Q00AA00026Q007F000300033Q00207401030003000800201B01030003000900069001050001000100012Q00103Q00024Q00420103000500020012F30004000A3Q0006750104003200013Q0004A23Q003200010012F30004000B4Q00B700040001000200207401040004000C0006750104003200013Q0004A23Q003200010012F30004000D3Q0012D20005000A3Q00122Q0006000B6Q00060001000200202Q00060006000C4Q0004000600010012F30004000D3Q00069001050002000100032Q007F3Q00034Q007F3Q00024Q007F3Q00044Q000E0004000200010012F30004000E3Q00207401040004000F2Q00B70004000100022Q001000056Q00B70005000100020006750105004C00013Q0004A23Q004C00010006E90002004C000100010004A23Q004C00010012F30005000E3Q00207401050005000F2Q00B70005000100022Q00B90005000500040026130005004C000100100004A23Q004C00010012F3000500013Q002074010500050002001227000600114Q000E0005000200010004A23Q003B000100201B0105000300122Q000E0005000200010012F3000500013Q0020740105000500020012F3000600133Q00207401060006001400205C010700010015001227000800164Q007C010600084Q006100053Q00012Q006D01025Q0004A23Q001800012Q007F000200023Q0030460002000500170004A23Q000400012Q00D33Q00013Q00033Q00043Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203053Q00416C69766503073Q0073652Q73696F6E00123Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001000013Q0004A23Q001000012Q007F7Q002074014Q0003000675012Q001000013Q0004A23Q001000012Q007F3Q00014Q007F000100023Q0020742Q01000100040006713Q000F000100010004A23Q000F00012Q0061017Q00AA3Q00014Q0068012Q00024Q00D33Q00019Q003Q00034Q00AA3Q00014Q00038Q00D33Q00017Q00033Q0003173Q0054656C65706F7274546F506C616365496E7374616E636503073Q00706C616365496403053Q006A6F62496400094Q007F7Q00201B014Q00012Q007F000200013Q00204D0102000200024Q000300013Q00202Q0003000300034Q000400028Q000400012Q00D33Q00017Q00073Q0003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C756501163Q0012082Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004A23Q000A000100201B010200010004001227000400054Q00420102000400020006750102001500013Q0004A23Q00150001000675012Q001300013Q0004A23Q001300010012F3000300014Q00B70003000100020020740103000300070010180102000600030004A23Q001500012Q007F000300013Q0010180102000600032Q00D33Q00017Q00073Q0003073Q0067657467656E76030E3Q0057616C6B53702Q656456616C7565030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q0012C0000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q0004A23Q001200012Q007F00015Q0020742Q01000100040006A20102000F000100010004A23Q000F000100201B010200010005001227000400064Q00420102000400020006750102001200013Q0004A23Q00120001001018010200074Q00D33Q00017Q000A3Q0003073Q0067657467656E76030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C7565026Q00494001163Q0012082Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004A23Q000A000100201B010200010004001227000400054Q00420102000400020006750102001500013Q0004A23Q00150001000675012Q001400013Q0004A23Q001400010030460002000600070012F3000300014Q00B70003000100020020740103000300090010180102000800030004A23Q0015000100304600020008000A2Q00D33Q00017Q00093Q0003073Q0067657467656E76030E3Q004A756D70506F77657256616C7565030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F77657201143Q0012C0000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001300013Q0004A23Q001300012Q007F00015Q0020742Q01000100040006A20102000F000100010004A23Q000F000100201B010200010005001227000400064Q00420102000400020006750102001300013Q0004A23Q00130001003046000200070008001018010200094Q00D33Q00017Q00023Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7001043Q0012F3000100014Q00B70001000100020010182Q0100024Q00D33Q00017Q00043Q0003083Q00746F737472696E6703043Q0067737562030C3Q005E25732A282E2D2925732A2403023Q00253101093Q0012F3000100014Q001000026Q00FF00010002000200201B2Q0100010002001227000300033Q001227000400044Q00422Q01000400022Q00682Q0100024Q00D33Q00017Q00123Q00028Q0003063Q00737472696E6703053Q00752Q70657203043Q006773756203013Q002C034Q002Q033Q0025732B026Q00F03F03043Q0066696E6403013Q004B025Q00408F4003013Q004D024Q0080842E4103013Q0042024Q00652QCD4103013Q0054023Q00A0941A6D4203083Q00746F6E756D626572014D3Q0006E93Q0004000100010004A23Q00040001001227000100014Q00682Q0100023Q0012F3000100023Q00207F2Q01000100034Q00028Q0001000200026Q00013Q00202Q00013Q000400122Q000300053Q00122Q000400066Q00010004000200202Q00010001000400122Q000300073Q00122Q000400066Q0001000400026Q00013Q00122Q000100083Q00202Q00023Q000900122Q0004000A6Q00020004000200062Q0002001F00013Q0004A23Q001F00010012270001000B3Q00203001023Q000400122Q0004000A3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201B01023Q00090012270004000C4Q00420102000400020006750102002B00013Q0004A23Q002B00010012270001000D3Q00203001023Q000400122Q0004000C3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201B01023Q00090012270004000E4Q00420102000400020006750102003700013Q0004A23Q003700010012270001000F3Q00203001023Q000400122Q0004000E3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201B01023Q0009001227000400104Q00420102000400020006750102004200013Q0004A23Q00420001001227000100113Q00208901023Q000400122Q000400103Q00122Q000500066Q0002000500026Q00023Q0012F3000200124Q001000036Q00FF0002000200020006750102004A00013Q0004A23Q004A00012Q00450003000200010006E90003004B000100010004A23Q004B0001001227000300014Q0068010300024Q00D33Q00017Q000E3Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D6503063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A65637403053Q007461626C6503063Q00696E7365727403043Q004E616D6503043Q00736F7274003A4Q00537Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q0004A23Q001E000100201B2Q013Q0001001227000300034Q00422Q01000300020006752Q01001E00013Q0004A23Q001E00010020742Q013Q000300201B2Q0100010001001227000300044Q00422Q01000300020006752Q01001E00013Q0004A23Q001E00010020742Q013Q000300208A2Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q0004A23Q001E00010020742Q013Q00030020B100010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006E900010022000100010004A23Q002200012Q001C000200024Q0068010200024Q00E600025Q0012B2000300073Q00202Q0004000100084Q000400056Q00033Q000500044Q0032000100201B010800070009001227000A000A4Q00420108000A00020006750108003200013Q0004A23Q003200010012F30008000B3Q00207401080008000C2Q0010000900023Q002074010A0007000D2Q00B80008000A00010006FE00030028000100020004A23Q002800010012F30003000B3Q00206400030003000E4Q000400026Q0003000200014Q000200028Q00017Q00163Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D65028Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A656374026Q00F03F030E3Q0047657444657363656E64616E747303073Q0056697369626C6503043Q004E616D6503063Q004C6F636B656403043Q004C6F636B03073Q00556E6B6E6F776E030A3Q00496D6167654C6162656C030B3Q00496D616765436F6C6F723303063Q00436F6C6F72332Q033Q006E657700634Q00537Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q0004A23Q001E000100201B2Q013Q0001001227000300034Q00422Q01000300020006752Q01001E00013Q0004A23Q001E00010020742Q013Q000300201B2Q0100010001001227000300044Q00422Q01000300020006752Q01001E00013Q0004A23Q001E00010020742Q013Q000300208A2Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q0004A23Q001E00010020742Q013Q00030020B100010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006E900010022000100010004A23Q002200012Q001C000200024Q0068010200023Q001227000200073Q001227000300073Q0012B2000400083Q00202Q0005000100094Q000500066Q00043Q000600044Q005D000100201B01090008000A001227000B000B4Q00420109000B00020006750109005D00013Q0004A23Q005D000100204400030003000C2Q00AA00095Q0012B2000A00083Q00202Q000B0008000D4Q000B000C6Q000A3Q000C00044Q0058000100201B010F000E000A0012270011000B4Q0042010F00110002000675010F004800013Q0004A23Q00480001002074010F000E000E000675010F004800013Q0004A23Q00480001002074010F000E000F00260C000F0046000100100004A23Q00460001002074010F000E000F00260C000F0046000100110004A23Q00460001002074010F000E000F002699000F0048000100120004A23Q004800012Q00AA000900013Q0004A23Q005A000100201B010F000E000A001227001100134Q0042010F00110002000675010F005800013Q0004A23Q00580001002074010F000E0014001270011000153Q00202Q00100010001600122Q001100073Q00122Q001200073Q00122Q001300076Q00100013000200062Q000F0058000100100004A23Q005800012Q00AA000900013Q0004A23Q005A00010006FE000A0035000100020004A23Q003500010006E90009005D000100010004A23Q005D000100204400020002000C0006FE00040029000100020004A23Q002900012Q0010000400024Q0010000500034Q0072000400034Q00D33Q00017Q00043Q00030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657200114Q007F7Q00201B014Q0001001227000200024Q0042012Q00020002000675012Q000A00013Q0004A23Q000A000100201B2Q013Q0001001227000300034Q00422Q01000300022Q00103Q00013Q0006A22Q01000F00013Q0004A23Q000F000100201B2Q013Q0001001227000300044Q00422Q01000300022Q00682Q0100024Q00D33Q00017Q00193Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q6665727303063Q0069706169727303053Q00546965723103053Q00546965723203053Q0054696572332Q033Q005065742Q033Q0049734103093Q00546578744C6162656C03073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03043Q006E616D6503043Q005465787403053Q00707269636503013Q003F03093Q00617661696C61626C6503073Q0056697369626C65034Q0003023Q00783000674Q00537Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q0004A23Q001E000100201B2Q013Q0001001227000300034Q00422Q01000300020006752Q01001E00013Q0004A23Q001E00010020742Q013Q000300201B2Q0100010001001227000300044Q00422Q01000300020006752Q01001E00013Q0004A23Q001E00010020742Q013Q000300208A2Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q0004A23Q001E00010020742Q013Q00030020B100010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006E900010022000100010004A23Q002200012Q001C000200024Q0068010200024Q00E600025Q001285010300076Q000400033Q00122Q000500083Q00122Q000600093Q00122Q0007000A6Q0004000300012Q006E0103000200050004A23Q0063000100201B0108000100012Q0010000A00074Q00420108000A00020006A201090033000100080004A23Q0033000100201B010900080001001227000B000B4Q00420109000B00020006750109006300013Q0004A23Q0063000100201B010A0009000C001227000C000D4Q0042010A000C0002000675010A006300013Q0004A23Q0063000100201B010A0008000100125D000C000E6Q000A000C000200202Q000B0008000100122Q000D000F6Q000B000D000200202Q000C0008000100122Q000E00106Q000C000E000200062Q000D00480001000C0004A23Q0048000100201B010D000C0001001227000F00114Q0042010D000F00022Q00E6000E3Q0003002074010F00090013001018010E0012000F000675010D005000013Q0004A23Q00500001002074010F000D00130006E9000F0051000100010004A23Q00510001001227000F00153Q001018010E0014000F000675010A005700013Q0004A23Q00570001002074010F000A00170006E9000F005F000100010004A23Q005F0001000675010B005C00013Q0004A23Q005C0001002074010F000B00130006E9000F005D000100010004A23Q005D0001001227000F00183Q002699000F0060000100190004A23Q006000012Q0061010F6Q00AA000F00013Q001018010E0016000F2Q008E00020007000E0006FE0003002B000100020004A23Q002B00012Q0068010200024Q00D33Q00017Q00033Q0003063Q0069706169727303073Q0067657467656E7603123Q0053656C656374656454617267657450657473010F3Q0012CD000100013Q00122Q000200026Q00020001000200202Q0002000200034Q00010002000300044Q000A00010006680005000A00013Q0004A23Q000A00012Q00AA000600014Q0068010600023Q0006FE00010006000100020004A23Q000600012Q00AA00016Q00682Q0100024Q00D33Q00017Q00093Q0003073Q0067657467656E7603123Q0053656C65637465645461726765745065747303063Q0069706169727303053Q007461626C6503063Q0072656D6F76652Q033Q006D617803043Q004D617820030E3Q00207065747320726561636865642103063Q00696E7365727401263Q0012002Q0100016Q00010001000200202Q00010001000200122Q000200036Q000300016Q00020002000400044Q001000010006680006001000013Q0004A23Q001000010012F3000700043Q00208B0007000700054Q000800016Q000900056Q0007000900014Q000700016Q000700023Q0006FE00020007000100020004A23Q000700012Q0021010200014Q007F00035Q0020740103000300060006290003001E000100020004A23Q001E00012Q00AA00025Q001227000300074Q007F00045Q002074010400040006001227000500084Q00A00103000300052Q0072000200033Q0012F3000200043Q00208B0002000200094Q000300016Q00048Q0002000400014Q000200016Q000200024Q00D33Q00017Q00043Q0003053Q007461626C6503053Q00636C65617203073Q0067657467656E7603123Q0053656C65637465645461726765745065747300073Q00129F3Q00013Q00206Q000200122Q000100036Q00010001000200202Q0001000100046Q000200016Q00017Q00063Q0003073Q0067657467656E7603123Q0053656C656374656454617267657450657473028Q0003043Q004E6F6E65026Q00F03F2Q033Q00202B2000143Q0012DF3Q00018Q0001000200206Q00024Q00015Q00262Q00010008000100030004A23Q00080001001227000100044Q00682Q0100024Q00212Q015Q0026990001000D000100050004A23Q000D00010020742Q013Q00052Q00682Q0100023Q0020742Q013Q0005001283010200066Q00035Q00202Q0003000300054Q0001000100034Q000100028Q00017Q00033Q0003073Q0067657467656E7603143Q004D65726368616E744D6178507269636554657874034Q0001073Q0012F3000100014Q00B70001000100020006090002000500013Q0004A23Q00050001001227000200033Q0010182Q01000200022Q00D33Q00017Q00063Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01173Q001240000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100030006E93Q000D000100010004A23Q000D00012Q00D33Q00013Q0012F3000200053Q00207401020002000600069001033Q000100052Q00103Q00014Q007F8Q007F3Q00014Q007F3Q00024Q007F3Q00034Q000E0002000200012Q00D33Q00013Q00013Q00283Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657203063Q0069706169727303123Q0053656C65637465645461726765745065747303063Q00737472696E6703053Q00752Q7065722Q0103043Q006E65787403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q66657273030A3Q007061727365507269636503143Q004D65726368616E744D617850726963655465787403053Q00546965723103053Q00546965723203053Q0054696572332Q033Q0050657403073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03073Q0056697369626C6503043Q0054657874034Q002Q033Q0049734103093Q00546578744C6162656C03023Q007830028Q0003053Q007063612Q6C0003043Q007461736B03043Q0077616974026Q33D33F00BF4Q00E68Q001C000100013Q0012F3000200014Q00B7000200010002002074010200020002000675010200BE00013Q0004A23Q00BE00012Q007F00026Q007F000300013Q002074010300030003000668000200BE000100030004A23Q00BE00010006E90001001E000100010004A23Q001E00012Q007F000200023Q00201B010200020004001227000400054Q00420102000400020006750102001800013Q0004A23Q0018000100201B010300020004001227000500064Q00420103000500022Q0010000200033Q0006A22Q01001E000100020004A23Q001E000100201B010300020004001227000500074Q00420103000500022Q0010000100034Q00E600025Q0012CD000300083Q00122Q000400016Q00040001000200202Q0004000400094Q00030002000500044Q002C00010012F30008000A3Q00207401080008000B2Q007F000900034Q0010000A00074Q00A60009000A4Q008100083Q000200206C01020008000C0006FE00030025000100020004A23Q002500010006752Q0100B900013Q0004A23Q00B900010012F30003000D4Q0010000400024Q00FF000300020002000675010300B900013Q0004A23Q00B900012Q007F000300043Q00201B0103000300040012270005000E4Q00420103000500020006A201040053000100030004A23Q0053000100201B0104000300040012270006000F4Q00420104000600020006750104005300013Q0004A23Q0053000100207401040003000F00201B010400040004001227000600104Q00420104000600020006750104005300013Q0004A23Q0053000100207401040003000F00208A01040004001000202Q00040004000400122Q000600116Q00040006000200062Q0004005300013Q0004A23Q0053000100207401040003000F0020B100040004001000202Q00040004001100202Q00040004000400122Q000600126Q000400060002000675010400B700013Q0004A23Q00B700012Q007F000500013Q0020740105000500130012F3000600014Q00B70006000100020020740106000600142Q00FF000500020002001285010600086Q000700033Q00122Q000800153Q00122Q000900163Q00122Q000A00176Q0007000300012Q006E0106000200080004A23Q00B4000100201B010B000400042Q0010000D000A4Q0042010B000D00022Q00AA000C5Q000675010B00B000013Q0004A23Q00B0000100201B010D000B0004001227000F00184Q0042010D000F000200201B010E000B000400125D001000196Q000E0010000200202Q000F000B000400122Q0011001A6Q000F0011000200202Q0010000B000400122Q0012001B6Q00100012000200062Q0011007A000100100004A23Q007A000100201B0111001000040012270013001C4Q00420111001300020006A20112007D0001000E0004A23Q007D00010020740112000E001D000675010F008200013Q0004A23Q008200010020740113000F001E0006E900130083000100010004A23Q008300010012270013001F3Q000675010D00B000013Q0004A23Q00B0000100201B0114000D0020001227001600214Q0042011400160002000675011400B000013Q0004A23Q00B000010006E9001200B0000100010004A23Q00B0000100260C001300B0000100220004A23Q00B000010012F30014000A3Q00203401140014000B4Q001500033Q00202Q0016000D001E4Q001500166Q00143Q00024Q00140002001400062Q001400B000013Q0004A23Q00B000010006750111009F00013Q0004A23Q009F00012Q007F001400013Q00207401140014001300207401150011001E2Q00FF0014000200020006E9001400A0000100010004A23Q00A00001001227001400233Q00265F000500A4000100230004A23Q00A40001000629001400B0000100050004A23Q00B000012Q00AA000C00014Q003000153Q000A0006E9001500B0000100010004A23Q00B000010012F3001500243Q00069001163Q000100022Q00103Q00014Q00103Q000A4Q00FF001500020002000675011500B000013Q0004A23Q00B0000100206C012Q000A000C0006E9000C00B3000100010004A23Q00B3000100206C012Q000A00252Q006D01095Q0006FE00060063000100020004A23Q006300010004A23Q00B900012Q00E600056Q00103Q00053Q0012F3000300263Q002074010300030027001227000400284Q000E0003000200010004A23Q000200012Q00D33Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q00517Q00206Q00014Q000200018Q000200016Q00017Q00053Q0003023Q00756903063Q007374617475732Q033Q00536574034Q0003073Q0063752Q72656E74021B4Q007F00025Q0020740102000200010020740102000200020006750102000D00013Q0004A23Q000D00012Q007F00025Q00207401020002000100207401020002000200201B0102000200030006090004000C00013Q0004A23Q000C0001001227000400044Q00B80002000400012Q007F00025Q0020740102000200010020740102000200050006750102001A00013Q0004A23Q001A00010006752Q01001A00013Q0004A23Q001A00012Q007F00025Q0020B300020002000100202Q00020002000500202Q0002000200034Q000400016Q0002000400012Q00D33Q00017Q00023Q0003173Q005365744175746F486174636853656C6563746564452Q6703123Q005365744175746F48617463685065724F6E65000E4Q007F8Q00B73Q00010002000675012Q000900013Q0004A23Q000900012Q007F3Q00013Q002074014Q00012Q00AA000100014Q000E3Q000200010004A23Q000D00012Q007F3Q00013Q002074014Q00022Q00AA000100014Q000E3Q000200012Q00D33Q00017Q00023Q0003173Q005365744175746F486174636853656C6563746564452Q6703123Q005365744175746F48617463685065724F6E6500094Q0047016Q00206Q00014Q00019Q00000200019Q0000206Q00024Q00019Q00000200016Q00017Q00013Q00030C3Q005365744175746F50756E636800054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00023Q00030F3Q005365745470546F426F2Q734B692Q6C030C3Q005365744175746F50756E636800094Q0047016Q00206Q00014Q00019Q00000200019Q0000206Q00024Q00019Q00000200016Q00017Q000A3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C030F3Q005365745470546F426F2Q734B692Q6C002D3Q0012F33Q00013Q00201B014Q0002001227000200034Q0042012Q000200022Q00AA00015Q000675012Q001500013Q0004A23Q001500010012F3000200043Q00201B01033Q00052Q00A6000300044Q008C01023Q00040004A23Q0013000100201B010700060006001227000900074Q00420107000900020006750107001300013Q0004A23Q001300012Q00AA000100013Q0004A23Q001500010006FE0002000C000100020004A23Q000C00010006752Q01002100013Q0004A23Q002100010012F3000200084Q00B70002000100020020740102000200090006E900020021000100010004A23Q002100012Q007F00025Q00207401020002000A2Q00AA000300014Q000E0002000200010004A23Q002C00010006E90001002C000100010004A23Q002C00010012F3000200084Q00B70002000100020020740102000200090006750102002C00013Q0004A23Q002C00012Q007F00025Q00207401020002000A2Q00AA00036Q000E0002000200012Q00D33Q00017Q00013Q00030B3Q005365744175746F4C69667400054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q00030B3Q005365744175746F4C69667400054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00013Q00030C3Q005365744175746F50756E636800054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q00030C3Q005365744175746F50756E636800054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00013Q00030C3Q005365744175746F53746F6D7000054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q00030C3Q005365744175746F53746F6D7000054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00013Q00030F3Q005365744175746F47656D54772Q656E00054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q00030F3Q005365744175746F47656D54772Q656E00054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00013Q00030B3Q005365744175746F53652Q6C00054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q00030B3Q005365744175746F53652Q6C00054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00013Q0003123Q005365744175746F54652Q7269746F7269657300054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q0003123Q005365744175746F54652Q7269746F7269657300054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00033Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F7269657303123Q005365744175746F54652Q7269746F72696573000A3Q0012F33Q00014Q00B73Q00010002002074014Q00020006E93Q0009000100010004A23Q000900012Q007F7Q002074014Q00032Q00AA000100014Q000E3Q000200012Q00D33Q00017Q00013Q00030C3Q005365744175746F437562657300054Q001F016Q00206Q00014Q000100018Q000200016Q00017Q00013Q00030C3Q005365744175746F437562657300054Q001F016Q00206Q00014Q00019Q00000200016Q00017Q00183Q0003063Q00737472696E6703053Q006C6F776572034Q0003043Q0066696E6403053Q0068617463682Q033Q00652Q6703063Q00706C617965722Q033Q0070767003043Q00626F2Q7303103Q0025665B25615D706C617925665B25415D03083Q00706C617974696D6503043Q00686F757203063Q006D696E75746503063Q006F6E6C696E6503073Q0070612Q7369766503043Q006C69667403053Q0070756E636803053Q0073746F6D702Q033Q0067656D03073Q00636F2Q6C65637403043Q0073652Q6C03083Q0074652Q7269746F7203093Q0074652Q7269746F727903043Q006375626501793Q0012F3000100013Q0020742Q01000100020006090002000500013Q0004A23Q00050001001227000200034Q00FF00010002000200201B010200010004001227000400054Q00420102000400020006E900020010000100010004A23Q0010000100201B010200010004001227000400064Q00420102000400020006750102001200013Q0004A23Q00120001001227000200054Q0068010200023Q00201B010200010004001227000400074Q00420102000400020006E90002001C000100010004A23Q001C000100201B010200010004001227000400084Q00420102000400020006750102001E00013Q0004A23Q001E0001001227000200084Q0068010200023Q00201B010200010004001227000400094Q00420102000400020006750102002500013Q0004A23Q00250001001227000200094Q0068010200023Q00201B0102000100040012270004000A4Q00420102000400020006E90002003E000100010004A23Q003E000100201B0102000100040012270004000B4Q00420102000400020006E90002003E000100010004A23Q003E000100201B0102000100040012270004000C4Q00420102000400020006E90002003E000100010004A23Q003E000100201B0102000100040012270004000D4Q00420102000400020006E90002003E000100010004A23Q003E000100201B0102000100040012270004000E4Q00420102000400020006750102004000013Q0004A23Q004000010012270002000F4Q0068010200023Q00201B010200010004001227000400104Q00420102000400020006750102004700013Q0004A23Q00470001001227000200104Q0068010200023Q00201B010200010004001227000400114Q00420102000400020006750102004E00013Q0004A23Q004E0001001227000200114Q0068010200023Q00201B010200010004001227000400124Q00420102000400020006750102005500013Q0004A23Q00550001001227000200124Q0068010200023Q00201B010200010004001227000400134Q00420102000400020006E90002005F000100010004A23Q005F000100201B010200010004001227000400144Q00420102000400020006750102006100013Q0004A23Q00610001001227000200134Q0068010200023Q00201B010200010004001227000400154Q00420102000400020006750102006800013Q0004A23Q00680001001227000200154Q0068010200023Q00201B010200010004001227000400164Q00420102000400020006750102006F00013Q0004A23Q006F0001001227000200174Q0068010200023Q00201B010200010004001227000400184Q00420102000400020006750102007600013Q0004A23Q00760001001227000200184Q0068010200024Q001C000200024Q0068010200024Q00D33Q00017Q00063Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030A3Q005175657374734C697374030E3Q005363726F2Q6C696E674672616D6503043Q004C697374001A4Q00537Q00206Q000100122Q000200028Q0002000200062Q0001000900013Q0004A23Q0009000100201B2Q013Q0001001227000300034Q00422Q01000300020006A20102000E000100010004A23Q000E000100201B010200010001001227000400044Q00420102000400020006A201030013000100020004A23Q0013000100201B010300020001001227000500054Q00420103000500020006A201040018000100030004A23Q0018000100201B010400030001001227000600064Q00420104000600022Q0068010400024Q00D33Q00017Q000A3Q0003043Q006773756203013Q002C034Q0003043Q0066696E6403013Q003A028Q0003063Q00676D617463682Q033Q0025642B026Q004E4003083Q00746F6E756D626572011C3Q0020892Q013Q000100122Q000300023Q00122Q000400036Q0001000400026Q00013Q00201B2Q013Q0004001227000300054Q00422Q01000300020006752Q01001700013Q0004A23Q00170001001227000100063Q00201B01023Q0007001227000400084Q00E00002000400040004A23Q0014000100205C0106000100090012F30007000A4Q0010000800054Q00FF0007000200022Q00522Q01000600070006FE0002000F000100010004A23Q000F00012Q00682Q0100023Q0012F30001000A4Q001000026Q00202Q0100024Q008C00016Q00D33Q00017Q00033Q0003063Q00737472696E6703053Q006D61746368031D3Q00285B25643A2C252E5D2B2925732A2F25732A285B25643A2C252E5D2B2901113Q001249000100013Q00202Q0001000100024Q00025Q00122Q000300036Q00010003000200062Q00010009000100010004A23Q000900012Q001C000300034Q0068010300024Q007F00036Q0010000400014Q00FF0003000200022Q007F00046Q0010000500024Q00A6000400054Q008C00036Q00D33Q00017Q00043Q002Q033Q0049734103093Q004775694F626A65637403073Q0056697369626C6503063Q00506172656E7402144Q001000025Q0006750102001100013Q0004A23Q0011000100067100020011000100010004A23Q0011000100201B010300020001001227000500024Q00420103000500020006750103000F00013Q0004A23Q000F00010020740103000200030006E90003000F000100010004A23Q000F00012Q00AA00036Q0068010300023Q0020740102000200040004A23Q000100012Q00AA000300014Q0068010300024Q00D33Q00017Q00243Q0003073Q006765744C69737403063Q00697061697273030B3Q004765744368696C6472656E03063Q00737472696E6703053Q006D6174636803043Q004E616D65030A3Q005E4461696C7943617264030E3Q0046696E6446697273744368696C64030A3Q00436F6E74656E74526F7703043Q00496E666F03093Q0051756573744E616D65030E3Q0051756573744E616D65426F6E7573034Q0003073Q0056697369626C6503043Q0054657874030B3Q00426F6E75734368726F6D652Q033Q0049734103093Q004775694F626A656374030E3Q0047657444657363656E64616E747303093Q00546578744C6162656C030D3Q00706172736550726F6772652Q73028Q0003053Q006C6F77657203043Q0066696E6403073Q00636C61696D656403093Q00636F6D706C6574656403053Q007461626C6503063Q00696E7365727403023Q00696403013Q007C03043Q006E616D6503043Q006465736303043Q006B696E6403083Q00636C612Q7369667903053Q00626F6E757303043Q00646F6E65008D4Q007F7Q002074014Q00012Q00B73Q000100022Q00E600015Q0006E93Q0007000100010004A23Q000700012Q00682Q0100023Q0012F3000200023Q00201B01033Q00032Q00A6000300044Q008C01023Q00040004A23Q008900010012F3000700043Q0020E800070007000500202Q00080006000600122Q000900076Q00070009000200062Q0007008900013Q0004A23Q0089000100201B010700060008001227000900094Q00420107000900020006750107001C00013Q0004A23Q001C000100207401070006000900201B0107000700080012270009000A4Q00420107000900020006A201080021000100070004A23Q0021000100201B010800070008001227000A000B4Q00420108000A00020006A201090026000100070004A23Q0026000100201B010900070008001227000B000C4Q00420109000B0002001227000A000D3Q0006750108002E00013Q0004A23Q002E0001002074010B0008000E000675010B002E00013Q0004A23Q002E0001002074010A0008000F0004A23Q003800010006750109003500013Q0004A23Q00350001002074010B0009000E000675010B003500013Q0004A23Q00350001002074010A0009000F0004A23Q003800010006750108003800013Q0004A23Q00380001002074010A0008000F00201B010B00060008001227000D00104Q0042010B000D0002000675010B004500013Q0004A23Q0045000100201B010C000B0011001227000E00124Q0042010C000E0002000675010C004500013Q0004A23Q00450001002074010C000B000E0006E9000C0046000100010004A23Q004600012Q00AA000C6Q00AA000D5Q0012B2000E00023Q00202Q000F000600134Q000F00106Q000E3Q001000044Q0073000100201B011300120011001227001500144Q00420113001500020006750113007300013Q0004A23Q007300012Q007F001300014Q0010001400124Q0010001500064Q00420113001500020006750113007300013Q0004A23Q007300012Q007F00135Q00207401130013001500207401140012000F2Q006E0113000200140006750113006400013Q0004A23Q006400010006750114006400013Q0004A23Q00640001000EA700160064000100140004A23Q0064000100062900140064000100130004A23Q006400012Q00AA000D00013Q0012F3001500043Q00207401150015001700207401160012000F2Q00FF00150002000200201B011600150018001227001800194Q00420116001800020006E900160072000100010004A23Q0072000100201B0116001500180012270018001A4Q00420116001800020006750116007300013Q0004A23Q007300012Q00AA000D00013Q0006FE000E004C000100020004A23Q004C00010012F3000E001B3Q002093000E000E001C4Q000F00016Q00103Q000600202Q00110006000600122Q0012001E6Q0013000A6Q00110011001300102Q0010001D001100202Q00110006000600102Q0010001F001100102Q00100020000A4Q00115Q00202Q0011001100224Q0012000A6Q00110002000200102Q00100021001100102Q00100023000C00102Q00100024000D4Q000E001000010006FE0002000C000100020004A23Q000C00012Q00682Q0100024Q00D33Q00017Q00043Q0003063Q006163746976650003053Q007063612Q6C2Q033Q006F2Q6600104Q002D7Q00206Q00014Q00015Q00302Q00010001000200064Q000F00013Q0004A23Q000F00012Q007F000100014Q0030000100013Q0006752Q01000F00013Q0004A23Q000F00010012F3000100034Q007F000200014Q0030000200023Q0020740102000200042Q000E0001000200012Q00D33Q00017Q00043Q0003043Q0073746F7003063Q0061637469766503053Q007063612Q6C03023Q006F6E010D4Q00C500015Q00202Q0001000100014Q0001000100014Q000100016Q000100013Q00062Q0001000C00013Q0004A23Q000C00012Q007F00025Q001018010200023Q0012F3000200033Q0020740103000100042Q000E0002000200012Q00D33Q00017Q00093Q0003073Q0067657467656E7603093Q004175746F517565737403073Q0073652Q73696F6E026Q00F03F03043Q0073746F702Q033Q004F2Q6603013Q002D03043Q007461736B03053Q00737061776E011E3Q001240000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100032Q007F00025Q0020740102000200052Q00690002000100010006E93Q0014000100010004A23Q001400012Q007F000200013Q001227000300063Q001227000400074Q00B80002000400012Q00D33Q00013Q0012F3000200083Q00207401020002000900069001033Q000100052Q007F3Q00024Q00103Q00014Q007F8Q007F3Q00014Q007F3Q00034Q000E0002000200012Q00D33Q00013Q00013Q00243Q00028Q00025Q00208C4003093Q0072656164436172647303193Q004F70656E2074686520517565737473206C697374206F6E636503013Q002D03063Q0069706169727303073Q0067657467656E76030A3Q005175657374446562756703053Q007072696E74032A3Q005B51756573745D202573207C202573207C206B696E643D257320626F6E75733D257320646F6E653D257303063Q00666F726D617403043Q006E616D6503043Q006465736303083Q00746F737472696E6703043Q006B696E6403053Q00626F6E757303043Q00646F6E6503073Q0070612Q736976652Q033Q0070767003023Q00696403053Q00737461727403023Q006F7303053Q00636C6F636B03073Q00576F726B696E6703043Q007469636B03053Q007063612Q6C2Q0103043Q007461736B03053Q0064656C617903043Q0073746F70031F3Q004F6E6C7920706C617974696D65202F2050765020717565737473206C65667403073Q00536B692Q70656403173Q00556E6B6E6F776E2071756573742074797065206C656674030F3Q00412Q6C2071756573747320646F6E6503043Q0077616974026Q00F03F00A93Q000690014Q000100032Q007F8Q007F3Q00014Q007F3Q00024Q001C000100013Q001227000200014Q00E600035Q001227000400024Q001000056Q00B7000500010002000675010500A500013Q0004A23Q00A500012Q007F000500023Q0020740105000500032Q00B70005000100022Q0021010600053Q00269900060016000100010004A23Q001600012Q007F000600033Q001227000700043Q001227000800054Q00B80006000800012Q001C000600064Q006001078Q00088Q00095Q00122Q000A00066Q000B00056Q000A0002000C00044Q005200010012F3000F00074Q00B7000F00010002002074010F000F0008000675010F003300013Q0004A23Q003300010012F3000F00093Q0012480010000A3Q00202Q00100010000B00202Q0012000E000C00202Q0013000E000D00122Q0014000E3Q00202Q0015000E000F4Q00140002000200122Q0015000E3Q00202Q0016000E00104Q0015000200020012F30016000E3Q0020740117000E00112Q00A6001600174Q000400106Q0061000F3Q0001002074010F000E00100006E9000F0052000100010004A23Q00520001002074010F000E00110006E9000F0052000100010004A23Q00520001002074010F000E000F002699000F003E000100120004A23Q003E00012Q00AA000700013Q0004A23Q00520001002074010F000E000F002699000F0043000100130004A23Q004300012Q00AA000900013Q0004A23Q00520001002074010F000E000F000675010F004E00013Q0004A23Q004E0001002074010F000E00142Q0030000F0003000F0006E9000F004E000100010004A23Q004E00010006E90006004D000100010004A23Q004D00012Q00100006000E3Q0004A23Q00520001002074010F000E000F0006E9000F0052000100010004A23Q005200012Q00AA000800013Q0006FE000A001E000100020004A23Q001E00010006750106008800013Q0004A23Q00880001002074010A00060014000671000100630001000A0004A23Q006300012Q007F000A00023Q002081010A000A001500202Q000B0006000F4Q000A0002000100202Q000A0006001400122Q000B00163Q00202Q000B000B00174Q000B000100024Q0002000B6Q0001000A4Q007F000A00033Q00127E010B00183Q00202Q000C0006000D4Q000A000C00014Q000A00043Q00202Q000B0006000F4Q000A000A000B00062Q000A007200013Q0004A23Q00720001002074010B000A0019000675010B007200013Q0004A23Q007200010012F3000B001A3Q002074010C000A00192Q000E000B000200010012F3000B00163Q002074010B000B00172Q00B7000B000100022Q00B9000B000B0002000699010400A00001000B0004A23Q00A00001002074010B0006001400206C0103000B001B002074010B000600140012F3000C001C3Q002074010C000C001D2Q0010000D00043Q000690010E0001000100022Q00103Q00034Q00103Q000B4Q0042000C000E00014Q000C00023Q00202Q000C000C001E4Q000C000100014Q000100016Q000B5Q00044Q00A000012Q007F000A00023Q002074010A000A001E2Q0069000A000100012Q001C000100013Q0006E900070090000100010004A23Q009000010006750109009500013Q0004A23Q009500012Q007F000A00033Q001227000B001F3Q001227000C00204Q00B8000A000C00010004A23Q00A000010006750108009C00013Q0004A23Q009C00012Q007F000A00033Q001227000B00213Q001227000C00054Q00B8000A000C00010004A23Q00A000012Q007F000A00033Q001227000B00223Q001227000C00054Q00B8000A000C00010012F3000A001C3Q002074010A000A0023001227000B00244Q000E000A000200010004A23Q000800012Q007F000500023Q00207401050005001E2Q00690005000100012Q00D33Q00013Q00023Q00043Q0003073Q0067657467656E7603093Q004175746F517565737403053Q00416C69766503073Q0073652Q73696F6E00123Q0012F33Q00014Q00B73Q00010002002074014Q0002000675012Q001000013Q0004A23Q001000012Q007F7Q002074014Q0003000675012Q001000013Q0004A23Q001000012Q007F3Q00014Q007F000100023Q0020742Q01000100040006713Q000F000100010004A23Q000F00012Q0061017Q00AA3Q00014Q0068012Q00024Q00D33Q00017Q00015Q00044Q007F8Q007F000100013Q00206C012Q000100012Q00D33Q00017Q00053Q0003083Q00496E7374616E63652Q033Q006E657703053Q00706169727303063Q00506172656E7403063Q00697061697273031D3Q001266010300013Q00202Q0003000300024Q00048Q00030002000200122Q000400036Q000500016Q00040002000600044Q000B000100260C0007000B000100040004A23Q000B00012Q008E0003000700080006FE00040008000100020004A23Q000800010006750102001600013Q0004A23Q001600010012F3000400054Q0010000500024Q006E0104000200060004A23Q001400010010180108000400030006FE00040013000100020004A23Q001300010020740104000100040006750104001B00013Q0004A23Q001B00010020740104000100040010180103000400042Q0068010300024Q00D33Q00017Q00073Q0003083Q005549436F726E6572030C3Q00436F726E657252616469757303043Q005544696D2Q033Q006E6577028Q00026Q00144003063Q00506172656E74020F4Q007E00025Q00122Q000300016Q00043Q000200122Q000500033Q00202Q00050005000400122Q000600053Q00062Q00070009000100010004A23Q00090001001227000700064Q004201050007000200102D01040002000500102Q000400076Q000200046Q00029Q0000017Q00083Q0003083Q0055495374726F6B6503053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q00E03F03093Q00546869636B6E652Q73026Q00F03F03063Q00506172656E7404144Q007F00045Q001227000500014Q00E600063Q000400060900070007000100010004A23Q000700012Q007F000700013Q0020740107000700030010180106000200070006090007000B000100020004A23Q000B0001001227000700053Q0010180106000400070006090007000F000100030004A23Q000F0001001227000700073Q001018010600060007001018010600084Q0020010400064Q008C00046Q00D33Q00017Q00013Q0003053Q007063612Q6C030E3Q0012F3000300013Q00069001043Q000100042Q007F8Q00103Q00014Q00103Q00024Q00108Q006E0103000200040006750103000B00013Q0004A23Q000B00010006090005000C000100040004A23Q000C00012Q001C000500054Q0068010500024Q00D33Q00013Q00013Q00093Q0003083Q005549536861646F77030A3Q00426C757252616469757303043Q005544696D2Q033Q006E6577028Q00026Q002840030C3Q005472616E73706172656E6379026Q00E03F03063Q00506172656E7400164Q00397Q00122Q000100016Q00023Q000300122Q000300033Q00202Q00030003000400122Q000400056Q000500013Q00062Q0005000A000100010004A23Q000A0001001227000500064Q00420103000500020010180102000200032Q007F000300023Q0006E900030010000100010004A23Q00100001001227000300083Q0010180102000700032Q007F000300033Q0010180102000900032Q0020012Q00024Q008C8Q00D33Q00017Q00023Q0003063Q0043726561746503043Q00506C6179030C4Q007F00035Q00201B0103000300012Q001000055Q00060900060006000100010004A23Q000600012Q007F000600014Q0010000700024Q00D900030007000200202Q0004000300024Q0004000200014Q000300028Q00017Q00133Q0003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0054657874034Q0003083Q00466F6E744661636503043Q00466F6E742Q033Q006E657703043Q00456E756D030A3Q00466F6E7457656967687403043Q00426F6C6403073Q00526567756C6172030A3Q0054657874436F6C6F7233030A3Q00546578745363616C65642Q0103043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574026Q002C40042A4Q00AA01045Q00122Q000500016Q00063Q000600302Q00060002000300062Q0007000700013Q0004A23Q00070001001227000700053Q0010180106000400070012F3000700073Q0020740107000700082Q007F000800013Q0006750102001200013Q0004A23Q001200010012F3000900093Q00207401090009000A00207401090009000B0006E900090015000100010004A23Q001500010012F3000900093Q00207401090009000A00207401090009000C2Q00420107000900020010180106000600070006090007001B000100030004A23Q001B00012Q007F000700023Q0020740107000700040010180106000D00070030460006000E000F0012F3000700113Q00207401070007001200060900080022000100010004A23Q00220001001227000800133Q00060900090025000100010004A23Q00250001001227000900134Q00420107000900020010180106001000072Q0020010400064Q008C00046Q00D33Q00017Q00153Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q00284003083Q00506F736974696F6E03063Q00506172656E74026Q0028C003053Q007461626C6503063Q00696E7365727403063Q00697061697273030A3Q00496E707574426567616E03073Q00436F2Q6E656374030C3Q00496E7075744368616E67656402953Q00060900060003000100010004A23Q000300012Q00E600066Q007F00075Q001226010800016Q00093Q000600302Q00090002000300302Q00090004000500302Q00090006000700122Q000A00093Q00202Q000A000A000A00122Q000B00073Q00122Q000C000B3Q00122Q000D000B3Q001227000E000C4Q0010010A000E000200102Q00090008000A00122Q000A00093Q00202Q000A000A000A00122Q000B000B3Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000B6Q000A000E000200102Q0009000D000A0010180109000E4Q00420107000900022Q007F00085Q001226010900016Q000A3Q000600302Q000A0002000300302Q000A0004000500302Q000A0006000700122Q000B00093Q00202Q000B000B000A00122Q000C00073Q00122Q000D000B3Q00122Q000E000B3Q001227000F000C4Q0010010B000F000200102Q000A0008000B00122Q000B00093Q00202Q000B000B000A00122Q000C000B3Q00122Q000D000B3Q00122Q000E00073Q00122Q000F000F6Q000B000F000200102Q000A000D000B001018010A000E4Q00420108000A00022Q007F00095Q001226010A00016Q000B3Q000600302Q000B0002000300302Q000B0004000500302Q000B0006000700122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000C3Q00122Q000F00073Q0012270010000B4Q0010010C0010000200102Q000B0008000C00122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000B3Q00122Q000F000B3Q00122Q0010000B6Q000C0010000200102Q000B000D000C001018010B000E4Q00420109000B00022Q007F000A5Q001227000B00014Q00E6000C3Q0006003046000C00020003003046000C00040005003046000C000600070012F3000D00093Q002074010D000D000A001227000E000B3Q001227000F000C3Q00124F011000073Q00122Q0011000B6Q000D0011000200102Q000C0008000D00122Q000D00093Q00202Q000D000D000A00122Q000E00073Q00122Q000F000F3Q00122Q0010000B3Q00122Q0011000B6Q000D0011000200102Q000C000D000D00102Q000C000E6Q000A000C000200122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00076Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00086Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00096Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D000A6Q000B000D000100122Q000B00126Q000C00066Q000B0002000D00044Q008800010020740110000F001300201B01100010001400069001123Q000100042Q00103Q00024Q00103Q00044Q00103Q00054Q00108Q00B80010001200010020740110000F001500201B01100010001400069001120001000100012Q00103Q00034Q00B80010001200010006FE000B007B000100020004A23Q007B00012Q007F000B00014Q007F000C00023Q002074010C000C0015000690010D0002000100052Q00103Q00034Q00103Q00024Q00103Q00044Q00108Q00103Q00054Q00B8000B000D00012Q00D33Q00013Q00033Q00073Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636803083Q00506F736974696F6E03073Q004368616E67656403073Q00436F2Q6E656374011A3Q00203D2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004A23Q000C00010020742Q013Q00010012F3000200023Q00207401020002000100207401020002000400066800010019000100020004A23Q001900012Q00AA000100014Q00282Q015Q00202Q00013Q00054Q000100016Q000100033Q00202Q0001000100054Q000100023Q00202Q00013Q000600202Q00010001000700069001033Q000100022Q00108Q007F8Q00B80001000300012Q00D33Q00013Q00013Q00033Q00030E3Q0055736572496E707574537461746503043Q00456E756D2Q033Q00456E64000A4Q007F7Q0020C75Q000100122Q000100023Q00202Q00010001000100202Q00010001000300064Q0009000100010004A23Q000900012Q00AA8Q00033Q00014Q00D33Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F756368010E3Q00203D2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004A23Q000C00010020742Q013Q00010012F3000200023Q0020740102000200010020740102000200040006680001000D000100020004A23Q000D00012Q00038Q00D33Q00017Q00073Q0003083Q00506F736974696F6E03053Q005544696D322Q033Q006E657703013Q005803053Q005363616C6503063Q004F2Q6673657403013Q0059011F4Q007F00015Q0006683Q001E000100010004A23Q001E00012Q007F000100013Q0006752Q01001E00013Q0004A23Q001E00010020742Q013Q00012Q00F4000200026Q0001000100024Q000200033Q00122Q000300023Q00202Q0003000300034Q000400043Q00202Q00040004000400202Q0004000400054Q000500043Q00202Q00050005000400202Q00050005000600202Q0006000100044Q0005000500064Q000600043Q00202Q00060006000700202Q0006000600054Q000700043Q00202Q00070007000700202Q00070007000600202Q0008000100074Q0007000700084Q00030007000200102Q0002000100032Q00D33Q00017Q00043Q00030A3Q00496E707574426567616E03073Q00436F2Q6E656374030A3Q00496E707574456E646564030C3Q00496E7075744368616E67656402184Q00AA00025Q00069001033Q000100022Q00108Q00103Q00013Q00207401043Q000100201B01040004000200069001060001000100022Q00103Q00024Q00103Q00034Q00B80004000600012Q007F00046Q007F000500013Q00207401050005000300069001060002000100012Q00103Q00024Q00B80004000600012Q007F00046Q007F000500013Q00207401050005000400069001060003000100022Q00103Q00024Q00103Q00034Q00B80004000600012Q00D33Q00013Q00043Q00093Q0003103Q004162736F6C757465506F736974696F6E030C3Q004162736F6C75746553697A6503043Q006D61746803053Q00636C616D7003083Q00506F736974696F6E03013Q0058028Q00026Q00F03F03013Q0059011F4Q007F00015Q0020742Q01000100012Q007F00025Q0020740102000200020012F3000300033Q00207401030003000400207401043Q00050020740104000400060020740105000100062Q00B90004000400050020740105000200062Q006901040004000500122Q000500073Q00122Q000600086Q00030006000200122Q000400033Q00202Q00040004000400202Q00053Q000500202Q00050005000900202Q0006000100094Q0005000500060020740106000200092Q000E010500050006001227000600073Q001227000700084Q00420104000700022Q007F000500014Q0010000600034Q0010000700044Q00B80005000700012Q00D33Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636801123Q00203D2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004A23Q000C00010020742Q013Q00010012F3000200023Q00207401020002000100207401020002000400066800010011000100020004A23Q001100012Q00AA000100014Q000300016Q007F000100014Q001000026Q000E0001000200012Q00D33Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F756368010F3Q00203D2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004A23Q000C00010020742Q013Q00010012F3000200023Q0020740102000200010020740102000200040006680001000E000100020004A23Q000E00012Q00AA00016Q000300016Q00D33Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F75636801134Q007F00015Q0006752Q01001200013Q0004A23Q001200010020742Q013Q00010012F3000200023Q0020740102000200010020740102000200030006710001000F000100020004A23Q000F00010020742Q013Q00010012F3000200023Q00207401020002000100207401020002000400066800010012000100020004A23Q001200012Q007F000100014Q001000026Q000E0001000200012Q00D33Q00017Q00093Q0003063Q0067657468756903053Q007063612Q6C030B3Q004C6F63616C506C61796572030C3Q0057616974466F724368696C6403093Q00506C61796572477569026Q00144003043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900223Q0012F33Q00013Q000675012Q000B00013Q0004A23Q000B00010012F33Q00023Q0012F3000100014Q006E012Q00020001000675012Q000B00013Q0004A23Q000B00010006752Q01000B00013Q0004A23Q000B00012Q00682Q0100023Q0012F33Q00023Q00023C2Q016Q006E012Q00020001000675012Q001300013Q0004A23Q001300010006752Q01001300013Q0004A23Q001300012Q00682Q0100024Q007F00025Q0020740102000200030006750102001C00013Q0004A23Q001C000100201B010300020004001227000500053Q001227000600064Q0020010300064Q008C00035Q0012F3000300073Q00208000030003000800122Q000500096Q000300056Q00039Q0000013Q00013Q00043Q0003083Q00636C6F6E6572656603043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900113Q0012F33Q00013Q000675012Q000B00013Q0004A23Q000B00010012F33Q00013Q0012542Q0100023Q00202Q00010001000300122Q000300046Q000100039Q00000200064Q000F000100010004A23Q000F00010012F33Q00023Q00201B014Q0003001227000200044Q0042012Q000200022Q0068012Q00024Q00D33Q00017Q00023Q002Q033Q0073796E030B3Q0070726F746563745F677569000C3Q0012F33Q00013Q000675012Q000B00013Q0004A23Q000B00010012F33Q00013Q002074014Q0002000675012Q000B00013Q0004A23Q000B00010012F33Q00013Q002074014Q00022Q007F00016Q000E3Q000200012Q00D33Q00017Q00413Q0003053Q00416C697665010003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C65030B3Q004175746F47656D57616C6B030F3Q004A756D70506F776572546F2Q676C6503083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E6703093Q0044697361626C65334403063Q0069706169727303083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E6503103Q004175746F486174636843756265452Q6703063Q004E6F636C6970030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030C3Q00496E66696E6974654A756D70030E3Q004D65726368616E74536E69706572030B3Q0057616C6B546F437562657303093Q004175746F517565737403073Q00456E61626C656403073Q0073652Q73696F6E026Q00F03F03043Q0073746F7003053Q007063612Q6C03053Q007461626C6503053Q00636C65617203093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403093Q004A756D70506F776572026Q004940030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F72656403093Q00776F726B7370616365030A3Q00426F2Q734D6F64656C73030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q004B657953797374656D426C757203073Q0044657374726F7903133Q0056656C6F63697479487562496E7374616E63650001BC4Q007F00015Q0020742Q01000100010006E900010005000100010004A23Q000500012Q00D33Q00014Q007F00015Q0030D100010001000200122Q000100036Q00010001000200202Q00020001000400062Q0002000D000100010004A23Q000D00010020740102000100050020740103000100060020740104000100070006E900040012000100010004A23Q0012000100207401040001000800207401050001000900209000060001000A00122Q0007000B6Q000800193Q00122Q0009000C3Q00122Q000A000D3Q00122Q000B000E3Q00122Q000C000F3Q00122Q000D00103Q00122Q000E00113Q00122Q000F00123Q00122Q001000133Q00122Q001100143Q00122Q001200153Q00122Q001300053Q00122Q001400073Q00122Q001500083Q00122Q001600093Q00122Q001700163Q00122Q001800173Q00122Q001900183Q00122Q001A00193Q00122Q001B001A3Q00122Q001C001B3Q00122Q001D001C3Q00122Q001E001D3Q00122Q001F001E3Q00122Q0020001F3Q00122Q0021000A3Q00122Q002200203Q00122Q002300213Q00122Q002400043Q00122Q002500063Q00122Q002600223Q00122Q002700233Q00122Q002800243Q00122Q002900256Q0008002100012Q006E0107000200090004A23Q003B000100206C2Q01000B00020006FE0007003A000100020004A23Q003A00012Q007F000700013Q0030840007002600024Q000700026Q000800023Q00202Q00080008002700202Q00080008002800102Q0007002700084Q000700036Q000800033Q00202Q00080008002700202Q00080008002800102Q0007002700084Q000700046Q000800043Q00202Q00080008002700202Q00080008002800102Q0007002700084Q000700056Q000800053Q00202Q00080008002700202Q00080008002800102Q0007002700084Q000700053Q00202Q0007000700294Q0007000100014Q000700066Q00070001000100122Q0007000B6Q000800076Q00070002000900044Q006100010012F3000C002A3Q000690010D3Q000100012Q00103Q000B4Q000E000C000200012Q006D010A5Q0006FE0007005C000100020004A23Q005C00010012F30007002B3Q00206600070007002C4Q000800076Q0007000200014Q000700083Q00202Q00070007002D00062Q0008006E000100070004A23Q006E000100201B01080007002E001227000A002F4Q00420108000A00020006750108007B00013Q0004A23Q007B00010006750102007800013Q0004A23Q0078000100201B01090008003000120C010B00313Q00202Q000B000B00324Q0009000B00014Q000900093Q00102Q0008003300090006750103007B00013Q0004A23Q007B00010030460008003400350006750104008500013Q0004A23Q008500010006A201090082000100070004A23Q0082000100201B010900070036001227000B00374Q00420109000B00020006750109008500013Q0004A23Q008500010030460009003800020006750105009F00013Q0004A23Q009F00010012F3000900393Q00201B010900090036001227000B003A4Q00420109000B00020006750109009F00013Q0004A23Q009F00010012F3000A000B3Q00201B010B0009003B2Q00A6000B000C4Q008C010A3Q000C0004A23Q009D000100201B010F000E003C0012270011003D4Q0042010F00110002000675010F009A00013Q0004A23Q009A000100201B010F000E0036001227001100374Q0042010F00110002000675010F009D00013Q0004A23Q009D0001003046000F003800020006FE000A0092000100020004A23Q00920001000675010600A500013Q0004A23Q00A500010012F30009002A3Q000690010A0001000100012Q007F3Q000A4Q000E0009000200012Q007F0009000B3Q00201B010900090036001227000B003E4Q00420109000B0002000675010900AD00013Q0004A23Q00AD000100201B010A0009003F2Q000E000A000200010012F3000A00034Q00B7000A00010002002074010A000A00402Q007F000B5Q000668000A00B60001000B0004A23Q00B600010012F3000A00034Q00B7000A00010002003046000A004000410006E93Q00BB000100010004A23Q00BB00012Q007F000A000C3Q00201B010A000A003F2Q000E000A000200012Q00D33Q00013Q00023Q00013Q00030A3Q00446973636F2Q6E65637400044Q007F7Q00201B014Q00012Q000E3Q000200012Q00D33Q00017Q00013Q0003153Q00536574336452656E646572696E67456E61626C656400054Q004B7Q00206Q00014Q000200018Q000200016Q00019Q003Q00044Q007F8Q00AA00016Q000E3Q000200012Q00D33Q00019Q003Q00044Q007F8Q00AA000100014Q000E3Q000200012Q00D33Q00017Q003C3Q0003083Q00496E7374616E63652Q033Q006E6577030A3Q00426C7572452Q6665637403043Q004E616D65030D3Q004B657953797374656D426C757203043Q0053697A65026Q00344003063Q00506172656E74028Q0003053Q004672616D65030E3Q004B657953797374656D4672616D65030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E03053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240025Q00C0624003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03063Q004163746976652Q01026Q00184003063Q005374726F6B65026Q00304003093Q00546578744C6162656C03043Q005465787403173Q004B65792053797374656D20566572696669636174696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40026Q002840026Q00F03F03093Q005365636F6E64617279025Q00804640026Q006E40026Q003E40026Q00104003073Q0054657874426F78034Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q005375625465787403103Q00436C656172546578744F6E466F6375730100026Q0024C0026Q001440030A3Q005465787442752Q746F6E030A3Q005375626D6974204B657903063Q00412Q63656E74025Q00805640030F3Q004175746F42752Q746F6E436F6C6F7203093Q0041637469766174656403073Q00436F2Q6E65637402E23Q0012AB000200013Q00202Q00020002000200122Q000300036Q00020002000200302Q00020004000500302Q0002000600074Q00035Q00102Q00020008000300122Q000300096Q000400013Q00122Q0005000A6Q00063Q000900302Q00060004000B00122Q0007000D3Q00202Q00070007000200122Q0008000E3Q00122Q0009000E6Q00070009000200102Q0006000C000700122Q000700103Q00202Q00070007000200122Q0008000E3Q00122Q000900093Q00122Q000A000E3Q00122Q000B00096Q0007000B000200102Q0006000F000700122Q000700103Q00202Q00070007001100122Q000800123Q00122Q000900136Q00070009000200102Q0006000600074Q000700023Q00202Q00070007001500102Q00060014000700302Q00060016001700302Q00060018000900302Q00060019001A4Q000700033Q00102Q0006000800074Q0004000600024Q000500046Q000600043Q00122Q0007001B6Q0005000700014Q000500056Q000600046Q000700023Q00202Q00070007001C00122Q0008000E6Q0005000800014Q000500066Q000600043Q00122Q0007001D3Q00122Q0008000E6Q0005000800014Q000500013Q00122Q0006001E6Q00073Q000800302Q0007001F00204Q000800073Q00102Q0007002100084Q000800023Q00202Q00080008001F00102Q00070022000800302Q00070023002400122Q000800103Q00202Q00080008000200122Q000900093Q00122Q000A00093Q00122Q000B00093Q00122Q000C00256Q0008000C000200102Q0007000F000800122Q000800103Q00202Q00080008000200122Q000900263Q00122Q000A00093Q00122Q000B00093Q001227000C00074Q00AD0108000C000200102Q00070006000800302Q00070016002600102Q0007000800044Q0005000700024Q000600013Q00122Q0007000A6Q00083Q00064Q000900023Q00202Q00090009002700102Q00080014000900122Q0009000D3Q00202Q00090009000200122Q000A000E3Q00122Q000B00096Q0009000B000200102Q0008000C000900122Q000900103Q00202Q00090009000200122Q000A000E3Q00122Q000B00093Q00122Q000C00093Q00122Q000D00286Q0009000D000200102Q0008000F000900122Q000900103Q00202Q00090009000200122Q000A00093Q00122Q000B00293Q00122Q000C00093Q00122Q000D002A6Q0009000D000200102Q00080006000900302Q00080018000900102Q0008000800044Q0006000800024Q000700046Q000800063Q00122Q0009002B6Q0007000900014Q000700056Q000800066Q000900023Q00202Q00090009001C4Q000A00086Q0007000A00024Q000800013Q00122Q0009002C6Q000A3Q000B00302Q000A0016002600302Q000A001F002D00302Q000A002E002F4Q000B00023Q00202Q000B000B003100102Q000A0030000B4Q000B00093Q00102Q000A0021000B4Q000B00023Q00202Q000B000B001F00102Q000A0022000B00302Q000A0023002500302Q000A0032003300122Q000B00103Q00202Q000B000B000200122Q000C00263Q00122Q000D00343Q00122Q000E00263Q00122Q000F00096Q000B000F000200102Q000A0006000B00122Q000B00103Q00202Q000B000B000200122Q000C00093Q00122Q000D00353Q00122Q000E00093Q00122Q000F00096Q000B000F000200102Q000A000F000B00102Q000A000800064Q0008000A00022Q007F000900013Q001227000A00364Q00E6000B3Q000A003046000B001F00372Q007F000C00073Q001018010B0021000C2Q007F000C00023Q002074010C000C001F001018010B0022000C003046000B002300252Q007F000C00023Q002074010C000C0038001018010B0014000C0012A5010C000D3Q00202Q000C000C000200122Q000D000E3Q00122Q000E00096Q000C000E000200102Q000B000C000C00122Q000C00103Q00202Q000C000C000200122Q000D000E3Q00122Q000E00093Q001293010F00093Q00122Q001000396Q000C0010000200102Q000B000F000C00122Q000C00103Q00202Q000C000C000200122Q000D00093Q00122Q000E00293Q00122Q000F00093Q00122Q0010002A4Q0042010C00100002001018010B0006000C003046000B003A0033001018010B000800042Q00420109000B00022Q007F000A00044Q0010000B00093Q001227000C002B4Q00B8000A000C0001000690010A3Q0001000B2Q00103Q00084Q00108Q00103Q00024Q007F3Q000A4Q00103Q00044Q007F3Q000B4Q00103Q00014Q00103Q00034Q007F3Q000C4Q00103Q00074Q007F3Q00023Q002016000B0009003B00202Q000B000B003C4Q000D000A6Q000B000D00014Q000B000D6Q000C00046Q000D00016Q000E00056Q000D000100012Q00B8000B000D00012Q00D33Q00013Q00013Q00173Q0003043Q005465787403073Q0044657374726F7903043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240028Q0003043Q007461736B03043Q0077616974029A5Q99C93F026Q00F03F026Q00084003043Q004B69636B030B3Q00496E76616C6964204B6579034Q00030F3Q00506C616365686F6C6465725465787403063Q00737472696E6703063Q00666F726D617403133Q00496E76616C6964204B657921202825642F332903053Q00436F6C6F7203073Q005761726E696E6703053Q0064656C6179026Q00F83F004A4Q007F7Q002074014Q00012Q007F000100013Q0006683Q001D000100010004A23Q001D00012Q007F3Q00023Q00209A014Q00026Q000200016Q00036Q000100046Q000200056Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300046Q0003000100124Q00083Q00206Q000900122Q0001000A8Q000200016Q00043Q00206Q00026Q000200016Q00068Q0001000100044Q004900012Q007F3Q00073Q0020445Q000B2Q00033Q00074Q007F3Q00073Q000E48010C002E00013Q0004A23Q002E00012Q007F3Q00023Q000675012Q002900013Q0004A23Q002900012Q007F3Q00023Q00201B014Q00022Q000E3Q000200012Q007F3Q00083Q00201B014Q000D0012270002000E4Q00B83Q000200012Q00D33Q00014Q007F7Q0030143Q0001000F9Q0000122Q000100113Q00202Q00010001001200122Q000200136Q000300076Q00010003000200104Q001000016Q00036Q000100096Q000200056Q00033Q00014Q0004000A3Q00202Q00040004001500102Q0003001400046Q0003000100124Q00083Q00206Q001600122Q000100173Q00069001023Q000100052Q007F8Q007F3Q00034Q007F3Q00094Q007F3Q00054Q007F3Q000A4Q00B83Q000200012Q00D33Q00013Q00013Q00043Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03053Q00436F6C6F7203063Q005374726F6B65000B4Q007F7Q0030463Q000100022Q0053012Q00016Q000100026Q000200036Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00613Q0003063Q00412Q63656E7403043Q005461627303083Q005F63752Q72656E740003053Q004672616D6503043Q004E616D6503063Q0057696E646F77030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407A40026Q00744003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03103Q00436C69707344657363656E64616E74732Q0103063Q0041637469766503063Q00506172656E74026Q00184003063Q005374726F6B65026Q00304003063Q00546F7042617203093Q005365636F6E64617279026Q00F03F026Q00414003063Q005A496E646578026Q001440026Q0020400285EB51B81E85EB3F03043Q006D2Q6F6E026Q00324003063Q00507572706C6503093Q00546578744C6162656C03053Q005469746C6503043Q0054657874030C3Q0056656C6F636974792048756203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q002Q40026Q0059C0026Q00344003013Q0078026Q0020C003063Q00436F6C6F723303073Q0066726F6D524742025Q00E06F40025Q0080564003053Q006D696E7573026Q0041C003083Q006B6579626F617264026Q004EC003093Q0041637469766174656403073Q00436F2Q6E65637403043Q00426F6479030E3Q005363726F2Q6C696E674672616D6503073Q005461624C697374029A5Q99C93F025Q00C05C40030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q7303103Q005363726F2Q6C696E67456E61626C6564030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D026Q00084003093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E67526967687403073Q00436F6E74656E74026Q005D40026Q005DC0030B3Q005369646544697669646572027Q0040030A3Q00496E707574426567616E03093Q00437265617465546162029C012Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100023Q0020740102000100010006750102000A00013Q0004A23Q000A00012Q007F00025Q0020740103000100010010180102000100032Q00E600023Q00022Q001100035Q00102Q00020002000300302Q0002000300044Q000300013Q00122Q000400056Q00053Q000A00302Q00050006000700122Q000600093Q00202Q00060006000A00122Q0007000B3Q0012270008000B4Q001001060008000200102Q00050008000600122Q0006000D3Q00202Q00060006000A00122Q0007000B3Q00122Q0008000E3Q00122Q0009000B3Q00122Q000A000E6Q0006000A000200102Q0005000C00060012F30006000D3Q00209100060006001000122Q000700113Q00122Q000800126Q00060008000200102Q0005000F00064Q00065Q00202Q00060006001400102Q00050013000600302Q00050015001600302Q00050017000E0030460005001800190030460005001A00192Q007F000600023Q0010180105001B00062Q00890003000500024Q000400036Q000500033Q00122Q0006001C6Q0004000600014Q000400046Q000500036Q00065Q00202Q00060006001D00122Q0007000B4Q00220004000700014Q000400056Q000500033Q00122Q0006001E3Q00122Q0007000B6Q0004000700014Q000400013Q00122Q000500056Q00063Q000800302Q00060006001F2Q007F00075Q0020740107000700200010180106001300070030460006001500160012F30007000D3Q0020D500070007000A00122Q000800213Q00122Q0009000E3Q00122Q000A000E3Q00122Q000B00226Q0007000B00020010180106000F000700304600060017000E0030460006002300240030460006001A00190010180106001B00032Q00420104000600022Q007F000500044Q0010000600044Q007F00075Q00207401070007001D2Q0050000800066Q0005000800014Q000500056Q000600043Q00122Q000700253Q00122Q000800266Q0005000800014Q000500073Q00122Q000600273Q00122Q000700284Q00AA000800014Q007F00095Q0020740109000900292Q00420105000900020012A5010600093Q00202Q00060006000A00122Q0007000E3Q00122Q0008000B6Q00060008000200102Q00050008000600122Q0006000D3Q00202Q00060006000A00122Q0007000E3Q00122Q000800253Q0012270009000B3Q001227000A000E4Q00420106000A00020010180105000C00060010180105001B00042Q007F000600013Q0012960107002A6Q00083Q000C00302Q00080006002B00202Q00090001002B00062Q0009007D000100010004A23Q007D00010012270009002D3Q0010180108002C00092Q0022010900083Q00102Q0008002E00094Q00095Q00202Q00090009002C00102Q0008002F000900302Q00080030003100122Q000900333Q00202Q00090009003200202Q00090009003400102Q0008003200090012F3000900333Q0020740109000900350020740109000900360010180108003500090030460008001500210012A5010900093Q00202Q00090009000A00122Q000A000E3Q00122Q000B000B6Q0009000B000200102Q00080008000900122Q0009000D3Q00202Q00090009000A00122Q000A000E3Q00122Q000B00373Q001293010C000B3Q00122Q000D000E6Q0009000D000200102Q0008000C000900122Q0009000D3Q00202Q00090009000A00122Q000A00213Q00122Q000B00383Q00122Q000C000E3Q00122Q000D00394Q00420109000D00020010180108000F00090010180108001B00042Q004201060008000200069001073Q000100072Q007F3Q00014Q007F8Q00103Q00044Q007F3Q00034Q007F3Q00074Q007F3Q00094Q007F3Q000A4Q0025000800073Q00122Q0009003A3Q00122Q000A003B3Q00122Q000B003C3Q00202Q000B000B003D00122Q000C003E3Q00122Q000D003F3Q00122Q000E003F6Q000B000E6Q00083Q00024Q000900073Q00122Q000A00403Q00122Q000B00416Q000C5Q00202Q000C000C002C4Q0009000C00024Q000A00073Q00122Q000B00423Q00122Q000C00436Q000D5Q00202Q000D000D00294Q000A000D00024Q000B5Q00202Q000C000A004400202Q000C000C0045000690010E00010001000D2Q007F3Q00024Q00103Q000B4Q007F3Q00014Q007F8Q007F3Q00034Q007F3Q00044Q007F3Q00054Q007F3Q00084Q007F3Q000B4Q007F3Q000C4Q007F3Q00094Q007F3Q000A4Q007F3Q000D4Q00B8000C000E00012Q007F000C00013Q001227000D00054Q00E6000E3Q0005003046000E00060046003046000E00150021002Q12000F000D3Q00202Q000F000F000A00122Q0010000E3Q00122Q0011000E3Q00122Q0012000E3Q00122Q001300226Q000F0013000200102Q000E000C000F00122Q000F000D3Q00202Q000F000F000A001227001000213Q0012230011000E3Q00122Q001200213Q00122Q001300416Q000F0013000200102Q000E000F000F00102Q000E001B00034Q000C000E00024Q000D00013Q00122Q000E00476Q000F3Q000B003046000F000600482Q007F00105Q002074011000100020001018010F00130010003046000F00150049003046000F0017000E002Q120010000D3Q00202Q00100010000A00122Q0011000E3Q00122Q0012004A3Q00122Q001300213Q00122Q0014000E6Q00100014000200102Q000F000F001000122Q0010000D3Q00202Q00100010000A2Q00B7001000010002001018010F004B00100012F3001000333Q00207401100010004D00207401100010004E001018010F004C0010003046000F004F000E0030C2000F0050001900302Q000F001A001900102Q000F001B000C4Q001000016Q001100013Q00122Q001200516Q00133Q000200122Q001400533Q00202Q00140014000A00122Q0015000E3Q001227001600544Q00420114001600020010180113005200140012F3001400333Q0020740114001400550020740114001400560010180113005500142Q00420111001300022Q007F001200013Q001255001300576Q00143Q000300122Q001500533Q00202Q00150015000A00122Q0016000E3Q00122Q0017001C6Q00150017000200102Q00140058001500122Q001500533Q00202Q00150015000A0012270016000E3Q0012270017001C4Q00420115001700020010180114005900150012F3001500533Q00207401150015000A0012270016000E3Q0012270017001C4Q00420115001700020010180114005A00152Q007C011200144Q005601103Q00012Q0042010D001000022Q0064010E00013Q00122Q000F00056Q00103Q000500302Q00100006005B00302Q00100015002100122Q0011000D3Q00202Q00110011000A00122Q0012000E3Q00122Q0013005C3Q00122Q0014000E3Q00122Q0015000E6Q00110015000200102Q0010000C001100122Q0011000D3Q00202Q00110011000A00122Q001200213Q00122Q0013005D3Q00122Q001400213Q00122Q0015000E6Q00110015000200102Q0010000F001100102Q0010001B000C4Q000E001000024Q000F00013Q00122Q001000056Q00113Q000800302Q00110006005E4Q00125Q00202Q00120012001D00102Q0011001300124Q001200063Q00102Q00110015001200302Q00110017000E00122Q0012000D3Q00202Q00120012000A00122Q0013000E3Q00122Q0014004A3Q00122Q0015000E3Q00122Q0016000E6Q00120016000200102Q0011000C001200122Q0012000D3Q00202Q00120012000A00122Q0013000E3Q00122Q001400213Q00122Q001500213Q00122Q0016000E6Q00120016000200102Q0011000F001200302Q00110023005F00102Q0011001B000C4Q000F001100014Q000F000D6Q001000036Q001100036Q001200046Q001300066Q001400056Q0011000300012Q00B8000F00110001002074010F0008004400201B010F000F004500069001110002000100052Q007F3Q000E4Q007F3Q00094Q00103Q00034Q007F3Q000A4Q007F3Q00024Q00B8000F001100012Q00AA000F5Q00207401100009004400201B01100010004500069001120003000100052Q00103Q000F4Q00103Q000C4Q007F3Q00094Q00103Q00034Q007F3Q000F4Q00B80010001200012Q00AA00106Q007F0011000B4Q007F0012000C3Q00207401120012006000069001130004000100032Q00103Q000B4Q00103Q00104Q00103Q00034Q00B800110013000100069001110005000100102Q007F3Q00014Q007F8Q00103Q000D4Q007F3Q00034Q007F3Q00074Q007F3Q00104Q00103Q000E4Q00103Q00024Q007F3Q00094Q007F3Q000F4Q007F3Q000A4Q007F3Q00044Q007F3Q00064Q007F3Q00084Q007F3Q00114Q007F3Q00123Q0010180102006100112Q0068010200024Q00D33Q00013Q00063Q001C3Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574026Q00364003063Q005A496E646578026Q00184003063Q00506172656E74026Q001440026Q002A4003073Q0053756254657874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503534Q006301035Q00122Q000400016Q00053Q000900302Q00050002000300302Q0005000400054Q000600013Q00202Q00060006000700102Q00050006000600302Q00050008000900122Q0006000B3Q00202Q00060006000C00122Q000700093Q00122Q0008000D6Q00060008000200102Q0005000A000600122Q0006000F3Q00202Q00060006000C00122Q000700096Q000800013Q00122Q0009000D3Q00122Q000A00106Q0006000A000200102Q0005000E000600122Q0006000F3Q00202Q00060006001200122Q000700133Q00122Q000800136Q00060008000200102Q00050011000600302Q0005001400154Q000600023Q00102Q0005001600064Q0003000500024Q000400036Q000500033Q00122Q000600176Q0004000600014Q000400046Q00055Q00122Q000600186Q00078Q000800013Q00202Q0008000800194Q00040008000200122Q0005000B3Q00202Q00050005000C00122Q0006000D3Q00122Q0007000D6Q00050007000200102Q0004000A000500122Q0005000F3Q00202Q00050005000C00122Q0006000D3Q00122Q000700103Q00122Q0008000D3Q00122Q000900106Q00050009000200102Q0004000E000500302Q00040014001500102Q00040016000300202Q00050003001A00202Q00050005001B00069001073Q000100062Q007F3Q00054Q00103Q00034Q007F3Q00064Q00103Q00044Q00103Q00024Q007F3Q00014Q00B800050007000100207401050003001C00201B01050005001B00069001070001000100052Q007F3Q00054Q00103Q00034Q007F3Q00064Q00103Q00044Q007F3Q00014Q00960005000700014Q000500036Q000600046Q000500038Q00013Q00023Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379028Q00030A3Q0054657874436F6C6F723303043Q005465787400124Q00679Q00000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00062Q0004000F000100010004A23Q000F00012Q007F000400053Q0020740104000400040010180103000300042Q00B83Q000300012Q00D33Q00017Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030A3Q0054657874436F6C6F723303073Q0053756254657874000F4Q007F8Q00BB000100016Q000200026Q00033Q000100302Q0003000100026Q000300012Q0053019Q000100036Q000200026Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00463Q00030E3Q0046696E6446697273744368696C6403143Q004B657962696E645069636B65724F7665726C617903073Q0044657374726F7903053Q004672616D6503043Q004E616D65030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407040025Q00805B4003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E64030F3Q00426F7264657253697A65506978656C03063Q005A496E646578026Q00594003063Q004163746976652Q0103063Q00506172656E74026Q00204003063Q00507572706C65029A5Q99C93F026Q00F83F026Q003440029A5Q99D93F03093Q00546578744C6162656C03043Q005465787403173Q00437573746F6D204B657962696E642053652Q74696E677303083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40026Q002440026Q00F03F026Q00324003163Q004261636B67726F756E645472616E73706172656E6379025Q0040594003093Q005365636F6E64617279026Q004240025Q00806B40026Q003E40026Q001440030A3Q0043552Q52454E543A205B03063Q00737472696E6703053Q00752Q70657203073Q0067657467656E7603093Q00546F2Q676C654B657903123Q005D0A5052452Q5320414E59204B45593Q2E026Q002640030E3Q005465787458416C69676E6D656E7403043Q00456E756D03063Q0043656E746572025Q00805940030A3Q005465787442752Q746F6E03063Q0043616E63656C03073Q005375625465787403073Q00456C656D656E74025Q00805240026Q003640030F3Q004175746F42752Q746F6E436F6C6F720100026Q00104003093Q0041637469766174656403073Q00436F2Q6E656374030A3Q00496E707574426567616E00F14Q007F7Q00201B014Q0001001227000200024Q0042012Q00020002000675012Q000B00013Q0004A23Q000B000100201B2Q013Q00032Q000E0001000200012Q00AA00016Q0003000100014Q00D33Q00014Q00AA000100014Q0003000100014Q007F000100023Q001227000200044Q00E600033Q00090030460003000500020012A5010400073Q00202Q00040004000800122Q000500093Q00122Q000600096Q00040006000200102Q00030006000400122Q0004000B3Q00202Q00040004000800122Q000500093Q00122Q0006000C3Q001227000700093Q0012270008000C4Q00420104000800020010180103000A00040012F30004000B3Q00209100040004000E00122Q0005000F3Q00122Q000600106Q00040006000200102Q0003000D00044Q000400033Q00202Q00040004001200102Q00030011000400302Q00030013000C00302Q0003001400150030460003001600172Q007F00045Q0010180103001800042Q00890001000300024Q000200046Q000300013Q00122Q000400196Q0002000400014Q000200056Q000300016Q000400033Q00202Q00040004001A00122Q0005001B3Q0012270006001C4Q00220002000600014Q000200066Q000300013Q00122Q0004001D3Q00122Q0005001E6Q0002000500014Q000200023Q00122Q0003001F6Q00043Q000900302Q0004002000212Q007F000500073Q00103A0004002200054Q000500033Q00202Q00050005002000102Q00040023000500302Q00040024002500122Q0005000B3Q00202Q00050005000800122Q0006000C3Q00122Q0007000C3Q00122Q0008000C3Q001227000900264Q001001050009000200102Q0004000A000500122Q0005000B3Q00202Q00050005000800122Q000600273Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900286Q00050009000200102Q0004000D000500304600040029002700304600040014002A0010180104001800012Q00420102000400022Q007F000300023Q001243010400046Q00053Q00074Q000600033Q00202Q00060006002B00102Q00050011000600122Q000600073Q00202Q00060006000800122Q000700093Q00122Q0008000C6Q00060008000200102Q00050006000600122Q0006000B3Q00202Q00060006000800122Q000700093Q00122Q0008000C3Q00122Q0009000C3Q00122Q000A002C6Q0006000A000200102Q0005000A000600122Q0006000B3Q00202Q00060006000800122Q0007000C3Q00122Q0008002D3Q00122Q0009000C3Q00122Q000A002E6Q0006000A000200102Q0005000D000600302Q00050013000C00302Q00050014002A00102Q0005001800014Q0003000500024Q000400046Q000500033Q00122Q0006002F6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006001A00122Q000700096Q0004000700024Q000500023Q00122Q0006001F6Q00073Q000900122Q000800303Q00122Q000900313Q00202Q00090009003200122Q000A00336Q000A0001000200202Q000A000A003400202Q000A000A00054Q00090002000200122Q000A00356Q00080008000A00102Q0007002000084Q000800073Q00102Q0007002200084Q000800033Q00202Q00080008001A00102Q00070023000800302Q00070024003600122Q000800383Q00202Q00080008003700202Q00080008003900102Q00070037000800122Q0008000B3Q00202Q00080008000800122Q000900273Q00122Q000A000C3Q00122Q000B00273Q00122Q000C000C6Q0008000C000200102Q0007000D000800302Q00070029002700302Q00070014003A00102Q0007001800034Q0005000700024Q000600023Q00122Q0007003B6Q00083Q000B00304600080020003C2Q007F000900073Q0010180108002200092Q007F000900033Q00207401090009003D0010180108002300090030460008002400362Q007F000900033Q00207401090009003E0010180108001100090012A5010900073Q00202Q00090009000800122Q000A00093Q00122Q000B000C6Q0009000B000200102Q00080006000900122Q0009000B3Q00202Q00090009000800122Q000A00093Q00122Q000B000C3Q001293010C000C3Q00122Q000D003F6Q0009000D000200102Q0008000A000900122Q0009000B3Q00202Q00090009000800122Q000A000C3Q00122Q000B00153Q00122Q000C000C3Q00122Q000D00404Q00420109000D00020010180108000D000900304600080041004200304600080014002A0010180108001800012Q00420106000800022Q007F000700044Q0010000800063Q001227000900434Q00B80007000900012Q001C000700073Q00069001083Q000100032Q00103Q00074Q00103Q00014Q007F3Q00013Q00200901090006004400202Q0009000900454Q000B00086Q0009000B00014Q000900086Q000A00093Q00202Q000A000A0046000690010B0001000100062Q007F3Q00014Q00103Q00084Q00103Q00054Q007F3Q000A4Q00103Q00044Q007F3Q000B4Q00600009000B00024Q000700096Q0009000C6Q000A00016Q000B00016Q000C00026Q000B000100012Q00B80009000B00012Q00D33Q00013Q00023Q00023Q00030A3Q00446973636F2Q6E65637403073Q0044657374726F7900114Q007F7Q000675012Q000800013Q0004A23Q000800012Q007F7Q00201B014Q00012Q000E3Q000200012Q001C8Q00038Q007F3Q00013Q000675012Q000E00013Q0004A23Q000E00012Q007F3Q00013Q00201B014Q00022Q000E3Q000200012Q00AA8Q00033Q00024Q00D33Q00017Q00163Q00030D3Q0055736572496E7075745479706503043Q00456E756D03083Q004B6579626F61726403073Q0067657467656E7603093Q00546F2Q676C654B657903073Q004B6579436F646503043Q0054657874030E3Q004E4557204B455942494E443A205B03063Q00737472696E6703053Q00752Q70657203043Q004E616D6503013Q005D030A3Q0054657874436F6C6F723303063Q00436F6C6F723303073Q0066726F6D524742026Q005E40025Q00E06F40026Q00644003053Q00436F6C6F7203043Q007461736B03043Q0077616974026Q00E03F02354Q007F00025Q0006E900020006000100010004A23Q000600012Q007F000200014Q00690002000100012Q00D33Q00013Q00207401023Q00010012F3000300023Q00207401030003000100207401030003000300066800020034000100030004A23Q003400010012F3000200044Q007301020001000200202Q00033Q000600102Q0002000500034Q000200023Q00122Q000300083Q00122Q000400093Q00202Q00040004000A00202Q00053Q000600202Q00050005000B4Q00040002000200122Q0005000C6Q00030003000500102Q0002000700034Q000200023Q00122Q0003000E3Q00202Q00030003000F00122Q000400103Q00122Q000500113Q00122Q000600126Q00030006000200102Q0002000D00034Q000200036Q000300046Q000400056Q00053Q000100122Q0006000E3Q00202Q00060006000F00122Q000700103Q00122Q000800113Q00122Q000900126Q00060009000200102Q0005001300064Q00020005000100122Q000200143Q00202Q00020002001500122Q000300166Q0002000200014Q000200016Q0002000100012Q00D33Q00017Q000A3Q0003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574030C3Q004162736F6C75746553697A6503013Q0058028Q0003043Q007461736B03043Q0077616974020AD7A3703D0AC73F03073Q0044657374726F7900184Q00F99Q00000100018Q000200016Q00016Q000100026Q000200036Q00033Q000100122Q000400023Q00202Q0004000400034Q000500023Q002074010500050004002074010500050005001227000600064Q00420104000600020010180103000100042Q00B83Q000300010012F33Q00073Q002074014Q0008001227000100094Q000E3Q000200012Q007F3Q00043Q00201B014Q000A2Q000E3Q000200012Q00D33Q00017Q000C3Q0003073Q0056697369626C65010003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00407A40026Q004140026Q00744003043Q007461736B03043Q007761697402B81E85EB51B8BE3F2Q0100264Q007F8Q00638Q00038Q007F7Q000675012Q001400013Q0004A23Q001400012Q007F3Q00013Q0030463Q000100022Q007F3Q00024Q00F0000100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300046Q000300010004A23Q002500012Q007F3Q00024Q00F0000100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600086Q00040006000200102Q0003000300046Q000300010012F33Q00093Q002074014Q000A0012270001000B4Q000E3Q000200012Q007F3Q00013Q0030463Q0001000C2Q00D33Q00017Q00063Q0003073Q004B6579436F646503073Q0067657467656E7603093Q00546F2Q676C654B657903043Q00456E756D03013Q004B03073Q0056697369626C6502193Q0006E900010005000100010004A23Q000500012Q007F00025Q0006750102000600013Q0004A23Q000600012Q00D33Q00013Q00207401023Q00010012F3000300024Q00B70003000100020020740103000300030006E90003000F000100010004A23Q000F00010012F3000300043Q00207401030003000100207401030003000500066800020018000100030004A23Q001800012Q007F000200014Q0063000200024Q0003000200014Q007F000200024Q007F000300014Q0063000300033Q0010180102000600032Q00D33Q00017Q00593Q0003063Q005F6F72646572028Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577026Q003C4003073Q0056697369626C6503063Q0048692Q64656E03063Q00506172656E74026Q00144003093Q0049636F6E436F6C6F7203073Q005375625465787403043Q0049636F6E03063Q00636972636C65026Q002E40030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q00184003093Q00546578744C6162656C03043Q004E616D652Q033Q0054616203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q003A40026Q003EC003053Q004672616D6503043Q0050616765030F3Q00426F7264657253697A65506978656C030E3Q005363726F2Q6C696E674672616D65030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q73027Q004003143Q005363726F2Q6C426172496D616765436F6C6F723303063Q005374726F6B65031A3Q005363726F2Q6C426172496D6167655472616E73706172656E6379030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D03093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70026Q002040030D3Q0050612Q64696E67426F2Q746F6D030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E675269676874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403043Q005F62746E03053Q005F69636F6E03053Q005F6E616D6503053Q005F7061676503053Q005F7772617003073Q005F73656C656374030A3Q005F69636F6E436F6C6F7203053Q007461626C6503063Q00696E7365727403043Q0054616273030D3Q0043726561746553656374696F6E030C3Q00437265617465546F2Q676C65030C3Q00437265617465536C69646572030D3Q0043726561746554657874626F78030B3Q004372656174654C6162656C030C3Q0043726561746542752Q746F6E03123Q00437265617465436F726E657242752Q746F6E030C3Q00437265617465427579526F770256012Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100024Q00E600023Q00010030A70102000100024Q00035Q00122Q000400036Q00053Q000700302Q00050004000500302Q0005000600074Q000600013Q00202Q00060006000900102Q00050008000600302Q0005000A000B0012F30006000D3Q0020D500060006000E00122Q0007000B3Q00122Q000800023Q00122Q000900023Q00122Q000A000F6Q0006000A00020010180105000C00060020740106000100112Q0063000600063Q0010180105001000062Q007F000600023Q00103B0005001200064Q0003000500024Q000400036Q000500033Q00122Q000600136Q00040006000100202Q00040001001400062Q00040026000100010004A23Q002600012Q007F000400013Q0020740104000400152Q007F000500043Q0020740106000100160006E90006002B000100010004A23Q002B0001001227000600173Q001227000700184Q00AA00086Q0010000900044Q00420105000900020012A50106001A3Q00202Q00060006000E00122Q000700023Q00122Q0008001B6Q00060008000200102Q00050019000600122Q0006000D3Q00202Q00060006000E00122Q000700023Q00122Q0008001D3Q0012270009001B3Q001227000A00024Q00420106000A00020010180105001C00060010180105001200032Q007F00065Q0012960107001E6Q00083Q000B00302Q0008000A000B00202Q00090001001F00062Q00090046000100010004A23Q00460001001227000900203Q0010180108000400092Q0022010900053Q00102Q0008002100094Q000900013Q00202Q00090009001500102Q00080022000900302Q00080023002400122Q000900263Q00202Q00090009002500202Q00090009002700102Q0008002500090012F3000900263Q00200701090009002800202Q00090009002900102Q00080028000900122Q0009001A3Q00202Q00090009000E00122Q000A00023Q00122Q000B001B6Q0009000B000200102Q00080019000900122Q0009000D3Q0020D500090009000E00122Q000A00023Q00122Q000B002A3Q00122Q000C001B3Q00122Q000D00026Q0009000D00020010180108001C00090012F30009000D3Q0020D500090009000E00122Q000A000B3Q00122Q000B002B3Q00122Q000C000B3Q00122Q000D00026Q0009000D00020010C10008000C000900102Q0008001200034Q0006000800024Q00075Q00122Q0008002C6Q00093Q000600302Q0009001F002D00302Q0009000A000B00302Q0009002E000200122Q000A000D3Q0020D5000A000A000E00122Q000B000B3Q00122Q000C00023Q00122Q000D000B3Q00122Q000E00026Q000A000E00020010180109000C000A0030460009001000072Q007F000A00063Q00101801090012000A2Q00380107000900024Q00085Q00122Q0009002F6Q000A3Q000900302Q000A000A000B00302Q000A002E000200122Q000B000D3Q00202Q000B000B000E00122Q000C000B3Q00122Q000D00023Q001227000E000B3Q001227000F00024Q0042010B000F0002001018010A000C000B0012F3000B000D3Q002074010B000B000E2Q00B7000B00010002001018010A0030000B0012F3000B00263Q002074010B000B0032002074010B000B0033001018010A0031000B0030A4010A003400354Q000B00013Q00202Q000B000B003700102Q000A0036000B00302Q000A0038001B00102Q000A001200074Q000B00016Q000C5Q00122Q000D00396Q000E3Q00020012F3000F003B3Q002074010F000F000E001227001000023Q0012270011001D4Q0042010F00110002001018010E003A000F0012F3000F00263Q002074010F000F003C002074010F000F003D001018010E003C000F2Q0042010C000E00022Q007F000D5Q001255000E003E6Q000F3Q000400122Q0010003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F003F001000122Q0010003B3Q00202Q00100010000E001227001100023Q001295001200406Q00100012000200102Q000F0041001000122Q0010003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F0042001000122Q0010003B3Q00207401100010000E001227001100023Q001227001200404Q0042011000120002001018010F004300102Q007C010D000F4Q0056010B3Q00012Q00420108000B000200069001093Q0001000B2Q007F3Q00074Q00103Q00024Q00103Q00014Q00103Q00074Q007F3Q00084Q007F3Q00094Q007F3Q000A4Q007F3Q00014Q00103Q00034Q00103Q00054Q00103Q00063Q002074010A0003004400201B010A000A0045000690010C0001000100042Q00103Q00074Q007F3Q00084Q00103Q00034Q007F3Q000A4Q00B8000A000C0001002074010A0003004600201B010A000A0045000690010C0002000100042Q00103Q00074Q007F3Q00084Q00103Q00034Q007F3Q000A4Q004C000A000C000100202Q000A0003004700202Q000A000A00454Q000C00096Q000A000C00014Q000A00036Q000B00056Q000C00066Q000D00086Q000E00073Q00102Q0002004D000900102Q0002004C000E00102Q0002004B000D00102Q0002004A000C00102Q00020049000B00102Q00020048000A00102Q0002004E000400122Q000A004F3Q00202Q000A000A00504Q000B00073Q00202Q000B000B00514Q000C00026Q000A000C00014Q000A00073Q00202Q000A000A00514Q000A000A3Q00262Q000A2Q002Q01000B0004A24Q002Q012Q0010000A00094Q0069000A00010001000690010A0003000100072Q00103Q00024Q007F8Q007F3Q00014Q00103Q00084Q007F3Q00034Q007F3Q000B4Q007F3Q000C3Q000690010B0004000100062Q00103Q00024Q007F8Q00103Q00084Q007F3Q00044Q007F3Q00014Q007F3Q000D3Q00101801020052000B000690010B0005000100082Q00103Q000A4Q007F8Q007F3Q00044Q007F3Q00014Q007F3Q00054Q007F3Q00034Q007F3Q00084Q007F3Q000A3Q00101801020053000B000690010B0006000100072Q00103Q000A4Q007F8Q007F3Q00054Q007F3Q00014Q007F3Q000D4Q007F3Q00034Q007F3Q000E3Q00101801020054000B000690010B0007000100092Q00103Q000A4Q007F8Q007F3Q00054Q007F3Q00014Q007F3Q00034Q007F3Q000B4Q007F3Q000C4Q007F3Q00084Q007F3Q000A3Q00101801020055000B000690010B0008000100062Q00103Q000A4Q007F3Q00044Q007F3Q00014Q007F8Q007F3Q00054Q007F3Q000D3Q00101801020056000B000690010B0009000100072Q00103Q000A4Q007F8Q007F3Q00044Q007F3Q00014Q007F3Q000D4Q007F3Q00084Q007F3Q000A3Q00101801020057000B000690010B000A0001000A2Q00103Q00084Q007F8Q007F3Q00014Q00103Q00074Q007F3Q00034Q007F3Q000B4Q007F3Q00044Q007F3Q000D4Q007F3Q00084Q007F3Q000A3Q00101801020058000B000690010B000B000100072Q00103Q000A4Q007F8Q007F3Q00054Q007F3Q00014Q007F3Q000D4Q007F3Q00034Q007F3Q000F3Q00101801020059000B2Q0068010200024Q00D33Q00013Q000C3Q00193Q0003083Q005F63752Q72656E7403063Q0048692Q64656E03063Q0069706169727303043Q005461627303053Q005F7772617003073Q0056697369626C6501002Q0103083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577027B14AE47E17AA43F028Q0003043Q005F62746E03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03053Q005F69636F6E030A3Q0054657874436F6C6F7233030A3Q005F69636F6E436F6C6F7203073Q005375625465787403053Q005F6E616D6503113Q0053656C656374656449636F6E436F6C6F7203093Q0049636F6E436F6C6F7203063Q00412Q63656E7403043Q005465787400914Q007F7Q002074014Q00012Q007F000100013Q0006683Q0006000100010004A23Q000600012Q00D33Q00014Q007F3Q00023Q002074014Q0002000675012Q003200013Q0004A23Q003200010012F33Q00034Q007F00015Q0020742Q01000100042Q006E012Q000200020004A23Q001400012Q007F000500013Q00067100040014000100050004A23Q001400010020740105000400050030460005000600070006FE3Q000F000100020004A23Q000F00012Q007F8Q00172Q0100013Q00104Q000100016Q00033Q00304Q000600086Q00033Q00122Q0001000A3Q00202Q00010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q00010005000200104Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00202Q00040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q00040008000200102Q0003000900046Q000300016Q00013Q0012F33Q00034Q007F00015Q0020742Q01000100042Q006E012Q000200020004A23Q005500012Q007F000500013Q0006710004003C000100050004A23Q003C00010020740105000400050030460005000600072Q007F000500043Q00207401060004000E2Q007F000700064Q00E600083Q00010030460008000F00102Q00B80005000800012Q007F000500043Q0020740106000400112Q007F000700064Q00E600083Q00010020740109000400130006E90009004B000100010004A23Q004B00012Q007F000900073Q0020740109000900140010180108001200092Q00940105000800014Q000500043Q00202Q0006000400154Q000700066Q00083Q00014Q000900073Q00202Q00090009001400102Q0008001200094Q0005000800010006FE3Q0037000100020004A23Q003700012Q007F8Q007F000100013Q001018012Q000100012Q007F3Q00033Q0030463Q000600082Q007F3Q00033Q0012F30001000A3Q0020D500010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q000100050002001018012Q000900012Q007F3Q00044Q007F000100034Q007F000200054Q00E600033Q00010012F30004000A3Q0020D500040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q0004000800020010180103000900042Q00B83Q000300012Q007F3Q00044Q00BB000100086Q000200066Q00033Q000100302Q0003000F000D6Q000300012Q0029012Q00046Q000100096Q000200066Q00033Q00014Q000400023Q00202Q00040004001600062Q00040086000100010004A23Q008600012Q007F000400023Q0020740104000400170006E900040086000100010004A23Q008600012Q007F000400073Q0020740104000400180010180103001200042Q00B83Q000300012Q0053012Q00046Q0001000A6Q000200066Q00033Q00014Q000400073Q00202Q00040004001900102Q0003001200046Q000300016Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q33E33F000B4Q007F7Q002074014Q00010006E93Q000A000100010004A23Q000A00012Q007F3Q00014Q00BB000100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q00D33Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F000B4Q007F7Q002074014Q00010006E93Q000A000100010004A23Q000A00012Q007F3Q00014Q00BB000100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q00D33Q00017Q000F3Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003C40030B3Q004C61796F75744F72646572030F3Q00426F7264657253697A65506978656C03063Q00506172656E74026Q00144003063Q005374726F6B6501284Q00F100018Q00025Q00202Q00020002000100202Q00020002000200102Q0001000100024Q000100013Q00122Q000200036Q00033Q00054Q000400023Q00202Q00040004000500101801030004000400127D000400073Q00202Q00040004000800122Q000500023Q00122Q000600093Q00122Q000700093Q00062Q0008001300013Q0004A23Q001300010012270008000A4Q00420104000800020010920103000600044Q00045Q00202Q00040004000100102Q0003000B000400302Q0003000C00094Q000400033Q00102Q0003000D00044Q0001000300024Q000200046Q000300013Q00122Q0004000E6Q0002000400014Q000200056Q000300016Q000400023Q00202Q00040004000F4Q000500066Q0002000500014Q000100028Q00017Q00213Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503163Q004261636B67726F756E645472616E73706172656E637903043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003640030B3Q004C61796F75744F7264657203063Q00506172656E7403043Q0049636F6E030D3Q00637562652D7665727465786573026Q002C4003093Q0049636F6E436F6C6F7203063Q00412Q63656E74030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E027Q004003093Q00546578744C6162656C03043Q005465787403043Q004E616D6503073Q0053656374696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q0036C002643Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100024Q007F00026Q008F01035Q00202Q00030003000100202Q00030003000200102Q0002000100034Q000200013Q00122Q000300036Q00043Q000400302Q00040004000200122Q000500063Q00202Q000500050007001227000600023Q0012FA000700083Q00122Q000800083Q00122Q000900096Q00050009000200102Q0004000500054Q00055Q00202Q00050005000100102Q0004000A00054Q000500023Q00102Q0004000B00052Q00420102000400022Q007F000300033Q00207401040001000C0006E900040020000100010004A23Q002000010012270004000D3Q0012270005000E4Q00AA00065Q00207401070001000F0006E900070027000100010004A23Q002700012Q007F000700043Q0020740107000700102Q00420103000700020012A5010400123Q00202Q00040004000700122Q000500083Q00122Q000600136Q00040006000200102Q00030011000400122Q000400063Q00202Q00040004000700122Q000500083Q00122Q000600153Q001227000700133Q001227000800084Q00420104000800020010180103001400040010180103000B00022Q007F000400013Q001296010500166Q00063Q000A00302Q00060004000200202Q00070001001800062Q0007003F000100010004A23Q003F0001001227000700193Q0010180106001700072Q0062010700053Q00102Q0006001A00074Q000700043Q00202Q00070007001700102Q0006001B000700302Q0006001C001D00122Q0007001F3Q00202Q00070007001E00202Q00070007002000102Q0006001E000700122Q000700123Q00202Q00070007000700122Q000800083Q00122Q000900136Q00070009000200102Q00060011000700122Q000700063Q00202Q00070007000700122Q000800083Q00122Q000900093Q00122Q000A00133Q00122Q000B00086Q0007000B000200102Q00060014000700122Q000700063Q00202Q00070007000700122Q000800023Q00122Q000900213Q00122Q000A00023Q00122Q000B00086Q0007000B000200102Q00060005000700102Q0006000B00024Q0004000600014Q000200028Q00017Q00353Q0003073Q0044656661756C74026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00546578744C6162656C03043Q004E616D6503063Q00546F2Q676C6503083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674025Q0080474003053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q66026Q0020C0030A3Q0066726F6D4F2Q66736574026Q002Q40026Q003040030F3Q00426F7264657253697A65506978656C026Q002CC0027Q0040026Q0018402Q033Q005365742Q033Q0047657403093Q0041637469766174656403073Q00436F2Q6E65637403083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03083Q00496E7374616E636502D73Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100023Q0020740102000100010006E900020008000100010004A23Q000800012Q00AA00026Q007F00035Q001280010400026Q0003000200024Q000400013Q00122Q000500036Q00063Q000400302Q00060004000500302Q00060006000700122Q000700093Q00202Q00070007000A00122Q000800073Q00122Q0009000B3Q00122Q000A00073Q00122Q000B000B6Q0007000B000200102Q00060008000700102Q0006000C00034Q00040006000200122Q0005000D3Q00202Q00060001000E00062Q0006003800013Q0004A23Q003800012Q007F000600023Q0020EF00070001000E00122Q0008000F6Q00095Q00202Q000A0001001000062Q000A0027000100010004A23Q002700012Q007F000A00033Q002074010A000A00112Q00420106000A0002001287010700133Q00202Q00070007000A00122Q0008000B3Q00122Q000900146Q00070009000200102Q00060012000700122Q000700093Q00202Q00070007000A00122Q0008000B3Q00122Q0009000D3Q00122Q000A00143Q00122Q000B000B6Q0007000B000200102Q00060015000700102Q0006000C000400122Q000500164Q007F000600013Q001296010700176Q00083Q000A00302Q00080006000700202Q00090001001800062Q00090040000100010004A23Q00400001001227000900193Q0010180108000400092Q0056000900043Q00102Q0008001A00094Q000900033Q00202Q00090009000400102Q0008001B000900302Q0008001C001D00122Q0009001F3Q00202Q00090009001E00202Q00090009002000102Q0008001E000900122Q000900133Q00202Q00090009000A00122Q000A000B3Q00122Q000B00146Q0009000B000200102Q00080012000900122Q000900093Q00202Q00090009000A00122Q000A000B6Q000B00053Q00122Q000C00143Q00122Q000D000B6Q0009000D000200102Q00080015000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00202Q000B000500214Q000B000B3Q00122Q000C00073Q00122Q000D000B6Q0009000D000200102Q00080008000900102Q0008000C00044Q0006000800014Q000600013Q00122Q000700226Q00083Q000600062Q0002006D00013Q0004A23Q006D00012Q007F000900033Q0020740109000900240006E90009006F000100010004A23Q006F00012Q007F000900033Q0020740109000900250010180108002300090012A5010900133Q00202Q00090009000A00122Q000A00073Q00122Q000B00146Q0009000B000200102Q00080012000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00122Q000B00263Q001227000C00143Q0012B6000D000B6Q0009000D000200102Q00080015000900122Q000900093Q00202Q00090009002700122Q000A00283Q00122Q000B00296Q0009000B000200102Q00080008000900302Q0008002A000B0010180108000C00042Q00420106000800022Q007F000700054Q0010000800063Q0012270009000D4Q00B80007000900012Q007F000700013Q001227000800224Q00E600093Q00062Q007F000A00033Q002074010A000A000400101801090023000A00120D010A00133Q00202Q000A000A000A00122Q000B000B3Q00122Q000C00146Q000A000C000200102Q00090012000A00062Q000200A200013Q0004A23Q00A200010012F3000A00093Q00209E000A000A000A00122Q000B00073Q00122Q000C002B3Q00122Q000D00143Q00122Q000E000B6Q000A000E000200062Q000A00A9000100010004A23Q00A900010012F3000A00093Q0020D5000A000A000A00122Q000B000B3Q00122Q000C002C3Q00122Q000D00143Q00122Q000E000B6Q000A000E000200101801090015000A0012B4000A00093Q00202Q000A000A002700122Q000B001D3Q00122Q000C001D6Q000A000C000200102Q00090008000A00302Q0009002A000B00102Q0009000C00064Q0007000900022Q008D010800056Q000900073Q00122Q000A002D6Q0008000A00014Q00085Q00069001093Q000100072Q00103Q00024Q007F3Q00064Q00103Q00064Q007F3Q00074Q007F3Q00034Q00103Q00074Q00103Q00013Q0010180108002E000900069001090001000100012Q00103Q00023Q0010180108002F000900207401090004003000201B010900090031000690010B0002000100022Q00103Q00084Q00103Q00024Q00B80009000B0001000675010200D400013Q0004A23Q00D40001002074010900010032000675010900D400013Q0004A23Q00D400010012F3000900333Q002074010900090034002074010A000100322Q00AA000B00014Q00B80009000B00010010180108003500032Q0068010800024Q00D33Q00013Q00033Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02344Q00372Q018Q000200016Q000300026Q000400036Q00053Q00014Q00065Q00062Q0006000C00013Q0004A23Q000C00012Q007F000600043Q0020740106000600020006E90006000E000100010004A23Q000E00012Q007F000600043Q0020740106000600030010180105000100062Q007C0002000500014Q000200016Q000300056Q000400036Q00053Q00014Q00065Q00062Q0006002000013Q0004A23Q002000010012F3000600053Q00209E00060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A000200062Q00060027000100010004A23Q002700010012F3000600053Q0020D500060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A00020010180105000400062Q00B80002000500012Q007F000200063Q00207401020002000C0006750102003300013Q0004A23Q003300010012F30002000D3Q00204D01020002000E4Q000300063Q00202Q00030003000C4Q00048Q0002000400012Q00D33Q00019Q002Q0001034Q007F00016Q00682Q0100024Q00D33Q00017Q00013Q002Q033Q0053657400064Q006B7Q00206Q00014Q000200016Q000200028Q000200016Q00017Q003A3Q002Q033Q004D696E028Q002Q033Q004D6178026Q00594003093Q00496E6372656D656E74026Q00F03F03043Q006D61746803053Q00636C616D7003073Q0044656661756C74026Q00454003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E637903043Q005465787403043Q004E616D6503063Q00536C6964657203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q002040026Q00104003043Q0053697A65026Q004EC0026Q002C4003063Q00506172656E7403083Q00746F737472696E6703063Q00412Q63656E7403053Q005269676874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q0020C0026Q004940030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66026Q00E03F026Q0026C0026Q0030C0026Q002440030F3Q00426F7264657253697A65506978656C026Q00084003053Q004672616D65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578027Q0040026Q0018402Q033Q005365742Q033Q0047657403083Q00496E7374616E636502E53Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100023Q0020740102000100010006E900020008000100010004A23Q00080001001227000200023Q0020740103000100030006E90003000C000100010004A23Q000C0001001227000300043Q0020740104000100050006E900040010000100010004A23Q00100001001227000400063Q0012F3000500073Q0020740105000500080020740106000100090006E900060016000100010004A23Q001600012Q0010000600024Q0010000700024Q0010000800034Q00420105000800022Q007F00065Q0012270007000A4Q00FF0006000200022Q007F000700013Q0012960108000B6Q00093Q000900302Q0009000C000600202Q000A0001000E00062Q000A0024000100010004A23Q00240001001227000A000F3Q0010180109000D000A2Q0022010A00023Q00102Q00090010000A4Q000A00033Q00202Q000A000A000D00102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A001600102Q00090014000A002Q12000A00183Q00202Q000A000A001900122Q000B00023Q00122Q000C001A3Q00122Q000D00023Q00122Q000E001B6Q000A000E000200102Q00090017000A00122Q000A00183Q00202Q000A000A0019001227000B00063Q001227000C001D3Q00124A000D00023Q00122Q000E001E6Q000A000E000200102Q0009001C000A00102Q0009001F00064Q0007000900014Q000700013Q00122Q0008000B6Q00093Q000A00302Q0009000C00060012F3000A00204Q0010000B00054Q00FF000A000200020010180109000D000A2Q0022010A00043Q00102Q00090010000A4Q000A00033Q00202Q000A000A002100102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A002200102Q00090014000A0012A5010A00243Q00202Q000A000A001900122Q000B00063Q00122Q000C00026Q000A000C000200102Q00090023000A00122Q000A00183Q00202Q000A000A001900122Q000B00063Q00122Q000C00253Q001293010D00023Q00122Q000E001B6Q000A000E000200102Q00090017000A00122Q000A00183Q00202Q000A000A001900122Q000B00023Q00122Q000C00263Q00122Q000D00023Q00122Q000E001E4Q0042010A000E00020010180109001C000A0010180109001F00062Q00420107000900022Q007F000800013Q001227000900274Q00E6000A3Q0008003046000A000D0028003046000A0029002A2Q007F000B00033Q002074010B000B002C001018010A002B000B0012F3000B00243Q002074010B000B0019001227000C00023Q00128A000D002D6Q000B000D000200102Q000A0023000B00122Q000B00183Q00202Q000B000B001900122Q000C00023Q00122Q000D001A3Q00122Q000E00063Q00122Q000F002E6Q000B000F000200102Q000A0017000B00122Q000B00183Q00202Q000B000B001900122Q000C00063Q00122Q000D002F3Q00122Q000E00023Q00122Q000F00306Q000B000F000200102Q000A001C000B00302Q000A0031000200102Q000A001F00064Q0008000A00024Q000900056Q000A00083Q00122Q000B00326Q0009000B00014Q000900013Q00122Q000A00336Q000B3Q00044Q000C00033Q00202Q000C000C002100102Q000B002B000C00122Q000C00183Q00202Q000C000C00194Q000D000500024Q000E000300024Q000D000D000E00122Q000E00023Q00122Q000F00063Q00122Q001000026Q000C0010000200102Q000B001C000C00302Q000B0031000200102Q000B001F00084Q0009000B00024Q000A00056Q000B00093Q00122Q000C00326Q000A000C00014Q000A00013Q00122Q000B00336Q000C3Q00074Q000D00033Q00202Q000D000D000D00102Q000C002B000D00122Q000D00243Q00202Q000D000D001900122Q000E002D3Q00122Q000F002D6Q000D000F000200102Q000C0023000D00122Q000D00183Q00202Q000D000D00194Q000E000500024Q000F000300024Q000E000E000F00122Q000F00023Q00122Q0010002D3Q00122Q001100026Q000D0011000200102Q000C0017000D00122Q000D00183Q00202Q000D000D003400122Q000E00133Q00122Q000F00136Q000D000F000200102Q000C001C000D00302Q000C0031000200302Q000C0035003600102Q000C001F00082Q0042010A000C00022Q008D010B00056Q000C000A3Q00122Q000D00376Q000B000D00014Q000B5Q000690010C3Q000100082Q00103Q00024Q00103Q00034Q00103Q00054Q00103Q00044Q00103Q00094Q00103Q000A4Q00103Q00074Q00103Q00014Q007F000D00064Q0010000E00083Q000690010F0001000100012Q00103Q000C4Q00B8000D000F0001000690010D0002000100032Q00103Q000C4Q00103Q00024Q00103Q00033Q001018010B0038000D000690010D0003000100012Q00103Q00053Q001018010B0039000D001018010B003A00062Q0068010B00024Q00D33Q00013Q00043Q000F3Q0003043Q006D61746803053Q00636C616D7003053Q00666C2Q6F72026Q00E03F028Q0003043Q0053697A6503053Q005544696D322Q033Q006E6577026Q00F03F03083Q00506F736974696F6E03043Q005465787403083Q00746F737472696E6703083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02474Q005F01028Q000300016Q00048Q0003000300044Q000300036Q00020002000300122Q000300013Q00202Q00030003000200122Q000400013Q00202Q0004000400034Q000500036Q00050002000500202Q0005000500044Q0004000200024Q000500036Q0004000400054Q00058Q000600016Q0003000600024Q000300026Q000300016Q00048Q00030003000400262Q0003001C000100050004A23Q001C0001001227000300053Q0006E900030023000100010004A23Q002300012Q007F000300024Q00CF00048Q0003000300044Q000400016Q00058Q0004000400054Q0003000300042Q007F000400043Q00124E000500073Q00202Q0005000500084Q000600033Q00122Q000700053Q00122Q000800093Q00122Q000900056Q00050009000200102Q0004000600054Q000400053Q00122Q000500073Q00202Q0005000500084Q000600033Q00122Q000700053Q00122Q000800043Q00122Q000900056Q00050009000200102Q0004000A00054Q000400063Q00122Q0005000C6Q000600026Q00050002000200102Q0004000B000500062Q0001004600013Q0004A23Q004600012Q007F000400073Q00207401040004000D0006750104004600013Q0004A23Q004600010012F30004000E3Q00204D01040004000F4Q000500073Q00202Q00050005000D4Q000600026Q0004000600012Q00D33Q00019Q002Q0001054Q007F00016Q001000026Q00AA000300014Q00B80001000300012Q00D33Q00017Q00023Q0003043Q006D61746803053Q00636C616D7002104Q007F00025Q0012F3000300013Q0020740103000300022Q0010000400014Q007F000500014Q007F000600024Q00420103000600022Q00CF000400016Q0003000300044Q000400026Q000500016Q0004000400054Q0003000300042Q00AA000400014Q00B80002000400012Q00D33Q00019Q002Q0001034Q007F00016Q00682Q0100024Q00D33Q00017Q00453Q00026Q003C4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503073Q0054657874626F7803083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65029A5Q99D93F026Q0020C003063Q00506172656E7403053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E64617279026Q0018C0026Q003440030D3Q004175746F6D6174696353697A6503013Q005803103Q00436C69707344657363656E64616E74732Q01030F3Q00426F7264657253697A65506978656C03093Q00554950612Q64696E67030B3Q0050612Q64696E674C65667403043Q005544696D026Q001440030C3Q0050612Q64696E675269676874026Q00104003063Q005374726F6B6503073Q0054657874426F7803073Q0044656661756C74034Q00030F3Q00506C616365686F6C64657254657874030B3Q00506C616365686F6C6465722Q033Q003Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q0053756254657874026Q00264003103Q00436C656172546578744F6E466F637573010003103Q00554953697A65436F6E73747261696E7403073Q004D696E53697A6503083Q004D696E5769647468025Q0080464003073Q004D617853697A6503083Q004D61785769647468025Q0080614003043Q006D61746803043Q006875676503073Q00466F637573656403073Q00436F2Q6E65637403093Q00466F6375734C6F73742Q033Q005365742Q033Q0047657403083Q00496E7374616E636502D23Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100024Q007F00025Q001227000300014Q00FF0002000200022Q007F000300013Q001296010400026Q00053Q000A00302Q00050003000400202Q00060001000600062Q0006000F000100010004A23Q000F0001001227000600073Q0010180105000500062Q0022010600023Q00102Q0005000800064Q000600033Q00202Q00060006000500102Q00050009000600302Q0005000A000B00122Q0006000D3Q00202Q00060006000C00202Q00060006000E00102Q0005000C00060012A5010600103Q00202Q00060006001100122Q000700123Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00202Q00060006001100122Q000700123Q00122Q000800163Q001293010900133Q00122Q000A00126Q0006000A000200102Q00050014000600122Q000600153Q00202Q00060006001100122Q000700183Q00122Q000800193Q00122Q000900043Q00122Q000A00124Q00420106000A00020010180105001700060010180105001A00022Q00B80003000500012Q007F000300013Q0012270004001B4Q00E600053Q00082Q007F000600033Q00207401060006001D0010180105001C00060012A5010600103Q00202Q00060006001100122Q000700043Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00202Q00060006001100122Q000700043Q00122Q0008001E3Q001293010900133Q00122Q000A00126Q0006000A000200102Q00050014000600122Q000600153Q00202Q00060006001100122Q000700123Q00122Q000800123Q00122Q000900123Q00122Q000A001F4Q00420106000A00020010180105001700060012F30006000D3Q0020740106000600200020740106000600210010180105002000060030C200050022002300302Q00050024001200102Q0005001A00024Q00068Q000700013Q00122Q000800256Q00093Q000200122Q000A00273Q00202Q000A000A001100122Q000B00123Q001227000C00284Q0042010A000C000200101801090026000A0012F3000A00273Q002074010A000A0011001227000B00123Q001227000C00284Q0042010A000C000200101801090029000A2Q007C010700094Q005601063Q00012Q00420103000600022Q0079010400046Q000500033Q00122Q0006002A6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006002B4Q000700066Q0004000700022Q007F000500013Q0012960106002C6Q00073Q000C00302Q00070003000400202Q00080001002D00062Q0008007A000100010004A23Q007A00010012270008002E3Q0010180107000500080020740108000100300006E90008007F000100010004A23Q007F0001001227000800313Q0010180107002F00082Q0005010800033Q00202Q00080008003300102Q0007003200084Q000800023Q00102Q0007000800084Q000800033Q00202Q00080008000500102Q00070009000800302Q0007000A003400302Q00070035003600122Q0008000D3Q00202Q00080008000C00202Q00080008000E00102Q0007000C000800122Q0008000D3Q00202Q00080008002000202Q00080008002100102Q00070020000800122Q000800153Q00202Q00080008001100122Q000900123Q00122Q000A00123Q00122Q000B00043Q00122Q000C00126Q0008000C000200102Q00070017000800102Q0007001A00034Q00088Q000900013Q00122Q000A00376Q000B3Q000200122Q000C00103Q00202Q000C000C001100202Q000D0001003900062Q000D00A5000100010004A23Q00A50001001227000D003A3Q001227000E00124Q006F010C000E000200102Q000B0038000C00122Q000C00103Q00202Q000C000C001100202Q000D0001003C00062Q000D00AE000100010004A23Q00AE0001001227000D003D3Q0012F3000E003E3Q002074010E000E003F2Q0042010C000E0002001018010B003B000C2Q007C0109000B4Q005601083Q00012Q004201050008000200207401060005004000201B01060006004100069001083Q000100042Q007F3Q00074Q00103Q00044Q007F3Q00084Q007F3Q00034Q00B800060008000100207401060005004200201B01060006004100069001080001000100072Q007F3Q00074Q00103Q00044Q007F3Q00084Q007F3Q00034Q007F3Q00064Q00103Q00014Q00103Q00054Q00B80006000800012Q00E600063Q000300069001070002000100012Q00103Q00053Q00101801060043000700069001070003000100012Q00103Q00053Q0010180106004400070010180106004500022Q0068010600024Q00D33Q00013Q00043Q00043Q0003053Q00436F6C6F7203063Q00412Q63656E74030C3Q005472616E73706172656E6379029A5Q99C93F000A4Q003D9Q00000100016Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030001000400302Q0003000300046Q000300016Q00017Q00073Q0003053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E637903083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03043Q005465787400164Q007F8Q007F000100014Q007F000200024Q00E600033Q00022Q007F000400033Q0020740104000400020010180103000100042Q007F000400043Q0010180103000300042Q00B83Q000300012Q007F3Q00053Q002074014Q0004000675012Q001500013Q0004A23Q001500010012F33Q00053Q002024014Q00064Q000100053Q00202Q0001000100044Q000200063Q00202Q0002000200076Q000200012Q00D33Q00017Q00013Q0003043Q005465787402034Q007F00025Q0010180102000100012Q00D33Q00017Q00013Q0003043Q005465787400044Q007F7Q002074014Q00012Q0068012Q00024Q00D33Q00017Q00253Q00026Q003C40026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D3203063Q00506172656E74026Q003A4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503053Q004C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403043Q0053697A6503053Q0056616C7565034Q0003063Q00412Q63656E7403053Q005269676874026Q0020C02Q033Q005365742Q033Q0047657403083Q00496E7374616E636502853Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100024Q007F00025Q001245010300016Q00020002000200122Q000300023Q00202Q00040001000300062Q0004002500013Q0004A23Q002500012Q007F000400013Q0020EF00050001000300122Q000600046Q00075Q00202Q00080001000500062Q00080014000100010004A23Q001400012Q007F000800023Q0020740108000800062Q0042010400080002001287010500083Q00202Q00050005000900122Q0006000A3Q00122Q0007000B6Q00050007000200102Q00040007000500122Q0005000D3Q00202Q00050005000900122Q0006000A3Q00122Q000700023Q00122Q0008000B3Q00122Q0009000A6Q00050009000200102Q0004000C000500102Q0004000E000200122Q0003000F4Q007F000400033Q001296010500106Q00063Q000A00302Q00060011001200202Q00070001001400062Q0007002D000100010004A23Q002D0001001227000700153Q0010180106001300072Q0022010700043Q00102Q0006001600074Q000700023Q00202Q00070007001300102Q00060017000700302Q00060018001900122Q0007001B3Q00202Q00070007001A00202Q00070007001C00102Q0006001A00070012F3000700083Q0020740107000700090012270008000A3Q0012270009000B4Q00420107000900020010180106000700070012F30007000D3Q0020740107000700090012270008000A4Q0010000900033Q001227000A000B3Q001227000B000A4Q00420107000B00020010180106000C00070012F30007000D3Q0020740107000700090012270008000B4Q0058010900033Q001227000A00123Q001227000B000A4Q00420107000B00020010180106001D00070010180106000E00022Q00420104000600022Q007F000500033Q001296010600106Q00073Q000A00302Q00070011001200202Q00080001001E00062Q00080058000100010004A23Q005800010012270008001F3Q0010180107001300082Q0022010800053Q00102Q0007001600084Q000800023Q00202Q00080008002000102Q00070017000800302Q00070018001900122Q0008001B3Q00202Q00080008001A00202Q00080008002100102Q0007001A00080012A5010800083Q00202Q00080008000900122Q000900123Q00122Q000A000B6Q0008000A000200102Q00070007000800122Q0008000D3Q00202Q00080008000900122Q000900123Q00122Q000A00223Q001293010B000B3Q00122Q000C000A6Q0008000C000200102Q0007000C000800122Q0008000D3Q00202Q00080008000900122Q0009000B3Q00122Q000A00223Q00122Q000B00123Q00122Q000C000A4Q00420108000C00020010180107001D00080010180107000E00022Q00420105000700022Q00E600063Q000300069001073Q000100012Q00103Q00053Q00101801060023000700069001070001000100012Q00103Q00053Q0010180106002400070010180106002500022Q0068010600024Q00D33Q00013Q00023Q00023Q0003043Q005465787403083Q00746F737472696E6702064Q007F00025Q0012F3000300024Q0010000400014Q00FF0003000200020010180102000100032Q00D33Q00017Q00013Q0003043Q005465787400044Q007F7Q002074014Q00012Q0068012Q00024Q00D33Q00017Q002C3Q00026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00526967687449636F6E030E3Q00526967687449636F6E436F6C6F72026Q0020C003093Q00546578744C6162656C03043Q004E616D6503063Q0042752Q746F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E6365030C3Q00536574526967687449636F6E02A53Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100024Q007F00025Q001207000300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000500302Q00050003000400302Q00050005000600302Q00050007000800122Q0006000A3Q00202Q00060006000B00122Q000700083Q00122Q0008000C3Q00122Q000900083Q00122Q000A000C6Q0006000A000200102Q00050009000600102Q0005000D00024Q00030005000200122Q0004000E3Q00202Q00050001000F00062Q0005003500013Q0004A23Q003500012Q007F000500023Q0020EF00060001000F00122Q000700106Q00085Q00202Q00090001001100062Q00090024000100010004A23Q002400012Q007F000900033Q0020740109000900122Q0042010500090002001287010600143Q00202Q00060006000B00122Q0007000C3Q00122Q000800156Q00060008000200102Q00050013000600122Q0006000A3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000E3Q00122Q000900153Q00122Q000A000C6Q0006000A000200102Q00050016000600102Q0005000D000300122Q000400174Q001C000500053Q0012270006000C3Q0020740107000100180006750107005500013Q0004A23Q005500012Q007F000700023Q0020EF00080001001800122Q000900106Q000A5Q00202Q000B0001001900062Q000B0043000100010004A23Q004300012Q007F000B00033Q002074010B000B00122Q00420107000B00022Q00E1000500073Q00122Q000700143Q00202Q00070007000B00122Q000800083Q00122Q000900156Q00070009000200102Q00050013000700122Q0007000A3Q00202Q00070007000B00122Q000800083Q00122Q0009001A3Q00122Q000A00153Q00122Q000B000C6Q0007000B000200102Q00050016000700102Q0005000D000300122Q000600174Q007F000700013Q0012960108001B6Q00093Q000B00302Q00090007000800202Q000A0001001C00062Q000A005D000100010004A23Q005D0001001227000A001D3Q00101801090003000A2Q0022010A00043Q00102Q0009001E000A4Q000A00033Q00202Q000A000A000300102Q0009001F000A00302Q00090020002100122Q000A00233Q00202Q000A000A002200202Q000A000A002400102Q00090022000A0012F3000A00233Q002007010A000A002500202Q000A000A002600102Q00090025000A00122Q000A00143Q00202Q000A000A000B00122Q000B000C3Q00122Q000C00156Q000A000C000200102Q00090013000A00122Q000A000A3Q002074010A000A000B00120A000B000C6Q000C00043Q00122Q000D00153Q00122Q000E000C6Q000A000E000200102Q00090016000A00122Q000A000A3Q00202Q000A000A000B00122Q000B00086Q000C000400062Q0058010C000C3Q00129B010D00083Q00122Q000E000C6Q000A000E000200102Q00090009000A00102Q0009000D00034Q00070009000200202Q00080003002700202Q000800080028000690010A3Q000100042Q007F3Q00054Q00103Q00024Q007F3Q00064Q007F3Q00034Q00B80008000A000100207401080003002900201B010800080028000690010A0001000100042Q007F3Q00054Q00103Q00024Q007F3Q00064Q007F3Q00034Q00B80008000A000100207401080003002A00201B010800080028000690010A0002000100052Q007F3Q00054Q00103Q00024Q007F3Q00064Q007F3Q00034Q00103Q00014Q00B80008000A00012Q00E600083Q00020010180108002B000200069001090003000100012Q00103Q00053Q0010180108002C00092Q0068010800024Q00D33Q00013Q00043Q00023Q0003103Q004261636B67726F756E64436F6C6F7233030C3Q00456C656D656E74486F76657200094Q0053019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q0053019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00073Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E7403043Q007461736B03053Q0064656C6179029A5Q99B93F03083Q0043612Q6C6261636B03053Q00737061776E001B4Q008E019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q0003000100124Q00033Q00206Q000400122Q000100053Q00069001023Q000100042Q007F8Q007F3Q00014Q007F3Q00024Q007F3Q00034Q00B83Q000200012Q007F3Q00043Q002074014Q0006000675012Q001A00013Q0004A23Q001A00010012F33Q00033Q002074014Q00072Q007F000100043Q0020742Q01000100062Q000E3Q000200012Q00D33Q00013Q00013Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q0053019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00013Q0003043Q005465787402064Q007F00025Q0006750102000500013Q0004A23Q000500012Q007F00025Q0010180102000100012Q00D33Q00017Q00353Q0003043Q0054657874025Q00805240026Q003A4003153Q0046696E6446697273744368696C644F66436C612Q7303093Q00554950612Q64696E67030D3Q0050612Q64696E67426F2Q746F6D03043Q005544696D2Q033Q006E6577028Q00026Q004640030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E6461727903163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00F03F03083Q00506F736974696F6E03053Q005544696D32026Q0028C0026Q0024C003043Q0053697A65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578026Q00244003063Q00506172656E74026Q002A4003063Q005374726F6B65026Q33E33F03043Q0049636F6E03063Q00636972636C6503073Q0053756254657874026Q002640026Q00E03F026Q0020C003093Q00546578744C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q003EC0030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E636502B43Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100023Q0020740102000100010006750102000A00013Q0004A23Q000A0001001227000300023Q0006E90003000B000100010004A23Q000B0001001227000300034Q007F00045Q00201B010400040004001227000600054Q00420104000600020006750104001700013Q0004A23Q001700010012F3000500073Q002074010500050008001227000600093Q0012270007000A4Q00420105000700020010180104000600052Q007F000500013Q00127A0006000B6Q00073Q000900302Q00070001000C00302Q0007000D000E4Q000800023Q00202Q00080008001000102Q0007000F000800302Q00070011001200122Q000800143Q00202Q00080008000800122Q000900153Q00122Q000A00156Q0008000A000200102Q00070013000800122Q000800173Q00202Q00080008000800122Q000900153Q00122Q000A00183Q00122Q000B00153Q00122Q000C00196Q0008000C000200102Q00070016000800122Q000800173Q00202Q00080008001B4Q000900033Q00122Q000A00036Q0008000A000200102Q0007001A000800302Q0007001C001D4Q000800033Q00102Q0007001E00084Q0005000700024Q000600046Q000700053Q00122Q0008001F6Q0006000800014Q000600056Q000700056Q000800023Q00202Q00080008002000122Q000900216Q0006000900024Q000700063Q00202Q00080001002200062Q00080047000100010004A23Q00470001001227000800233Q0012270009001F4Q002A000A8Q000B00023Q00202Q000B000B00244Q0007000B000200302Q0007001C002500102Q0007001E00054Q000800083Q00062Q0002008900013Q0004A23Q008900010012F3000900143Q00207101090009000800122Q000A00153Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00153Q00122Q000B00273Q00122Q000C00263Q001227000D00094Q00E70009000D000200102Q0007001600094Q000900013Q00122Q000A00286Q000B3Q000B00302Q000B0011001500102Q000B000100024Q000C00073Q00102Q000B0029000C4Q000C00023Q002074010C000C0024001018010B002A000C003050010B002B002C00122Q000C002E3Q00202Q000C000C002D00202Q000C000C002F00102Q000B002D000C00122Q000C00143Q00202Q000C000C000800122Q000D00093Q00122Q000E00266Q000C000E0002001018010B0013000C002Q12000C00173Q00202Q000C000C000800122Q000D00093Q00122Q000E00253Q00122Q000F00263Q00122Q001000096Q000C0010000200102Q000B0016000C00122Q000C00173Q00202Q000C000C0008001227000D00153Q001227000E00303Q001227000F00153Q001227001000094Q0042010C00100002001018010B001A000C003046000B001C0025001018010B001E00052Q00420109000B00022Q0010000800093Q0004A23Q009700010012F3000900143Q00207101090009000800122Q000A00263Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00263Q00122Q000B00093Q00122Q000C00263Q001227000D00094Q00420109000D000200101801070016000900207401090005003100201B010900090032000690010B3Q000100062Q007F3Q00084Q00103Q00074Q007F3Q00094Q007F3Q00024Q00103Q00064Q00103Q00084Q00B80009000B000100207401090005003300201B010900090032000690010B0001000100062Q007F3Q00084Q00103Q00074Q007F3Q00094Q007F3Q00024Q00103Q00064Q00103Q00084Q00B80009000B000100207401090005003400201B010900090032000690010B0002000100012Q00103Q00014Q00B80009000B00012Q00E600093Q00010010180109003500052Q0068010900024Q00D33Q00013Q00033Q00063Q00030A3Q0054657874436F6C6F723303063Q00412Q63656E7403053Q00436F6C6F72030C3Q005472616E73706172656E6379029A5Q99C93F03043Q0054657874001D4Q009D019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030003000400302Q0003000400056Q000300016Q00053Q00064Q001C00013Q0004A23Q001C00012Q007F8Q003C000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000600102Q0003000100046Q000300012Q00D33Q00017Q00063Q00030A3Q0054657874436F6C6F723303073Q005375625465787403053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q33E33F001D4Q009D019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000400102Q00030003000400302Q0003000500066Q000300016Q00053Q00064Q001C00013Q0004A23Q001C00012Q007F8Q003C000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q00D33Q00017Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q007F7Q002074014Q0001000675012Q000900013Q0004A23Q000900010012F33Q00023Q002074014Q00032Q007F00015Q0020742Q01000100012Q000E3Q000200012Q00D33Q00017Q002D3Q00026Q003E4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403013Q002D03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65025Q004060C003063Q00506172656E74034Q0003063Q00412Q63656E7403053Q005269676874026Q004CC0026Q005040030A3Q005465787442752Q746F6E2Q033Q0042757903103Q004261636B67726F756E64436F6C6F7233030F3Q004175746F42752Q746F6E436F6C6F720100026Q0018C0030A3Q0066726F6D4F2Q66736574026Q004640026Q003440026Q00104003093Q0041637469766174656403073Q00436F2Q6E65637403083Q00496E7374616E63652Q033Q0053657402913Q0006E900010004000100010004A23Q000400012Q00E600026Q0010000100024Q007F00025Q00121C010300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000B00302Q00050003000400302Q0005000500064Q000600023Q00102Q0005000700064Q000600033Q00207401060006000500104101050008000600302Q00050009000A00122Q0006000C3Q00202Q00060006000B00202Q00060006000D00102Q0005000B000600122Q0006000C3Q00202Q00060006000E00202Q00060006000F00102Q0005000E00060012A5010600113Q00202Q00060006001200122Q000700133Q00122Q000800146Q00060008000200102Q00050010000600122Q000600163Q00202Q00060006001200122Q000700133Q00122Q000800173Q001293010900143Q00122Q000A00136Q0006000A000200102Q00050015000600122Q000600163Q00202Q00060006001200122Q000700043Q00122Q000800193Q00122Q000900043Q00122Q000A00134Q00420106000A00020010180105001800060010180105001A00022Q00F20003000500024Q000400013Q00122Q000500026Q00063Q000A00302Q00060003000400302Q00060005001B4Q000700043Q00102Q0006000700074Q000700033Q00202Q00070007001C00101801060008000700305001060009000A00122Q0007000C3Q00202Q00070007000B00202Q00070007001D00102Q0006000B000700122Q000700113Q00202Q00070007001200122Q000800043Q00122Q000900146Q000700090002001018010600100007002Q12000700163Q00202Q00070007001200122Q000800043Q00122Q0009001E3Q00122Q000A00143Q00122Q000B00136Q0007000B000200102Q00060015000700122Q000700163Q00202Q000700070012001227000800133Q0012230009001F3Q00122Q000A00043Q00122Q000B00136Q0007000B000200102Q00060018000700102Q0006001A00024Q0004000600024Q000500013Q00122Q000600206Q00073Q000A0030460007000500212Q007F000800043Q0010180107000700082Q007F000800033Q00207401080008000500101801070008000800304600070009000A2Q007F000800033Q00207401080008001C0010180107002200080030460007002300240012A5010800113Q00202Q00080008001200122Q000900043Q00122Q000A00146Q0008000A000200102Q00070010000800122Q000800163Q00202Q00080008001200122Q000900043Q00122Q000A00253Q001227000B00143Q001227000C00134Q006A0108000C000200102Q00070015000800122Q000800163Q00202Q00080008002600122Q000900273Q00122Q000A00286Q0008000A000200102Q00070018000800102Q0007001A00024Q0005000700022Q007F000600054Q0055010700053Q00122Q000800296Q00060008000100202Q00060005002A00202Q00060006002B00069001083Q000100012Q00103Q00014Q00B80006000800012Q00E600063Q00020010180106002C000200069001070001000100052Q00103Q00034Q00103Q00044Q007F3Q00064Q00103Q00054Q007F3Q00033Q0010180106002D00072Q0068010600024Q00D33Q00013Q00023Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q007F7Q002074014Q0001000675012Q000900013Q0004A23Q000900010012F33Q00023Q002074014Q00032Q007F00015Q0020742Q01000100012Q000E3Q000200012Q00D33Q00017Q00113Q0003043Q0054657874034Q002Q033Q00737562027Q00C003023Q002D5303103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66030A3Q0054657874436F6C6F723303043Q00536F6C6403053Q005368696E7903063Q00436F6C6F723303073Q0066726F6D524742026Q004440026Q003E40028Q002Q033Q0042757903063Q00412Q63656E74043A4Q007F00045Q0010180104000100012Q007F000400013Q00060900050006000100020004A23Q00060001001227000500023Q0010180104000100052Q005C000400026Q000500016Q00040002000200202Q00040004000300122Q000600046Q00040006000200262Q00040010000100050004A23Q001000012Q006101046Q00AA000400013Q0006E90003001E000100010004A23Q001E00012Q007F000500034Q001A010600043Q00202Q00060006000700102Q0005000600064Q000500036Q000600043Q00202Q00060006000100102Q0005000800064Q000500033Q00302Q00050001000900044Q003900010006750104002F00013Q0004A23Q002F00012Q007F000500034Q00D7000600043Q00202Q00060006000A00102Q0005000600064Q000500033Q00122Q0006000B3Q00202Q00060006000C00122Q0007000D3Q00122Q0008000E3Q00122Q0009000F6Q0006000900020010180105000800062Q007F000500033Q0030460005000100100004A23Q003900012Q007F000500034Q007F000600043Q0020740106000600110010180105000600062Q007F000500034Q007F000600043Q0020740106000600010010180105000800062Q007F000500033Q0030460005000100102Q00D33Q00017Q00CD3Q00030C3Q0043726561746557696E646F7703053Q005469746C6503093Q0043726561746554616203043Q004E616D6503043Q004D61696E03043Q0049636F6E03123Q0033642D637562652D612Q726F772D6C656674030C3Q00437265617465546F2Q676C6503093Q004175746F204C69667403063Q00637572736F7203073Q0044656661756C74010003083Q0043612Q6C6261636B030B3Q005365744175746F4C696674030A3Q004175746F2050756E636803113Q0068616E642D68616C662D652Q6C69707365030C3Q005365744175746F50756E6368030A3Q004175746F2053746F6D702Q033Q006C6567030C3Q005365744175746F53746F6D7003103Q004175746F2054652Q7269746F72696573030C3Q006C6F636174696F6E2D70696E03123Q005365744175746F54652Q7269746F72696573030C3Q00436F2Q6C65637461626C6573030D3Q0062752Q6C65742D666C79696E67030F3Q0057616C6B20746F2041697264726F70030E3Q0070617065722D616972706C616E6503103Q0053657457616C6B546F41697264726F7003103Q0057616C6B20746F207461726765742Q20030E3Q00706572736F6E2D77616C6B696E67030E3Q005365744175746F47656D57616C6B030C3Q00437265617465536C69646572030E3Q0047656D2077616C6B2073702Q65642Q033Q004D696E026Q0034402Q033Q004D6178025Q00408E4003093Q00496E6372656D656E74026Q00F03F030F3Q0053657447656D57616C6B53702Q6564030F3Q004175746F2047656D732054772Q656E030F3Q0062752Q746572666C792D77696E6773030F3Q005365744175746F47656D54772Q656E03103Q0047656D732074772Q656E2073702Q6564026Q004940025Q00208C4003103Q0053657447656D54772Q656E53702Q656403043Q00426F2Q7303063Q0073776F72647303103Q004272696E6720412Q6C20626F2Q73657303163Q0074776F2D612Q726F77732D646F776E2D616E642D7570030C3Q00536574426F2Q734272696E67030C3Q0057616C6B20746F20426F2Q73030D3Q0053657457616C6B546F426F2Q73030A3Q00547020746F20626F2Q73030F3Q005365745470546F426F2Q734B692Q6C030D3Q0043726561746553656374696F6E030C3Q00426F2Q7320747261636B657203053Q0073776F726403093Q0049636F6E436F6C6F7203063Q00507572706C6503043Q007461736B03053Q00737061776E03043Q00452Q67732Q033Q00652Q6703173Q004175746F2068617463682073656C656374656420652Q6703173Q005365744175746F486174636853656C6563746564452Q6703103Q004175746F20686174636820706572203103123Q005365744175746F48617463685065724F6E6503103Q0053656C65637420452Q672028312D3529026Q00144003133Q0053657453656C6563746564452Q67496E646578030A3Q004375626520776F726C64030D3Q00637562652D7665727465786573030E3Q004175746F20486174636820452Q6703133Q005365744175746F486174636843756265452Q6703043Q0053686F70030D3Q0073686F2Q70696E672D6361727403093Q004175746F2053652Q6C03123Q00636972636C652D646F2Q6C61722D7369676E030B3Q005365744175746F53652Q6C03103Q004175746F20427579205765696768747303153Q0073686F2Q70696E672D6261736B65742D636865636B03113Q005365744175746F42757957656967687473030C3Q004175746F2042757920444E412Q033Q0070696E030D3Q005365744175746F427579444E41030F3Q004175746F2042757920426F6469657303103Q00706572736F6E2D63726F756368696E6703103Q005365744175746F427579426F6469657303063Q004D61726B6574030D3Q006275696C64696E672D62616E6B030C3Q004175746F2042757920412Q6C031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F7269657303083Q004D65726368616E74030F3Q004D65726368616E74206F2Q6665727303063Q0048692Q64656E2Q0103133Q004D65726368616E742050657420536E6970657203113Q005365744D65726368616E74536E69706572030D3Q0043726561746554657874626F7803093Q004D6178207072696365030B3Q00506C616365686F6C64657203153Q00652E672E20354D2028626C616E6B203D20616E792903133Q005365744D65726368616E744D61785072696365030B3Q004372656174654C6162656C03063Q0054617267657403053Q0056616C756503043Q004E6F6E6503063Q00537461747573034Q00030A3Q005065747320666F756E64030E3Q004F70656E2050657420496E646578025Q00408F40030A3Q0053656172636820706574030B3Q00706574206E616D653Q2E030C3Q0043726561746542752Q746F6E03103Q005265667265736820706574206C69737403183Q00726F746174652D636F756E7465722D636C6F636B7769736503113Q00436C65617220612Q6C207461726765747303013Q007803273Q005069636B20796F75722070657473202874617020746F2073656C6563742F756E73656C6563742903093Q00526967687449636F6E03103Q00612Q726F772D6C617267652D646F776E03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203123Q00437265617465436F726E657242752Q746F6E03113Q00612Q726F772D6C617267652D726967687403043Q005465787403063Q004F2Q6665727303103Q004261636B20746F204D65726368616E7403103Q00612Q726F772D6C617267652D6C65667403063Q0069706169727303053Q00546965723103053Q00546965723203053Q005469657233030C3Q00437265617465427579526F7703043Q004375626503043Q006375626503153Q0055736520617420796F7572206F776E207269736B2103123Q00637562652D7175657374696F6E2D6D61726B030A3Q004175746F204375626573030C3Q005365744175746F4375626573030B3Q0054772Q656E2053702Q6564026Q004440030C3Q005365744375626553702Q6564030D3Q0057616C6B20746F204375626573030E3Q0053657457616C6B546F437562657303063Q0051756573747303043Q007374617203113Q0053656C656374656449636F6E436F6C6F7203063Q0059652Q6C6F77030C3Q005072656D69756D204F6E6C7903143Q00612Q726F772D746869636B2D746F2D7269676874030A3Q004175746F205175657374030C3Q005365744175746F517565737403023Q00756903063Q007374617475732Q033Q004F2Q6603073Q0063752Q72656E7403053Q00446F696E6703013Q002D03043Q004D69736303103Q0063726F2Q7368616972732D736C61736803143Q0044697361626C652033442052656E646572696E6703103Q00636F6D70757465722D6D6F6E69746F72030C3Q0053657444697361626C653344030B3Q004175746F2052656A6F696E03133Q00612Q726F772D6375726C2D746F2D7269676874030D3Q005365744175746F52656A6F696E030E3Q004175746F2052656A6F696E205632030F3Q005365744175746F52656A6F696E563203103Q00437573746F6D2057616C6B53702Q656403063Q00706572736F6E03123Q0053657457616C6B53702Q6564546F2Q676C65030F3Q0057616C6B53702Q65642056616C7565025Q0070974003113Q0053657457616C6B53702Q656456616C756503103Q00437573746F6D204A756D70506F776572030E3Q00706572736F6E2D66612Q6C696E6703123Q005365744A756D70506F776572546F2Q676C65030F3Q004A756D70506F7765722056616C7565025Q00407F4003113Q005365744A756D70506F77657256616C7565030D3Q00496E66696E697465204A756D70030F3Q00536574496E66696E6974654A756D7003053Q005374617473030A3Q00636861696E2D6C696E6B2Q033Q00465053031A3Q0063686576726F6E2D6C617267652D636F6E64656E7365642D7570030A3Q004C6967687447722Q656E03013Q0030030C3Q004E6574776F726B2050696E6703103Q00776966692D7369676E616C2D6661697203043Q0030206D73030C3Q00456C61707365642054696D6503053Q00636C6F636B03083Q002Q303A2Q303A2Q30030A3Q0047656D73202F204D696E03193Q00636C6F636B2D7370696E2D726576657273652D646173686564030B3Q0047656D73204561726E656403073Q006469616D6F6E64030B3Q00526573657420537461747300B3023Q007F7Q00201B014Q00012Q00E600023Q00012Q007F000300013Q0010180102000200032Q006B012Q0002000200202Q00013Q00034Q00033Q000200302Q00030004000500302Q0003000600074Q00010003000200202Q0002000100084Q00043Q000400302Q00040004000900302Q00040006000A0030460004000B000C2Q007F000500023Q00207401050005000E0010C90004000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004000F00302Q0004000600100030460004000B000C2Q007F000500023Q0020740105000500110010C90004000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001200302Q0004000600130030460004000B000C2Q007F000500023Q0020740105000500140010C90004000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001500302Q0004000600160030460004000B000C2Q007F000500023Q0020740105000500170010C90004000D00054Q00020004000100202Q00023Q00034Q00043Q000200302Q00040004001800302Q0004000600192Q00420102000400020020750003000200084Q00053Q000400302Q00050004001A00302Q00050006001B00302Q0005000B000C4Q000600023Q00202Q00060006001C0010C90005000D00064Q00030005000100202Q0003000200084Q00053Q000400302Q00050004001D00302Q00050006001E0030460005000B000C2Q007F000600023Q00207401060006001F0010C90005000D00064Q00030005000100202Q0003000200204Q00053Q000600302Q00050004002100302Q00050022002300301901050024002500302Q0005000B002300302Q0005002600274Q000600023Q00202Q0006000600280010C90005000D00064Q00030005000100202Q0003000200084Q00053Q000400302Q00050004002900302Q00050006002A0030460005000B000C2Q007F000600023Q00207401060006002B0010C90005000D00064Q00030005000100202Q0003000200204Q00053Q000600302Q00050004002C00302Q00050022002D00301901050024002E00302Q0005000B002D00302Q0005002600274Q000600023Q00202Q00060006002F0010C90005000D00064Q00030005000100202Q00033Q00034Q00053Q000200302Q00050004003000302Q0005000600312Q00420103000500020020750004000300084Q00063Q000400302Q00060004003200302Q00060006003300302Q0006000B000C4Q000700023Q00202Q0007000700340010C90006000D00074Q00040006000100202Q0004000300084Q00063Q000400302Q00060004003500302Q0006000600330030460006000B000C2Q007F000700023Q0020740107000700360010C90006000D00074Q00040006000100202Q0004000300084Q00063Q000400302Q00060004003700302Q0006000600330030460006000B000C2Q007F000700023Q0020740107000700380010C90006000D00074Q00040006000100202Q0004000300394Q00063Q000300302Q00060004003A00302Q00060006003B2Q007F000700033Q00207401070007003D0010180106003C00072Q00B80004000600012Q00E600045Q0012F30005003E3Q00207401050005003F00069001063Q000100032Q007F3Q00044Q00103Q00044Q00103Q00034Q004901050002000100202Q00053Q00034Q00073Q000200302Q00070004004000302Q0007000600414Q00050007000200202Q0006000500084Q00083Q000400302Q00080004004200302Q00080006004100302Q0008000B000C4Q000900023Q00202Q00090009004300102Q0008000D00094Q00060008000100202Q0006000500084Q00083Q000400302Q00080004004400302Q00080006004100302Q0008000B000C4Q000900023Q00202Q00090009004500102Q0008000D00094Q00060008000100202Q0006000500204Q00083Q000600302Q00080004004600302Q00080022002700302Q00080024004700302Q0008000B002700302Q0008002600274Q000900023Q00202Q00090009004800102Q0008000D00094Q00060008000100202Q0006000500394Q00083Q000200302Q00080004004900302Q00080006004A4Q00060008000100202Q0006000500084Q00083Q000400302Q00080004004B00302Q00080006004100302Q0008000B000C4Q000900023Q00202Q00090009004C00102Q0008000D00094Q00060008000100202Q00063Q00034Q00083Q000200302Q00080004004D00302Q00080006004E4Q00060008000200202Q0007000600084Q00093Q000400302Q00090004004F00302Q00090006005000302Q0009000B000C4Q000A00023Q00202Q000A000A005100102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005200302Q00090006005300302Q0009000B000C4Q000A00023Q00202Q000A000A005400102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005500302Q00090006005600302Q0009000B000C4Q000A00023Q00202Q000A000A005700102Q0009000D000A2Q002E00070009000100202Q0007000600084Q00093Q000400302Q00090004005800302Q00090006005900302Q0009000B000C4Q000A00023Q00202Q000A000A005A0010C90009000D000A4Q00070009000100202Q00073Q00034Q00093Q000200302Q00090004005B00302Q00090006005C2Q00420107000900020020750008000700084Q000A3Q000400302Q000A0004005D00302Q000A0006005300302Q000A000B000C4Q000B00023Q00202Q000B000B005E0010C9000A000D000B4Q0008000A000100202Q0008000700084Q000A3Q000400302Q000A0004004F00302Q000A00060050003046000A000B000C2Q007F000B00023Q002074010B000B005F0010C9000A000D000B4Q0008000A000100202Q0008000700084Q000A3Q000400302Q000A0004001500302Q000A00060016003046000A000B000C2Q007F000B00023Q002074010B000B00600010C9000A000D000B4Q0008000A000100202Q00083Q00034Q000A3Q000200302Q000A0004006100302Q000A000600532Q006B0108000A000200202Q00093Q00034Q000B3Q000200302Q000B0004006200302Q000B006300644Q0009000B000200202Q000A000800084Q000C3Q000400302Q000C0004006500302Q000C00060053003046000C000B000C2Q007F000D00023Q002074010D000D00660010C9000C000D000D4Q000A000C000100202Q000A000800674Q000C3Q000300302Q000C0004006800302Q000C0069006A2Q007F000D00023Q002074010D000D006B0010C9000C000D000D4Q000A000C000100202Q000A0008006C4Q000C3Q000200302Q000C0004006D00302Q000C006E006F2Q006B010A000C000200202Q000B0008006C4Q000D3Q000200302Q000D0004007000302Q000D006E00714Q000B000D000200202Q000C0008006C4Q000E3Q000300302Q000E0004007200302Q000E00060041003046000E006E00732Q0042010C000E00020012F3000D003E3Q002074010D000D003F000690010E0001000100032Q007F3Q00044Q007F3Q00054Q00103Q000C4Q007D010D000200014Q000D8Q000E5Q00122Q000F00746Q001000113Q00069001120002000100052Q00103Q000A4Q007F3Q00054Q00103Q000D4Q007F3Q00034Q007F3Q00063Q00069001130003000100032Q00103Q00104Q00103Q000D4Q00103Q000E3Q00069001140004000100032Q00103Q000E4Q00103Q00114Q00103Q00133Q00201B0115000800672Q00E600173Q000300304600170004007500304600170069007600069001180005000100022Q00103Q00144Q00103Q00133Q0010180117000D00182Q00420115001700022Q0010001000153Q00069001150006000100082Q00103Q000D4Q007F3Q00054Q00103Q000B4Q00103Q00084Q007F3Q00024Q00103Q00124Q00103Q000F4Q00103Q00133Q00201B0116000800772Q00E600183Q00030030460018000400780030460018000600790010C90018000D00154Q00160018000100202Q0016000800774Q00183Q000300302Q00180004007A00302Q00180006007B00069001190007000100042Q007F3Q00024Q00103Q000B4Q007F3Q00054Q00103Q00123Q0010C90018000D00194Q00160018000100202Q0016000800774Q00183Q000500302Q00180004007C00302Q0018000600412Q007F001900033Q00207401190019003D0010180118003C00190030460018007D007E00069001190008000100022Q00103Q00144Q00103Q000E3Q0010180118000D00192Q00420116001800022Q0010001100163Q00207401160011007F0020E50017000F00270010180116008000172Q0010001600144Q00AA00176Q000E0016000200012Q0010001600154Q006900160001000100201B0116000800812Q00E600183Q000300304600180006008200304600180083008400069001190009000100012Q00103Q00093Q0010C90018000D00194Q00160018000100202Q0016000900774Q00183Q000300302Q00180004008500302Q0018000600860006900119000A000100012Q00103Q00083Q0010C90018000D00194Q00160018000100202Q00160009006C4Q00183Q000200302Q00180004007000302Q0018006E00712Q004201160018000200201B0117000900392Q00E600193Q00030030460019000400620030460019000600532Q007F001A00033Q002074011A001A003D0010180119003C001A2Q00B80017001900012Q00E600176Q00E600185Q001285011900876Q001A00033Q00122Q001B00883Q00122Q001C00893Q00122Q001D008A6Q001A000300012Q006E01190002001B0004A23Q00BE2Q0100201B011E0009008B2Q00E600203Q00010006900121000B000100042Q00103Q00184Q00103Q001D4Q007F3Q00054Q00103Q00163Q0010180120000D00212Q0042011E002000022Q008E0017001D001E2Q006D011C5Q0006FE001900B32Q0100020004A23Q00B32Q010012F30019003E3Q00207401190019003F000690011A000C000100042Q007F3Q00044Q007F3Q00054Q00103Q00174Q00103Q00184Q00FD00190002000100202Q00193Q00034Q001B3Q000200302Q001B0004008C00302Q001B0006008D4Q0019001B000200202Q001A001900394Q001C3Q000300302Q001C0004008E00302Q001C0006008F4Q001D00033Q00202Q001D001D003D00102Q001C003C001D4Q001A001C000100202Q001A001900084Q001C3Q000400302Q001C0004009000302Q001C0006000700302Q001C000B000C4Q001D00023Q00202Q001D001D009100102Q001C000D001D4Q001A001C000100202Q001A001900204Q001C3Q000600302Q001C0004009200302Q001C0022002300302Q001C0024009300302Q001C000B009300302Q001C002600274Q001D00023Q00202Q001D001D009400102Q001C000D001D4Q001A001C000100202Q001A001900084Q001C3Q000400302Q001C0004009500302Q001C0006001E00302Q001C000B000C4Q001D00023Q00202Q001D001D009600102Q001C000D001D4Q001A001C000100202Q001A3Q00034Q001C3Q000300302Q001C0004009700302Q001C000600984Q001D00033Q00202Q001D001D009A00102Q001C0099001D4Q001A001C000200202Q001B001A00394Q001D3Q000300302Q001D0004009B00302Q001D0006009C4Q001E00033Q00202Q001E001E009A00102Q001D003C001E4Q001B001D000100202Q001B001A00084Q001D3Q000500302Q001D0004009D00302Q001D000600984Q001E00033Q00202Q001E001E009A00102Q001D003C001E00302Q001D000B000C4Q001E00023Q00202Q001E001E009E00102Q001D000D001E4Q001B001D00014Q001B00073Q00202Q001B001B009F00202Q001C001A006C4Q001E3Q000200302Q001E0004007000302Q001E006E00A14Q001C001E000200102Q001B00A0001C4Q001B00073Q002074011B001B009F002033001C001A006C4Q001E3Q000200302Q001E000400A300302Q001E006E00A44Q001C001E000200102Q001B00A2001C00202Q001B3Q00034Q001D3Q000200302Q001D000400A500302Q001D000600A64Q001B001D000200202Q001C001B00084Q001E3Q000400302Q001E000400A700302Q001E000600A800302Q001E000B000C4Q001F00023Q00202Q001F001F00A900102Q001E000D001F4Q001C001E000100202Q001C001B00084Q001E3Q000400302Q001E000400AA00302Q001E000600AB00302Q001E000B000C4Q001F00023Q00202Q001F001F00AC00102Q001E000D001F4Q001C001E000100202Q001C001B00084Q001E3Q000400302Q001E000400AD00302Q001E000600AB00302Q001E000B000C4Q001F00023Q00202Q001F001F00AE00102Q001E000D001F4Q001C001E000100202Q001C001B00084Q001E3Q000400302Q001E000400AF00302Q001E000600B000302Q001E000B000C4Q001F00023Q00202Q001F001F00B100102Q001E000D001F4Q001C001E000100202Q001C001B00204Q001E3Q000600302Q001E000400B200302Q001E0022002300302Q001E002400B300302Q001E000B002300302Q001E002600274Q001F00023Q00202Q001F001F00B400102Q001E000D001F4Q001C001E000100202Q001C001B00084Q001E3Q000400302Q001E000400B500302Q001E000600B600302Q001E000B000C4Q001F00023Q00202Q001F001F00B700102Q001E000D001F4Q001C001E000100202Q001C001B00204Q001E3Q000600302Q001E000400B800302Q001E0022002D00302Q001E002400B900302Q001E000B002D00302Q001E002600274Q001F00023Q00202Q001F001F00BA00102Q001E000D001F4Q001C001E000100202Q001C001B00084Q001E3Q0003003046001E000400BB00309C011E000B000C4Q001F00023Q00202Q001F001F00BC00102Q001E000D001F4Q001C001E000100202Q001C3Q00034Q001E3Q000200302Q001E000400BD00302Q001E000600BE4Q001C001E000200202Q001D001C006C4Q001F3Q000400302Q001F000400BF00302Q001F000600C04Q002000033Q00202Q0020002000C100102Q001F003C002000302Q001F006E00C24Q001D001F000200202Q001E001C006C4Q00203Q000300302Q0020000400C300302Q0020000600C400302Q0020006E00C54Q001E0020000200202Q001F001C006C4Q00213Q000300302Q0021000400C600302Q0021000600C700302Q0021006E00C84Q001F0021000200202Q0020001C006C4Q00223Q000300302Q0022000400C900302Q0022000600CA00302Q0022006E00C24Q00200022000200202Q0021001C006C4Q00233Q000300302Q0023000400CB00302Q0023000600CC00302Q0023006E00C24Q00210023000200202Q0022001C00774Q00243Q000300302Q0024000400CD00302Q0024000600790006900125000D000100062Q007F3Q00084Q007F3Q00094Q007F3Q000A4Q00103Q001F4Q00103Q00214Q00103Q00203Q0010180124000D00252Q00B80022002400010012F30022003E3Q00207401220022003F0006900123000E0001000D2Q007F3Q00044Q00103Q001D4Q007F3Q000B4Q007F3Q000C4Q00103Q001E4Q007F3Q00084Q00103Q001F4Q007F3Q000D4Q007F3Q000A4Q007F3Q00094Q00103Q00214Q007F3Q000E4Q00103Q00204Q000E0022000200012Q00D33Q00013Q000F3Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03083Q00486974734C65667403073Q004D61784869747303043Q004E616D652Q0103053Q0056616C7565030B3Q004372656174654C6162656C03083Q00746F737472696E672Q033Q00202F202Q033Q0053657403053Q00706169727303083Q00496E7374616E636503073Q0044657374726F792Q00614Q007F7Q002074014Q0001000675012Q006000013Q0004A23Q006000010012F33Q00023Q00204C014Q000300122Q000100048Q0002000100124Q00053Q00206Q000600122Q000200078Q000200024Q00015Q00064Q004E00013Q0004A23Q004E00010012F3000200083Q00201B01033Q00092Q00A6000300044Q008C01023Q00040004A23Q004C000100201B01070006000A0012270009000B4Q00420107000900020006750107004C00013Q0004A23Q004C000100201B0107000600060012760109000C6Q00070009000200202Q00080006000600122Q000A000D6Q0008000A000200062Q0007004C00013Q0004A23Q004C00010006750108004C00013Q0004A23Q004C000100207401090006000E0020252Q010009000F00202Q00090007001000202Q000A000800104Q000B00013Q00202Q000C0006000E4Q000B000B000C00062Q000B003F000100010004A23Q003F00012Q007F000B00013Q00206C000C0006000E4Q000D00023Q00202Q000D000D00114Q000F3Q000200202Q00100006000E00102Q000F000E001000122Q001000126Q001100096Q00100002000200122Q001100133Q0012F3001200124Q003B0113000A6Q0012000200024Q00100010001200102Q000F001000104Q000D000F00024Q000B000C000D00044Q004C00012Q007F000B00013Q002074010C0006000E2Q0030000B000B000C00201B010B000B00140012F3000D00124Q0010000E00094Q00FF000D00020002001227000E00133Q0012F3000F00124Q00100010000A4Q00FF000F000200022Q00A0010D000D000F2Q00B8000B000D00010006FE00020014000100020004A23Q001400010012F3000200154Q007F000300014Q006E0102000200040004A23Q005D00012Q00300007000100050006E90007005D000100010004A23Q005D00010020740107000600160006750107005B00013Q0004A23Q005B000100207401070006001600201B0107000700172Q000E0007000200012Q007F000700013Q00206C0107000500180006FE00020052000100020004A23Q005200010004A25Q00012Q00D33Q00017Q00083Q0003053Q00416C697665030D3Q00676574496E646578436F756E742Q033Q005365742Q033Q00202F2003143Q004F70656E2050657420496E64657820666972737403043Q007461736B03043Q0077616974027Q004000194Q007F7Q002074014Q0001000675012Q001800013Q0004A23Q001800012Q007F3Q00013Q00206F5Q00026Q000100014Q000200023Q00202Q00020002000300064Q001100013Q0004A23Q001100012Q001000045Q001227000500044Q0010000600014Q00A00104000400060006E900040012000100010004A23Q00120001001227000400054Q00B80002000400010012CA000200063Q00202Q00020002000700122Q000300086Q00020002000100046Q00012Q00D33Q00017Q00123Q002Q033Q00536574030A3Q007461726765745465787403063Q00697061697273030A3Q006973546172676574656403043Q006E616D652Q033Q00726F7703153Q0046696E6446697273744368696C644F66436C612Q7303083Q0055495374726F6B6503053Q00436F6C6F7203063Q00412Q63656E7403063Q005374726F6B65030C3Q005472616E73706172656E6379029A5Q99B93F03163Q0046696E6446697273744368696C64576869636849734103093Q00546578744C6162656C030A3Q0054657874436F6C6F7233030A3Q004C6967687447722Q656E03043Q005465787400374Q007F7Q00201B014Q00012Q007F000200013Q0020740102000200022Q0024000200014Q00615Q00010012F33Q00034Q007F000100024Q006E012Q000200020004A23Q003400012Q007F000500013Q0020740105000500040020740106000400052Q00FF00050002000200208A01060004000600202Q00060006000700122Q000800086Q00060008000200062Q0006002400013Q0004A23Q002400010006750105001A00013Q0004A23Q001A00012Q007F000700033Q00207401070007000A0006E90007001C000100010004A23Q001C00012Q007F000700033Q00207401070007000B0010180106000900070006750105002200013Q0004A23Q002200010012270007000D3Q0006E900070023000100010004A23Q002300012Q007F000700043Q0010180106000C000700207401070004000600202E01070007000E00122Q0009000F6Q000A00016Q0007000A000200062Q0007003400013Q0004A23Q003400010006750105003100013Q0004A23Q003100012Q007F000800033Q0020740108000800110006E900080033000100010004A23Q003300012Q007F000800033Q0020740108000800120010180107001000080006FE3Q000A000100020004A23Q000A00012Q00D33Q00017Q000B3Q0003063Q00737472696E6703053Q006C6F7765722Q033Q00476574034Q0003063Q006970616972732Q033Q00726F7703073Q0056697369626C6503043Q0066696E6403043Q006E616D65026Q00F03F2Q00283Q0012F33Q00013Q002074014Q00022Q007F00015Q0006752Q01000A00013Q0004A23Q000A00012Q007F00015Q0020742Q01000100032Q00B70001000100020006E90001000B000100010004A23Q000B0001001227000100044Q00FF3Q000200020012F3000100054Q007F000200014Q006E2Q01000200030004A23Q002500010020740106000500062Q007F000700023Q0006750107002400013Q0004A23Q0024000100260C3Q0023000100040004A23Q002300010012F3000700013Q0020DB00070007000800122Q000800013Q00202Q00080008000200202Q0009000500094Q0008000200024Q00095Q00122Q000A000A6Q000B00016Q0007000B000200262Q000700230001000B0004A23Q002300012Q006101076Q00AA000700013Q0010180106000700070006FE00010010000100020004A23Q001000012Q00D33Q00017Q00033Q00030C3Q00536574526967687449636F6E030E3Q00612Q726F772D6C617267652D757003103Q00612Q726F772D6C617267652D646F776E01104Q00038Q007F000100013Q0006752Q01000D00013Q0004A23Q000D00012Q007F000100013Q00201B2Q0100010001000675012Q000B00013Q0004A23Q000B0001001227000300023Q0006E90003000C000100010004A23Q000C0001001227000300034Q00B80001000300012Q007F000100024Q00690001000100012Q00D33Q00017Q00013Q00034Q0001093Q00260C3Q0006000100010004A23Q000600012Q007F00016Q00AA000200014Q000E0001000200010004A23Q000800012Q007F000100014Q00690001000100012Q00D33Q00017Q00133Q0003063Q006970616972732Q033Q00726F7703073Q0044657374726F79030B3Q006765745065744E616D65732Q033Q0053657403153Q004F70656E2050657420496E6465782066697273742103073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564030C3Q0043726561746542752Q746F6E03043Q004E616D6503083Q0043612Q6C6261636B03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203053Q007461626C6503063Q00696E7365727403043Q006E616D6500483Q0012F33Q00014Q007F00016Q006E012Q000200020004A23Q000A00010020740105000400020006750105000A00013Q0004A23Q000A000100207401050004000200201B0105000500032Q000E0005000200010006FE3Q0004000100020004A23Q000400012Q00E68Q0004019Q002Q00013Q00206Q00046Q0001000200064Q0018000100010004A23Q001800012Q007F000100023Q00201B2Q0100010005001227000300064Q00B80001000300012Q00D33Q00014Q007F000100023Q0020BF00010001000500122Q000300076Q00030001000200202Q0003000300084Q000300033Q00122Q000400096Q000500013Q00202Q00050005000A00122Q0006000B6Q0003000300064Q00010003000100122Q000100016Q00028Q00010002000300044Q004100012Q007F000600033Q00201B01060006000C2Q00E600083Q00020010180108000D000500069001093Q000100052Q007F3Q00044Q00103Q00054Q007F3Q00024Q007F3Q00014Q007F3Q00053Q00101F0008000E00094Q00060008000200202Q00070006000F4Q000800066Q00080008000400102Q00070010000800122Q000700113Q00202Q0007000700124Q00088Q00093Q0002001018010900130005002074010A0006000F00101801090002000A2Q00B80007000900012Q006D01045Q0006FE00010028000100020004A23Q002800012Q007F000100074Q00690001000100012Q007F000100054Q00690001000100012Q00D33Q00013Q00013Q00073Q0003143Q00546F2Q676C654D65726368616E745461726765742Q033Q0053657403073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564001A4Q0014016Q00206Q00014Q000100018Q0002000100064Q001300013Q0004A23Q001300012Q007F000200023Q00201B0102000200020012F3000400034Q00B70004000100020020740104000400042Q0021010400043Q001227000500054Q007F000600033Q002074010600060006001227000700074Q00A00104000400072Q00B80002000400010004A23Q001700012Q007F000200023Q00201B0102000200022Q0010000400014Q00B80002000400012Q007F000200044Q00690002000100012Q00D33Q00017Q00053Q0003143Q00436C6561724D65726368616E74546172676574732Q033Q0053657403023Q00302F2Q033Q006D617803093Q00207461726765746564000E4Q00577Q00206Q00016Q000100016Q00013Q00206Q000200122Q000200036Q000300023Q00202Q00030003000400122Q000400056Q0002000200042Q00B83Q000200012Q007F3Q00034Q00693Q000100012Q00D33Q00019Q003Q00054Q00389Q00000100016Q000100018Q000200016Q00017Q00013Q0003073Q005F73656C65637400044Q007F7Q002074014Q00012Q00693Q000100012Q00D33Q00017Q00013Q0003073Q005F73656C65637400044Q007F7Q002074014Q00012Q00693Q000100012Q00D33Q00017Q00093Q0003093Q0067657452656D6F746503093Q00617661696C61626C652Q033Q00536574030D3Q004E6F7420617661696C61626C6503103Q0052656D6F7465206E6F7420666F756E6403053Q007063612Q6C03073Q00426F756768742003043Q006E616D65030A3Q00427579206661696C656400284Q007F8Q007F000100014Q00305Q00012Q007F000100023Q0020742Q01000100012Q00B7000100010002000675012Q000B00013Q0004A23Q000B000100207401023Q00020006E900020010000100010004A23Q001000012Q007F000200033Q00201B010200020003001227000400044Q00B80002000400010004A23Q002700010006E900010017000100010004A23Q001700012Q007F000200033Q00201B010200020003001227000400054Q00B80002000400010004A23Q002700010012F3000200063Q00069001033Q000100022Q00103Q00014Q007F3Q00014Q00FF0002000200022Q007F000300033Q00201B0103000300030006750102002500013Q0004A23Q00250001001227000500073Q00207401063Q00082Q00A00105000500060006E900050026000100010004A23Q00260001001227000500094Q00B80003000500012Q00D33Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q00517Q00206Q00014Q000200018Q000200016Q00017Q000C3Q0003053Q00416C69766503093Q006765744F2Q6665727303053Q0070616972732Q033Q0053657403043Q006E616D6503053Q00707269636503093Q00617661696C61626C6503203Q004F70656E20746865204D65726368616E7420746F206C6F6164206F2Q66657273034Q0003043Q007461736B03043Q0077616974026Q00E03F00254Q007F7Q002074014Q0001000675012Q002400013Q0004A23Q002400012Q007F3Q00013Q002097014Q00026Q0001000200122Q000100036Q000200026Q00010002000300044Q001D00010006A20106000E00013Q0004A23Q000E00012Q003000063Q00042Q007F000700034Q008E0007000400060006750106001800013Q0004A23Q0018000100201B010700050004002074010900060005002074010A00060006002074010B000600072Q00B80007000B00010004A23Q001D000100201B010700050004001227000900083Q001227000A00094Q00AA000B6Q00B80007000B00010006FE0001000B000100020004A23Q000B00010012F30001000A3Q0020742Q010001000B0012270002000C4Q000E0001000200010004A25Q00012Q00D33Q00017Q00063Q0003023Q006F7303043Q0074696D65028Q002Q033Q0053657403083Q002Q303A2Q303A2Q3003013Q003000153Q0012263Q00013Q00206Q00026Q000100029Q0000124Q00038Q00019Q008Q00028Q00033Q00206Q0004001227000200054Q00B83Q000200012Q007F3Q00043Q00201B014Q0004001227000200064Q00B83Q000200012Q007F3Q00053Q00201B014Q0004001227000200064Q00B83Q000200012Q00D33Q00017Q00153Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00F03F2Q033Q0053657403083Q00746F737472696E67028Q0003053Q007063612Q6C2Q033Q00206D7303023Q006F7303043Q0074696D6503043Q006D61746803053Q00666C2Q6F72025Q0020AC40026Q004E4003063Q00737472696E6703063Q00666F726D6174030E3Q00253032643A253032643A2530326403053Q0056616C75650003013Q0030005E4Q007F7Q002074014Q0001000675012Q005D00013Q0004A23Q005D00010012F33Q00023Q0020F75Q000300122Q000100048Q000200016Q00013Q00206Q000500122Q000200066Q000300026Q000200039Q00000100124Q00073Q00122Q000100083Q00069001023Q000100022Q00108Q007F3Q00034Q00AE0001000200014Q000100043Q00202Q00010001000500122Q000300066Q00048Q00030002000200122Q000400096Q0003000300044Q00010003000100122Q0001000A3Q0020742Q010001000B2Q00B70001000100022Q007F000200054Q00B90001000100020012F30002000C3Q00207401020002000D00206E00030001000E2Q00FF0002000200020012F30003000C3Q00207401030003000D00209F01040001000E00206E00040004000F2Q00FF00030002000200209F01040001000F2Q007F000500063Q00201B0105000500050012F3000700103Q002074010700070011001227000800124Q0010000900024Q0010000A00034Q0010000B00044Q007C0107000B4Q006100053Q00012Q007F000500074Q00B70005000100020006750105005B00013Q0004A23Q005B00010020740106000500132Q007F000700083Q00260C00070044000100140004A23Q004400012Q007F000700084Q00B9000700060007000EA700070044000100070004A23Q004400012Q007F000800094Q00520108000800072Q0003000800094Q0003000600084Q003E0107000A3Q00202Q0007000700054Q0009000B6Q000A00096Q0009000A6Q00073Q0001000E2Q00070057000100010004A23Q005700012Q007F000700094Q003400070007000100202Q00070007000F4Q0008000C3Q00202Q0008000800054Q000A000B6Q000B00076Q000A000B6Q00083Q000100044Q005B00012Q007F0007000C3Q00201B010700070005001227000900154Q00B80007000900012Q006D016Q0004A25Q00012Q00D33Q00013Q00013Q00043Q0003043Q006D61746803053Q00666C2Q6F72030E3Q004765744E6574776F726B50696E67025Q00408F4000093Q001223012Q00013Q00206Q00024Q000100013Q00202Q0001000100034Q00010002000200202Q0001000100046Q000200029Q006Q00017Q00", GetFEnv(), ...);
