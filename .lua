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
											elseif (Enum == 1) then
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
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]] = Env[Inst[3]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											else
												Stk[Inst[2]] = Upvalues[Inst[3]];
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
												if (Stk[Inst[2]] == Stk[Inst[4]]) then
													VIP = VIP + 1;
												else
													VIP = Inst[3];
												end
											end
										elseif (Enum <= 3) then
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
										elseif (Enum > 4) then
											do
												return Stk[Inst[2]]();
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
										end
									elseif (Enum <= 8) then
										if (Enum <= 6) then
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
										elseif (Enum > 7) then
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
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Env[Inst[3]];
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
											Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Top)));
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
										end
									elseif (Enum <= 10) then
										if (Enum > 9) then
											local Results;
											local Edx;
											local Results, Limit;
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
											Stk[Inst[2]] = Upvalues[Inst[3]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Results, Limit = _R(Stk[A]());
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
									elseif (Enum == 11) then
										local Results;
										local Edx;
										local Results, Limit;
										local B;
										local A;
										Stk[Inst[2]] = Inst[3];
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
									end
								elseif (Enum <= 18) then
									if (Enum <= 15) then
										if (Enum <= 13) then
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
										elseif (Enum == 14) then
											local A;
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
											VIP = VIP + 1;
											Inst = Instr[VIP];
											do
												return;
											end
										end
									elseif (Enum <= 16) then
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
									elseif (Enum == 17) then
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
									else
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
									end
								elseif (Enum <= 21) then
									if (Enum <= 19) then
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
									elseif (Enum > 20) then
										local A;
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
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
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = {};
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
									end
								elseif (Enum <= 23) then
									if (Enum > 22) then
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
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
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
								elseif (Enum == 24) then
									local A;
									local K;
									local B;
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
									do
										return Stk[A], Stk[A + 1];
									end
								else
									local A = Inst[2];
									local Results = {Stk[A](Unpack(Stk, A + 1, Top))};
									local Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								end
							elseif (Enum <= 38) then
								if (Enum <= 31) then
									if (Enum <= 28) then
										if (Enum <= 26) then
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
										elseif (Enum == 27) then
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
											Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
										end
									elseif (Enum <= 29) then
										local A = Inst[2];
										do
											return Stk[A](Unpack(Stk, A + 1, Inst[3]));
										end
									elseif (Enum == 30) then
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
								elseif (Enum <= 34) then
									if (Enum <= 32) then
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
									elseif (Enum == 33) then
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
								elseif (Enum <= 36) then
									if (Enum > 35) then
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
								elseif (Enum == 37) then
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
								end
							elseif (Enum <= 45) then
								if (Enum <= 41) then
									if (Enum <= 39) then
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
									elseif (Enum == 40) then
										local A;
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
										Stk[A] = Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
									else
										local A;
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
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									end
								elseif (Enum <= 43) then
									if (Enum > 42) then
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
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
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
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									end
								elseif (Enum > 44) then
									local A = Inst[2];
									do
										return Unpack(Stk, A, A + Inst[3]);
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
								end
							elseif (Enum <= 48) then
								if (Enum <= 46) then
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
								elseif (Enum == 47) then
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
								else
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
								end
							elseif (Enum <= 50) then
								if (Enum > 49) then
									Stk[Inst[2]]();
								else
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
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
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
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								end
							elseif (Enum == 51) then
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
							end
						elseif (Enum <= 79) then
							if (Enum <= 65) then
								if (Enum <= 58) then
									if (Enum <= 55) then
										if (Enum <= 53) then
											local A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										elseif (Enum == 54) then
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
											B = Stk[Inst[4]];
											if B then
												VIP = VIP + 1;
											else
												Stk[Inst[2]] = B;
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
										end
									elseif (Enum <= 56) then
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
									elseif (Enum == 57) then
										local B = Stk[Inst[4]];
										if not B then
											VIP = VIP + 1;
										else
											Stk[Inst[2]] = B;
											VIP = Inst[3];
										end
									else
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Stk[Inst[2]] == Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 61) then
									if (Enum <= 59) then
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
									elseif (Enum == 60) then
										Stk[Inst[2]] = Inst[3] ~= 0;
									else
										local B;
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
								elseif (Enum <= 63) then
									if (Enum > 62) then
										local T;
										local Edx;
										local Results, Limit;
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
										T = Stk[A];
										for Idx = A + 1, Top do
											Insert(T, Stk[Idx]);
										end
									else
										local A = Inst[2];
										local B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Stk[Inst[4]]];
									end
								elseif (Enum == 64) then
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
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								else
									local A;
									Stk[Inst[2]] = Inst[3] ~= 0;
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
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 72) then
								if (Enum <= 68) then
									if (Enum <= 66) then
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
									elseif (Enum == 67) then
										VIP = Inst[3];
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
								elseif (Enum <= 70) then
									if (Enum == 69) then
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
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									end
								elseif (Enum == 71) then
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
							elseif (Enum <= 75) then
								if (Enum <= 73) then
									local A;
									Stk[Inst[2]] = Stk[Inst[3]];
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
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum > 74) then
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
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A]());
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
							elseif (Enum <= 77) then
								if (Enum > 76) then
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
								end
							elseif (Enum == 78) then
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
						elseif (Enum <= 92) then
							if (Enum <= 85) then
								if (Enum <= 82) then
									if (Enum <= 80) then
										if (Stk[Inst[2]] > Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 81) then
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
									else
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									end
								elseif (Enum <= 83) then
									local B;
									local A;
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
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								elseif (Enum == 84) then
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
									Stk[Inst[2]] = Upvalues[Inst[3]];
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
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								else
									local B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 88) then
								if (Enum <= 86) then
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
								elseif (Enum > 87) then
									local A = Inst[2];
									Stk[A](Stk[A + 1]);
								else
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
								end
							elseif (Enum <= 90) then
								if (Enum > 89) then
									local B;
									local A;
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
									Stk[Inst[2]] = Upvalues[Inst[3]];
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
								end
							elseif (Enum == 91) then
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
								Upvalues[Inst[3]] = Stk[Inst[2]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum <= 99) then
							if (Enum <= 95) then
								if (Enum <= 93) then
									Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
								elseif (Enum > 94) then
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
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
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
								end
							elseif (Enum <= 97) then
								if (Enum == 96) then
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
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum > 98) then
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
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
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
							end
						elseif (Enum <= 102) then
							if (Enum <= 100) then
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
							elseif (Enum == 101) then
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
						elseif (Enum <= 104) then
							if (Enum > 103) then
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
							else
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
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum > 105) then
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
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
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
						end
					elseif (Enum <= 160) then
						if (Enum <= 133) then
							if (Enum <= 119) then
								if (Enum <= 112) then
									if (Enum <= 109) then
										if (Enum <= 107) then
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
										elseif (Enum > 108) then
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
										end
									elseif (Enum <= 110) then
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
									elseif (Enum > 111) then
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
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								elseif (Enum <= 115) then
									if (Enum <= 113) then
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
									elseif (Enum > 114) then
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
										local A = Inst[2];
										do
											return Stk[A], Stk[A + 1];
										end
									end
								elseif (Enum <= 117) then
									if (Enum == 116) then
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
										Stk[Inst[2]] = Inst[3] ~= 0;
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
									else
										Stk[Inst[2]] = Env[Inst[3]];
									end
								elseif (Enum == 118) then
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
								else
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
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
								end
							elseif (Enum <= 126) then
								if (Enum <= 122) then
									if (Enum <= 120) then
										Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
									elseif (Enum > 121) then
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
										Stk[Inst[2]] = {};
									else
										local T;
										local Edx;
										local Results, Limit;
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
										T = Stk[A];
										for Idx = A + 1, Top do
											Insert(T, Stk[Idx]);
										end
									end
								elseif (Enum <= 124) then
									if (Enum == 123) then
										Stk[Inst[2]] = not Stk[Inst[3]];
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
								else
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
								end
							elseif (Enum <= 129) then
								if (Enum <= 127) then
									local A = Inst[2];
									local T = Stk[A];
									for Idx = A + 1, Top do
										Insert(T, Stk[Idx]);
									end
								elseif (Enum > 128) then
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
								else
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
								end
							elseif (Enum <= 131) then
								if (Enum > 130) then
									local A;
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
								else
									Stk[Inst[2]] = Wrap(Proto[Inst[3]], nil, Env);
								end
							elseif (Enum > 132) then
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
							else
								local A;
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
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum <= 146) then
							if (Enum <= 139) then
								if (Enum <= 136) then
									if (Enum <= 134) then
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
									elseif (Enum > 135) then
										local A;
										A = Inst[2];
										Stk[A](Stk[A + 1]);
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
									else
										local B;
										local A;
										Stk[Inst[2]] = Inst[3];
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
									end
								elseif (Enum <= 137) then
									local A;
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
								elseif (Enum == 138) then
									local A = Inst[2];
									local B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								elseif (Stk[Inst[2]] ~= Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum <= 142) then
								if (Enum <= 140) then
									local A;
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								elseif (Enum == 141) then
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
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum <= 144) then
								if (Enum == 143) then
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
							elseif (Enum == 145) then
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
							else
								local A;
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
							end
						elseif (Enum <= 153) then
							if (Enum <= 149) then
								if (Enum <= 147) then
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
								elseif (Enum == 148) then
									local A;
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
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
								end
							elseif (Enum <= 151) then
								if (Enum > 150) then
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
									Stk[Inst[2]] = Upvalues[Inst[3]];
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
									do
										return;
									end
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
							elseif (Enum == 152) then
								local A;
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
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
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
							end
						elseif (Enum <= 156) then
							if (Enum <= 154) then
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
							elseif (Enum == 155) then
								Stk[Inst[2]] = Upvalues[Inst[3]];
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
						elseif (Enum <= 158) then
							if (Enum > 157) then
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
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							end
						elseif (Enum == 159) then
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
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
						end
					elseif (Enum <= 187) then
						if (Enum <= 173) then
							if (Enum <= 166) then
								if (Enum <= 163) then
									if (Enum <= 161) then
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
									elseif (Enum == 162) then
										local A;
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
									else
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
											if (Mvm[1] == 213) then
												Indexes[Idx - 1] = {Stk,Mvm[3]};
											else
												Indexes[Idx - 1] = {Upvalues,Mvm[3]};
											end
											Lupvals[#Lupvals + 1] = Indexes;
										end
										Stk[Inst[2]] = Wrap(NewProto, NewUvals, Env);
									end
								elseif (Enum <= 164) then
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
								elseif (Enum == 165) then
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
							elseif (Enum <= 169) then
								if (Enum <= 167) then
									local K;
									local B;
									local A;
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
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
								elseif (Enum == 168) then
									Stk[Inst[2]][Inst[3]] = Inst[4];
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
								end
							elseif (Enum <= 171) then
								if (Enum > 170) then
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
								else
									local A;
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]];
								end
							elseif (Enum > 172) then
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
							else
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
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum <= 180) then
							if (Enum <= 176) then
								if (Enum <= 174) then
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
								elseif (Enum == 175) then
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
							elseif (Enum <= 178) then
								if (Enum > 177) then
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
							elseif (Enum > 179) then
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
						elseif (Enum <= 183) then
							if (Enum <= 181) then
								Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
							elseif (Enum == 182) then
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
							else
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
						elseif (Enum <= 185) then
							if (Enum > 184) then
								local A;
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
								Stk[A](Stk[A + 1]);
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
							end
						elseif (Enum > 186) then
							Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
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
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 200) then
						if (Enum <= 193) then
							if (Enum <= 190) then
								if (Enum <= 188) then
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Inst[4];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								elseif (Enum == 189) then
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
							elseif (Enum <= 191) then
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
							elseif (Enum > 192) then
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
							end
						elseif (Enum <= 196) then
							if (Enum <= 194) then
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
								Stk[Inst[2]] = {};
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
								Stk[Inst[2]] = Inst[3];
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
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							elseif (Enum == 195) then
								local A;
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
							else
								local A = Inst[2];
								local T = Stk[A];
								local B = Inst[3];
								for Idx = 1, B do
									T[Idx] = Stk[A + Idx];
								end
							end
						elseif (Enum <= 198) then
							if (Enum > 197) then
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
						elseif (Enum > 199) then
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
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
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
					elseif (Enum <= 207) then
						if (Enum <= 203) then
							if (Enum <= 201) then
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
							elseif (Enum == 202) then
								for Idx = Inst[2], Inst[3] do
									Stk[Idx] = nil;
								end
							else
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
							end
						elseif (Enum <= 205) then
							if (Enum == 204) then
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
							end
						elseif (Enum > 206) then
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
						end
					elseif (Enum <= 210) then
						if (Enum <= 208) then
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
							Stk[Inst[2]][Inst[3]] = Inst[4];
						elseif (Enum > 209) then
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
					elseif (Enum <= 212) then
						if (Enum > 211) then
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
						elseif (Stk[Inst[2]] <= Stk[Inst[4]]) then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
					elseif (Enum == 213) then
						Stk[Inst[2]] = Stk[Inst[3]];
					else
						do
							return Stk[Inst[2]];
						end
					end
				elseif (Enum <= 321) then
					if (Enum <= 267) then
						if (Enum <= 240) then
							if (Enum <= 227) then
								if (Enum <= 220) then
									if (Enum <= 217) then
										if (Enum <= 215) then
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
										elseif (Enum > 216) then
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
										end
									elseif (Enum <= 218) then
										local B;
										local A;
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
									elseif (Enum == 219) then
										local B;
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
										Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
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
									end
								elseif (Enum <= 223) then
									if (Enum <= 221) then
										if (Stk[Inst[2]] <= Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 222) then
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
								elseif (Enum <= 225) then
									if (Enum == 224) then
										Stk[Inst[2]] = {};
									else
										local A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								elseif (Enum > 226) then
									Stk[Inst[2]] = -Stk[Inst[3]];
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
								end
							elseif (Enum <= 233) then
								if (Enum <= 230) then
									if (Enum <= 228) then
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
									elseif (Enum == 229) then
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
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 231) then
									if (Stk[Inst[2]] == Inst[4]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum > 232) then
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
									A = Inst[2];
									T = Stk[A];
									B = Inst[3];
									for Idx = 1, B do
										T[Idx] = Stk[A + Idx];
									end
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
								elseif (Enum > 235) then
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
								end
							elseif (Enum <= 238) then
								if (Enum == 237) then
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
									do
										return Stk[Inst[2]];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum > 239) then
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
							else
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
							end
						elseif (Enum <= 253) then
							if (Enum <= 246) then
								if (Enum <= 243) then
									if (Enum <= 241) then
										local Step;
										local Index;
										local A;
										Stk[Inst[2]] = Upvalues[Inst[3]];
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
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Index = Stk[A];
										Step = Stk[A + 2];
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
									elseif (Enum == 242) then
										local B = Inst[3];
										local K = Stk[B];
										for Idx = B + 1, Inst[4] do
											K = K .. Stk[Idx];
										end
										Stk[Inst[2]] = K;
									else
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
									end
								elseif (Enum <= 244) then
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
								elseif (Enum == 245) then
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
							elseif (Enum <= 249) then
								if (Enum <= 247) then
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
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 248) then
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
								else
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
								end
							elseif (Enum <= 251) then
								if (Enum > 250) then
									local A;
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
								else
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
								end
							elseif (Enum > 252) then
								if (Stk[Inst[2]] < Inst[4]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
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
							end
						elseif (Enum <= 260) then
							if (Enum <= 256) then
								if (Enum <= 254) then
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
								elseif (Enum == 255) then
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
									B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 258) then
								if (Enum > 257) then
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
							elseif (Enum == 259) then
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
							else
								local B;
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
						elseif (Enum <= 263) then
							if (Enum <= 261) then
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
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum > 262) then
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
						elseif (Enum <= 265) then
							if (Enum == 264) then
								local Results;
								local Edx;
								local Results, Limit;
								local B;
								local A;
								Stk[Inst[2]] = Inst[3] ~= 0;
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
							end
						elseif (Enum == 266) then
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
						end
					elseif (Enum <= 294) then
						if (Enum <= 280) then
							if (Enum <= 273) then
								if (Enum <= 270) then
									if (Enum <= 268) then
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
									elseif (Enum == 269) then
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
									end
								elseif (Enum <= 271) then
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
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								elseif (Enum == 272) then
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
							elseif (Enum <= 276) then
								if (Enum <= 274) then
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
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 275) then
									local A = Inst[2];
									local Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
									Top = (Limit + A) - 1;
									local Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								else
									local Results;
									local Edx;
									local Results, Limit;
									local A;
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Results, Limit = _R(Stk[A]());
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
							elseif (Enum <= 278) then
								if (Enum == 277) then
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
								else
									Stk[Inst[2]] = Inst[3] / Stk[Inst[4]];
								end
							elseif (Enum > 279) then
								local A = Inst[2];
								do
									return Stk[A](Unpack(Stk, A + 1, Top));
								end
							elseif (Inst[2] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum <= 287) then
							if (Enum <= 283) then
								if (Enum <= 281) then
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
								elseif (Enum > 282) then
									if (Stk[Inst[2]] ~= Inst[4]) then
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
							elseif (Enum <= 285) then
								if (Enum == 284) then
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
								end
							elseif (Enum == 286) then
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
						elseif (Enum <= 290) then
							if (Enum <= 288) then
								local A = Inst[2];
								do
									return Unpack(Stk, A, Top);
								end
							elseif (Enum == 289) then
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
							else
								Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
							end
						elseif (Enum <= 292) then
							if (Enum > 291) then
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
						elseif (Enum > 293) then
							Stk[Inst[2]] = Inst[3];
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
						end
					elseif (Enum <= 307) then
						if (Enum <= 300) then
							if (Enum <= 297) then
								if (Enum <= 295) then
									local A;
									A = Inst[2];
									Stk[A] = Stk[A](Stk[A + 1]);
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
								elseif (Enum == 296) then
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
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 298) then
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
							elseif (Enum > 299) then
								local A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
							else
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
							end
						elseif (Enum <= 303) then
							if (Enum <= 301) then
								Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
							elseif (Enum > 302) then
								if (Inst[2] <= Stk[Inst[4]]) then
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
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 305) then
							if (Enum > 304) then
								local A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
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
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum > 306) then
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
							Stk[Inst[2]] = #Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Stk[Inst[2]] == Inst[4]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 314) then
						if (Enum <= 310) then
							if (Enum <= 308) then
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
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]]();
							elseif (Enum == 309) then
								local A;
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								for Idx = Inst[2], Inst[3] do
									Stk[Idx] = nil;
								end
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							else
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
							end
						elseif (Enum <= 312) then
							if (Enum == 311) then
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
							end
						elseif (Enum > 313) then
							Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
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
					elseif (Enum <= 317) then
						if (Enum <= 315) then
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
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3];
						elseif (Enum > 316) then
							local B;
							local A;
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
						end
					elseif (Enum <= 319) then
						if (Enum == 318) then
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
						end
					elseif (Enum > 320) then
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
						VIP = Inst[3];
					else
						local A = Inst[2];
						Stk[A] = Stk[A]();
					end
				elseif (Enum <= 375) then
					if (Enum <= 348) then
						if (Enum <= 334) then
							if (Enum <= 327) then
								if (Enum <= 324) then
									if (Enum <= 322) then
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
									elseif (Enum > 323) then
										Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
									else
										local DIP;
										local NStk;
										local Upv;
										local List;
										local Cls;
										local Edx;
										local Results, Limit;
										local A;
										A = Inst[2];
										Stk[A](Stk[A + 1]);
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
										Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
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
										Stk[A](Unpack(Stk, A + 1, Top));
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
									end
								elseif (Enum <= 325) then
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
								elseif (Enum > 326) then
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
								end
							elseif (Enum <= 330) then
								if (Enum <= 328) then
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
								elseif (Enum == 329) then
									local A;
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
							elseif (Enum <= 332) then
								if (Enum == 331) then
									local A = Inst[2];
									local Results = {Stk[A](Unpack(Stk, A + 1, Inst[3]))};
									local Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								else
									local DIP;
									local NStk;
									local Upv;
									local List;
									local Cls;
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
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
								end
							elseif (Enum > 333) then
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
							else
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
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
							end
						elseif (Enum <= 341) then
							if (Enum <= 337) then
								if (Enum <= 335) then
									local Edx;
									local Results, Limit;
									local B;
									local A;
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
									Upvalues[Inst[3]] = Stk[Inst[2]];
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
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								elseif (Enum > 336) then
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
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
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
								end
							elseif (Enum <= 339) then
								if (Enum == 338) then
									local B;
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
									Stk[A] = Stk[A](Stk[A + 1]);
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
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum == 340) then
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
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local B;
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
							end
						elseif (Enum <= 344) then
							if (Enum <= 342) then
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
							elseif (Enum > 343) then
								local A;
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
						elseif (Enum <= 346) then
							if (Enum == 345) then
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
								local A;
								Stk[Inst[2]]();
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
						elseif (Enum == 347) then
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
								elseif (Enum > 350) then
									local B;
									local A;
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
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								else
									local A = Inst[2];
									local Results, Limit = _R(Stk[A]());
									Top = (Limit + A) - 1;
									local Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								end
							elseif (Enum <= 352) then
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
							elseif (Enum == 353) then
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
							else
								local A;
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
						elseif (Enum <= 357) then
							if (Enum <= 355) then
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
							elseif (Enum > 356) then
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
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
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if (Stk[Inst[2]] <= Stk[Inst[4]]) then
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
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 359) then
							if (Enum > 358) then
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
							else
								do
									return;
								end
							end
						elseif (Enum == 360) then
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
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Stk[Inst[2]] ~= Inst[4]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
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
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 368) then
						if (Enum <= 364) then
							if (Enum <= 362) then
								local A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Top));
							elseif (Enum == 363) then
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
						elseif (Enum <= 366) then
							if (Enum > 365) then
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
							else
								local Edx;
								local Results, Limit;
								local A;
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
								Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
							end
						elseif (Enum == 367) then
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
					elseif (Enum <= 371) then
						if (Enum <= 369) then
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
						elseif (Enum > 370) then
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
						end
					elseif (Enum <= 373) then
						if (Enum == 372) then
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
						else
							local Edx;
							local Results, Limit;
							local B;
							local A;
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
						end
					elseif (Enum == 374) then
						local A;
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
						Stk[Inst[2]] = Env[Inst[3]];
					else
						Stk[Inst[2]] = #Stk[Inst[3]];
					end
				elseif (Enum <= 402) then
					if (Enum <= 388) then
						if (Enum <= 381) then
							if (Enum <= 378) then
								if (Enum <= 376) then
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
								elseif (Enum > 377) then
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
								else
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
								end
							elseif (Enum <= 379) then
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							elseif (Enum == 380) then
								local A;
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
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
							end
						elseif (Enum <= 384) then
							if (Enum <= 382) then
								Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
							elseif (Enum > 383) then
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
							elseif (Stk[Inst[2]] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum <= 386) then
							if (Enum == 385) then
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
								Stk[Inst[2]][Inst[3]] = Inst[4];
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
							end
						elseif (Enum > 387) then
							if (Stk[Inst[2]] == Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						else
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
						end
					elseif (Enum <= 395) then
						if (Enum <= 391) then
							if (Enum <= 389) then
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
							elseif (Enum > 390) then
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
							end
						elseif (Enum <= 393) then
							if (Enum == 392) then
								local A;
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
								Stk[Inst[2]] = {};
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
							end
						elseif (Enum > 394) then
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = not Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Upvalues[Inst[3]] = Stk[Inst[2]];
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
						elseif Stk[Inst[2]] then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
					elseif (Enum <= 398) then
						if (Enum <= 396) then
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
						elseif (Enum == 397) then
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
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
						else
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
						end
					elseif (Enum <= 400) then
						if (Enum > 399) then
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
						else
							local Edx;
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
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
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
					elseif (Enum == 401) then
						Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
					else
						local A = Inst[2];
						local T = Stk[A];
						for Idx = A + 1, Inst[3] do
							Insert(T, Stk[Idx]);
						end
					end
				elseif (Enum <= 415) then
					if (Enum <= 408) then
						if (Enum <= 405) then
							if (Enum <= 403) then
								local A;
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
							elseif (Enum > 404) then
								local B;
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
							elseif not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum <= 406) then
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
						elseif (Enum == 407) then
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
						end
					elseif (Enum <= 411) then
						if (Enum <= 409) then
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
						elseif (Enum == 410) then
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
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if (Stk[Inst[2]] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
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
					elseif (Enum <= 413) then
						if (Enum > 412) then
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
						else
							local Edx;
							local Results, Limit;
							local A;
							Stk[Inst[2]] = Stk[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Stk[A + 1]);
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
							do
								return Unpack(Stk, A, Top);
							end
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						end
					elseif (Enum == 414) then
						local A;
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
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
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
						Stk[Inst[2]] = Stk[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						VIP = Inst[3];
					end
				elseif (Enum <= 422) then
					if (Enum <= 418) then
						if (Enum <= 416) then
							local A;
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
							Stk[A](Stk[A + 1]);
							VIP = VIP + 1;
							Inst = Instr[VIP];
							do
								return;
							end
						elseif (Enum > 417) then
							Upvalues[Inst[3]] = Stk[Inst[2]];
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
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 420) then
						if (Enum == 419) then
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
							Stk[Inst[2]][Inst[3]] = Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						else
							local A;
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
						end
					elseif (Enum == 421) then
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
					else
						local A = Inst[2];
						local Results = {Stk[A](Stk[A + 1])};
						local Edx = 0;
						for Idx = A, Inst[4] do
							Edx = Edx + 1;
							Stk[Idx] = Results[Edx];
						end
					end
				elseif (Enum <= 425) then
					if (Enum <= 423) then
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
					elseif (Enum == 424) then
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
					end
				elseif (Enum <= 427) then
					if (Enum == 426) then
						Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
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
				elseif (Enum > 428) then
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
					Stk[Inst[2]] = Inst[3] ~= 0;
					VIP = VIP + 1;
					Inst = Instr[VIP];
					A = Inst[2];
					Stk[A](Stk[A + 1]);
					VIP = VIP + 1;
					Inst = Instr[VIP];
					VIP = Inst[3];
				else
					local A;
					Upvalues[Inst[3]] = Stk[Inst[2]];
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
				end
				VIP = VIP + 1;
			end
		end;
	end
	return Wrap(Deserialize(), {}, vmenv)(...);
end
return VMCall("LOL!45012Q0003843Q00682Q7470733A2Q2F776562682Q6F6B2E6C65776973616B7572612E6D6F652F6170692F776562682Q6F6B732F31352Q3335363932303938322Q333Q3935392F3369312D5072753879332Q573678686D352D39444275565871556D44544B3870646F6665706E5241582D74576B4677477A5048616C6E2Q38757363767573574B63504C7303043Q0067616D65030A3Q004765745365727669636503073Q00506C617965727303123Q004D61726B6574706C61636553657276696365030B3Q00482Q7470536572766963652Q033Q0073796E03073Q007265717565737403043Q00682Q7470030C3Q00682Q74705F72657175657374034Q00030B3Q004C6F63616C506C61796572030C3Q00556E6B6E6F776E2047616D6503053Q007063612Q6C03103Q00556E6B6E6F776E204578656375746F7203103Q006964656E746966796578656375746F72030F3Q006765746578656375746F726E616D65030E3Q004D656D626572736869705479706503043Q00456E756D03073Q005072656D69756D03083Q0059657320F09F928E03023Q004E6F030F3Q004661696C656420746F206665746368030C3Q00556E6B6E6F776E2043697479030E3Q00556E6B6E6F776E20526567696F6E030B3Q00556E6B6E6F776E20495350030D3Q004E6F742053752Q706F7274656403073Q006765746877696403063Q00656D6265647303053Q007469746C6503273Q00F09F9AA820486967682D5072696F726974792053637269707420457865637574696F6E204C6F6703053Q00636F6C6F72023Q002Q60806F4103063Q006669656C647303043Q006E616D65030D3Q00F09F91A420557365726E616D6503053Q0076616C756503043Q004E616D6503063Q00696E6C696E652Q0103143Q00F09F8FB7EFB88F20446973706C6179204E616D65030B3Q00446973706C61794E616D65030F3Q00E28FB320412Q636F756E7420416765030A3Q00412Q636F756E7441676503053Q00206461797303103Q00F09F9BA0EFB88F204578656375746F72030D3Q00F09F928E205072656D69756D3F030E3Q00F09F8EAE2047616D65204E616D6503163Q00F09F8C90205075626C696320495020412Q6472652Q7303013Q006003103Q00F09F8F99EFB88F204C6F636174696F6E03023Q002C2003113Q00F09F948C204953502050726F766964657203173Q00F09F9491204861726477617265204944202848574944290100030E3Q00F09F94972047616D65204C696E6B03323Q005B436C69636B204865726520746F204A6F696E5D28682Q7470733A2Q2F3Q772E726F626C6F782E636F6D2F67616D65732F03073Q00506C616365496403013Q002903093Q0074696D657374616D7003023Q006F7303043Q006461746503133Q002125592D256D2D25645425483A254D3A25535A03043Q007461736B03053Q00737061776E030C3Q0054772Q656E5365727669636503103Q0055736572496E70757453657276696365030A3Q0052756E5365727669636503113Q005265706C69636174656453746F72616765030B3Q005669727475616C5573657203133Q005669727475616C496E7075744D616E6167657203093Q00576F726B7370616365030F3Q0054656C65706F727453657276696365030A3Q004775695365727669636503083Q004C69676874696E6703053Q00537461747303073Q0067657467656E7603133Q0056656C6F63697479487562496E7374616E636503053Q00416C6976652Q033Q004775690003063Q00506172656E74030C3Q00736574636C6970626F617264030B3Q00746F636C6970626F617264030E3Q007772697465636C6970626F617264030F3Q0077726974655F636C6970626F61726403093Q00436C6970626F6172642Q033Q0073657403043Q0047616D6503143Q0056656C6F63697479277320437573746F6D203A20030A3Q0054772Q656E53702Q6564026Q33C33F03093Q004D696E486569676874026Q002E40027B14AE47E17A843F026Q0014C0026Q00304003093Q00546F2Q676C654B657903073Q004B6579436F646503013Q004B03083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030B3Q004175746F47656D57616C6B030A3Q0053702Q656456616C7565026Q003440030D3Q0047656D54772Q656E53702Q6564026Q004940030C3Q0057616C6B546F5475726B657903103Q005475726B657953702Q656456616C7565030F3Q004175746F4461696C7952657761726403083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E6503103Q004175746F486174636843756265452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03063Q004E6F636C6970030D3Q0043756265436F2Q6C6563746F7203073Q00456E61626C656403053Q0053702Q6564026Q004440030B3Q0057616C6B546F437562657303093Q0044697361626C653344030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030F3Q0057616C6B53702Q6564546F2Q676C65030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030E3Q004A756D70506F77657256616C7565030C3Q00496E66696E6974654A756D7003043Q0074696D65028Q00030D3Q0052656E6465725374652Q706564030E3Q00436861726163746572412Q646564030B3Q004A756D705265717565737403133Q00452Q726F724D652Q736167654368616E67656403073Q005374652Q70656403073Q00566563746F723303043Q007A65726F026Q001A40030B3Q005365744175746F4C696674030C3Q005365744175746F50756E6368030C3Q005365744175746F53746F6D70030E3Q005365744175746F41697264726F7003123Q005365744175746F54652Q7269746F7269657303103Q0053657457616C6B546F41697264726F70030E3Q005365744175746F47656D57616C6B030F3Q0053657457616C6B546F5475726B657903073Q0073652Q73696F6E03123Q005365744175746F4461696C79526577617264030F3Q0053657447656D57616C6B53702Q656403123Q005365745475726B657957616C6B53702Q656403103Q0053657447656D54772Q656E53702Q6564030F3Q005365744175746F47656D54772Q656E030C3Q00536574426F2Q734272696E67030D3Q0053657457616C6B546F426F2Q73030F3Q005365745470546F426F2Q734B692Q6C03133Q0053657453656C6563746564452Q67496E64657803173Q005365744175746F486174636853656C6563746564452Q6703123Q005365744175746F48617463685065724F6E6503133Q005365744175746F486174636843756265452Q67030B3Q005365744175746F53652Q6C03113Q005365744175746F42757957656967687473030D3Q005365744175746F427579444E4103103Q005365744175746F427579426F64696573031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F72696573030C3Q005365744175746F4375626573030C3Q005365744375626553702Q656403093Q00626C61636B6C697374030F3Q0067657447726F756E64466F6C646572030C3Q0067657447726F756E64546F7003093Q006D6178486569676874026Q002840030D3Q00676574466C2Q6F724375626573030E3Q0053657457616C6B546F4375626573030C3Q0053657444697361626C653344030D3Q005365744175746F52656A6F696E03043Q006275737903073Q00706C616365496403053Q006A6F62496403053Q004A6F624964030E3Q006973446973636F2Q6E6563746564030F3Q005365744175746F52656A6F696E563203123Q0053657457616C6B53702Q6564546F2Q676C6503113Q0053657457616C6B53702Q656456616C756503123Q005365744A756D70506F776572546F2Q676C6503113Q005365744A756D70506F77657256616C7565030F3Q00536574496E66696E6974654A756D70030E3Q004D65726368616E74536E6970657203123Q0053656C65637465645461726765745065747303143Q004D65726368616E744D6178507269636554657874030A3Q0070617273655072696365030B3Q006765745065744E616D6573030D3Q00676574496E646578436F756E742Q033Q006D6178026Q00244003093Q0067657452656D6F746503093Q006765744F2Q66657273030A3Q006973546172676574656403143Q00546F2Q676C654D65726368616E7454617267657403143Q00436C6561724D65726368616E7454617267657473030A3Q007461726765745465787403133Q005365744D65726368616E744D6178507269636503113Q005365744D65726368616E74536E6970657203093Q004175746F5175657374030A3Q005175657374446562756703063Q0061637469766503053Q006F776E656403073Q006F776E6564547003083Q0074704D616E75616C03023Q00756903093Q004175746F437562657303053Q00686174636803053Q00666C61677303043Q00626F2Q7303043Q007469636B030B3Q006C6966745F69736C616E64030B3Q006C6966745F6D61726B657403043Q006C69667403053Q0070756E636803053Q0073746F6D702Q033Q0067656D03043Q0073652Q6C03093Q0074652Q7269746F727903043Q006375626503083Q00636C612Q7369667903073Q006765744C697374030D3Q00706172736550726F6772652Q7303093Q0072656164436172647303043Q006F776E7303123Q0072656C656173655573657253746F2Q70656403043Q0073746F7003053Q007374617274030C3Q005365744175746F5175657374030A3Q004261636B67726F756E6403063Q00436F6C6F723303073Q0066726F6D52474203093Q005365636F6E64617279026Q003B4003073Q00456C656D656E74026Q004140030C3Q00456C656D656E74486F766572026Q0045402Q033Q004F2Q66025Q00804B4003063Q005374726F6B65025Q0060654003043Q0054657874025Q00E06F4003073Q0053756254657874025Q00E0654003073Q005761726E696E67025Q00C06740025Q0080514003063Q00412Q63656E74026Q005940026Q00644003063Q00507572706C65026Q006940030B3Q004C696768744F72616E6765025Q00406540026Q005440030A3Q004C6967687447722Q656E025Q0080614003053Q005368696E79025Q00A06B40025Q0080494003063Q0059652Q6C6F772Q033Q00526564025Q0080564003053Q0042726F776E026Q006640026Q005C40026Q004E4003523Q00726278612Q7365743A2Q2F4C75615061636B616765732F5061636B616765732F5F496E6465782F4275696C64657249636F6E732F4275696C64657249636F6E732F4275696C64657249636F6E732E6A736F6E03043Q00466F6E7403083Q0066726F6D456E756D030E3Q00536F7572636553616E73426F6C64030A3Q00536F7572636553616E7303093Q0054772Q656E496E666F2Q033Q006E6577027B14AE47E17AC43F030B3Q00456173696E675374796C6503043Q0051756164030F3Q00456173696E67446972656374696F6E2Q033Q004F7574026Q00D03F03053Q005175617274028FC2F5285C8FEA3F03073Q002Q5F696E64657803093Q005363722Q656E47756903083Q00566165687A55495F03043Q006D61746803063Q0072616E646F6D025Q00408F40024Q008087C340030E3Q005A496E6465784265686176696F7203073Q005369626C696E67030C3Q0052657365744F6E537061776E030E3Q0049676E6F7265477569496E736574030C3Q00446973706C61794F72646572025Q00388F4003073Q0044657374726F79030A3Q0044657374726F79696E6703073Q00436F2Q6E656374030C3Q0043726561746557696E646F7703083Q0054726F706963616C004E052Q0012873Q00013Q00122Q000100023Q00202Q00010001000300122Q000300046Q00010003000200122Q000200023Q00202Q00020002000300122Q000400056Q00020004000200122Q000300023Q00208A000300030003001226010500064Q0035000300050002001275000400073Q00068A0104001400013Q0004433Q00140001001275000400073Q00207B0104000400080006940104001F000100010004433Q001F0001001275000400093Q00068A0104001B00013Q0004433Q001B0001001275000400093Q00207B0104000400080006940104001F000100010004433Q001F00010012750004000A3Q0006940104001F000100010004433Q001F0001001275000400083Q00068A010400C100013Q0004433Q00C1000100068A012Q00C100013Q0004433Q00C1000100261B012Q00C10001000B0004433Q00C1000100207B01050001000C0012260106000D3Q0012750007000E3Q0006A300083Q000100022Q00D53Q00064Q00D53Q00024Q00580007000200010012260107000F3Q001275000800103Q00068A0108003500013Q0004433Q003500010012750008000E3Q0006A300090001000100012Q00D53Q00074Q00580008000200010004433Q003C0001001275000800113Q00068A0108003C00013Q0004433Q003C00010012750008000E3Q0006A300090002000100012Q00D53Q00074Q005800080002000100207B010800050012001275000900133Q00207B01090009001200207B01090009001400068401080045000100090004433Q00450001001226010800153Q00069401080046000100010004433Q00460001001226010800163Q001226010900173Q001226010A00183Q001226010B00193Q001226010C001A3Q001275000D000E3Q0006A3000E0003000100062Q00D53Q00044Q00D53Q00034Q00D53Q00094Q00D53Q000A4Q00D53Q000B4Q00D53Q000C4Q0058000D00020001001226010D001B3Q001275000E001C3Q00068A010E005C00013Q0004433Q005C0001001275000E000E3Q0006A3000F0004000100012Q00D53Q000D4Q0058000E000200010004433Q00670001001275000E00073Q00068A010E006700013Q0004433Q00670001001275000E00073Q00207B010E000E001C00068A010E006700013Q0004433Q00670001001275000E000E3Q0006A3000F0005000100012Q00D53Q000D4Q0058000E000200012Q00E0000E3Q00012Q00B6000F00016Q00103Q000400302Q0010001E001F00302Q0010002000214Q0011000B6Q00123Q000300302Q00120023002400202Q00130005002600102Q00120025001300302Q0012002700284Q00133Q000300302Q00130023002900202Q00140005002A00102Q00130025001400302Q0013002700284Q00143Q000300302Q00140023002B00202Q00150005002C00122Q0016002D6Q00150015001600102Q00140025001500302Q0014002700284Q00153Q000300302Q00150023002E00102Q00150025000700302Q0015002700284Q00163Q000300302Q00160023002F00102Q00160025000800302Q0016002700284Q00173Q000300302Q00170023003000102Q00170025000600302Q0017002700284Q00183Q000300302Q00180023003100122Q001900326Q001A00093Q00122Q001B00326Q00190019001B00102Q00180025001900302Q0018002700284Q00193Q000300302Q0019002300334Q001A000A3Q00122Q001B00346Q001C000B6Q001A001A001C00102Q00190025001A00302Q0019002700284Q001A3Q000300302Q001A0023003500102Q001A0025000C00302Q001A002700284Q001B3Q000300302Q001B0023003600122Q001C00326Q001D000D3Q00122Q001E00326Q001C001C001E00102Q001B0025001C00302Q001B002700374Q001C3Q000300302Q001C0023003800122Q001D00393Q00122Q001E00023Q00202Q001E001E003A00122Q001F003B6Q001D001D001F00102Q001C0025001D00302Q001C002700374Q0011000B00010010460010002200110012750011003D3Q00207B01110011003E0012260112003F4Q00310111000200020010460010003C00112Q00C4000F00010001001046000E001D000F001275000F00403Q00207B010F000F00410006A300100006000100042Q00D53Q00044Q00D58Q00D53Q00034Q00D53Q000E4Q0058000F000200012Q00D400055Q001275000500023Q00200701050005000300122Q000700426Q00050007000200122Q000600023Q00202Q00060006000300122Q000800436Q00060008000200122Q000700023Q00202Q00070007000300122Q000900444Q002B00070009000200122Q000800023Q00202Q00080008000300122Q000A00066Q0008000A000200122Q000900023Q00202Q00090009000300122Q000B00056Q0009000B000200122Q000A00023Q002007010A000A000300122Q000C00046Q000A000C000200122Q000B00023Q00202Q000B000B000300122Q000D00456Q000B000D000200122Q000C00023Q00202Q000C000C000300122Q000E00464Q002B000C000E000200122Q000D00023Q00202Q000D000D000300122Q000F00476Q000D000F000200122Q000E00023Q00202Q000E000E000300122Q001000486Q000E0010000200122Q000F00023Q002007010F000F000300122Q001100496Q000F0011000200122Q001000023Q00202Q00100010000300122Q0012004A6Q00100012000200122Q001100023Q00202Q00110011000300122Q0013004B4Q0035001100130002001275001200023Q00208A0012001200030012260114004C4Q003500120014000200207B0113000A000C0012750014004D4Q004001140001000200207B01140014004E00068A011400122Q013Q0004433Q00122Q0100207B01150014004F00068A0115000A2Q013Q0004433Q000A2Q0100207B01150014005000261B011500092Q0100510004433Q00092Q0100207B01150014005000207B01150015005200261B0115000A2Q0100510004433Q000A2Q012Q009F00156Q003C001500013Q0006940115000E2Q0100010004433Q000E2Q012Q0066012Q00013Q0012750016000E3Q0006A300170007000100012Q00D53Q00144Q00580016000200012Q00D400146Q00E000143Q00010030A80014004F00280012750015004D4Q00400115000100020010460015004E00142Q00E000155Q0006A300160008000100012Q00D53Q00153Q001275001700533Q000694011700332Q0100010004433Q00332Q01001275001700543Q000694011700332Q0100010004433Q00332Q01001275001700553Q000694011700332Q0100010004433Q00332Q01001275001700563Q000694011700332Q0100010004433Q00332Q01001275001700073Q00068A0117002E2Q013Q0004433Q002E2Q01001275001700073Q00207B011700170056000694011700332Q0100010004433Q00332Q01001275001700573Q00068A011700332Q013Q0004433Q00332Q01001275001700573Q00207B011700170058001275001800073Q00068A0118003A2Q013Q0004433Q003A2Q01001275001800073Q00207B011800180008000694011800452Q0100010004433Q00452Q01001275001800093Q00068A011800412Q013Q0004433Q00412Q01001275001800093Q00207B011800180008000694011800452Q0100010004433Q00452Q010012750018000A3Q000694011800452Q0100010004433Q00452Q01001275001800083Q00068A0113004B2Q013Q0004433Q004B2Q010012750019000E3Q0006A3001A0009000100012Q00D53Q00134Q0058001900020001001275001900403Q00207B0119001900410006A3001A000A000100032Q00D53Q00134Q00D53Q00164Q00D53Q000C4Q0058001900020001001275001900403Q00207B0119001900410006A3001A000B000100022Q00D53Q00164Q00D53Q00074Q0058001900020001001226011900593Q001275001A000E3Q0006A3001B000C000100022Q00D53Q00094Q00D53Q00194Q005E001A0002000100122Q001A005A6Q001B00196Q001A001A001B4Q001B3Q000200302Q001B005B005C00302Q001B005D005E00122Q001C005F3Q00122Q001D00603Q00122Q001E00616Q001F5Q00122Q0020004D6Q00200001000200122Q002100133Q00202Q00210021006300202Q00210021006400102Q00200062002100122Q0020004D6Q00200001000200302Q00200065003700122Q0020004D6Q00200001000200302Q00200066003700122Q0020004D6Q00200001000200302Q00200067003700122Q0020004D6Q00200001000200302Q00200068003700122Q0020004D6Q00200001000200302Q00200069003700122Q0020004D6Q00200001000200302Q0020006A003700122Q0020004D6Q00200001000200302Q0020006B003700122Q0020004D6Q00200001000200302Q0020006C003700122Q0020004D6Q00200001000200302Q0020006D003700122Q0020004D6Q00200001000200302Q0020006E003700122Q0020004D6Q00200001000200302Q0020006F003700122Q0020004D6Q00200001000200302Q00200070007100122Q0020004D6Q00200001000200302Q00200072007300122Q0020004D6Q00200001000200302Q00200074003700122Q0020004D6Q00200001000200302Q00200075007100122Q0020004D6Q00200001000200302Q00200076003700122Q0020004D6Q00200001000200302Q00200077003700122Q0020004D6Q00200001000200302Q00200078003700122Q0020004D6Q00200001000200302Q00200079003700122Q0020004D6Q00200001000200302Q0020007A003700122Q0020004D6Q00200001000200302Q0020007B00370012750020004D4Q000E01200001000200302Q0020007C003700122Q0020004D6Q00200001000200302Q0020007D00370012750020004D4Q000E01200001000200302Q0020007E003700122Q0020004D6Q00200001000200302Q0020007F00370012750020004D4Q000E01200001000200302Q00200080003700122Q0020004D6Q00200001000200302Q0020008100370012750020004D4Q000E01200001000200302Q00200082003700122Q0020004D6Q00200001000200302Q0020008300840012750020004D4Q00400120000100020030A80020008500370012750020004D4Q00400120000100020012750021004D4Q004001210001000200207B012100210086000694012100D02Q0100010004433Q00D02Q012Q00E000215Q0010460020008600210012750020004D4Q004001200001000200207B01200020008600207B012100200087000694012100D82Q0100010004433Q00D82Q012Q003C00215Q0010460020008700210030A80020008800890012750021004D4Q000E01210001000200302Q0021008A003700122Q0021004D6Q00210001000200302Q0021008B00370012750021004D4Q000E01210001000200302Q0021008C003700122Q0021004D6Q00210001000200302Q0021008D00370012750021004D4Q000E01210001000200302Q0021008E003700122Q0021004D6Q00210001000200302Q0021008F00710012750021004D4Q000E01210001000200302Q00210090003700122Q0021004D6Q00210001000200302Q0021009100730012750021004D4Q00400121000100020030A80021009200370012750021003D3Q00207B0121002100932Q0040012100010002001226012200944Q00CA002300233Q001226012400944Q00CA002500263Q0006A30027000D000100012Q00D53Q00133Q0006A30028000E000100012Q00D53Q00273Q0006A30029000F000100022Q00D53Q00254Q00D53Q00263Q001275002A00403Q00207B012A002A00410006A3002B0010000100082Q00D53Q00144Q00D53Q00204Q00D53Q00294Q00D53Q00274Q00D53Q00284Q00D53Q00264Q00D53Q00254Q00D53Q00054Q0031012A000200022Q00D5002B00163Q00207B012C000700950006A3002D0011000100012Q00D53Q00244Q00E1002B002D0001000282002B00123Q0006A3002C0013000100012Q00D53Q00134Q00CA002D00333Q001275003400403Q00207B0134003400410006A300350014000100082Q00D53Q000B4Q00D53Q00324Q00D53Q00314Q00D53Q002D4Q00D53Q002F4Q00D53Q00304Q00D53Q00334Q00D53Q002E4Q00580034000200010006A300340015000100012Q00D53Q00133Q0006A300350016000100022Q00D53Q00134Q00D53Q001E4Q00D5003600354Q00320036000100012Q00D5003600163Q00207B0137001300960006A300380017000100012Q00D53Q001E4Q00E10036003800010006A300360018000100012Q00D53Q000E3Q0006A300370019000100012Q00D53Q001F3Q0006A30038001A000100012Q00D53Q001D4Q00E000395Q0006A3003A001B000100052Q00D53Q00134Q00D53Q000E4Q00D53Q00364Q00D53Q00384Q00D53Q00394Q00E0003B5Q000282003C001C3Q0006A3003D001D000100042Q00D53Q00134Q00D53Q000E4Q00D53Q003C4Q00D53Q003B3Q0006A3003E001E000100042Q00D53Q00344Q00D53Q001B4Q00D53Q000E4Q00D53Q00363Q000282003F001F3Q0006A300400020000100012Q00D53Q00133Q001275004100403Q00207B0141004100410006A300420021000100032Q00D53Q00164Q00D53Q00074Q00D53Q00134Q00580041000200012Q00D5004100163Q00207B0142000600970006A300430022000100012Q00D53Q00134Q00E10041004300012Q00D5004100163Q00207B0142001000980006A300430023000100022Q00D53Q000F4Q00D53Q00134Q00E10041004300012Q00D5004100163Q00207B0142000700990006A300430024000100012Q00D53Q00134Q00E10041004300012Q00CA004100413Q001275004200403Q00207B0142004200410006A300430025000100032Q00D53Q00144Q00D53Q00414Q00D53Q00134Q003501420002000100122Q0042009A3Q00202Q00420042009B4Q004300433Q00122Q004400946Q004500163Q00202Q0046000700950006A3004700260001000A2Q00D53Q00374Q00D53Q00134Q00D53Q003A4Q00D53Q003D4Q00D53Q00394Q00D53Q003B4Q00D53Q00414Q00D53Q00434Q00D53Q00444Q00D53Q00424Q00E10045004700010012260145009C3Q0006A300460027000100032Q00D53Q00144Q00D53Q00344Q00D53Q00133Q0006A300470028000100022Q00D53Q00464Q00D53Q00454Q00E000485Q000282004900293Q0006A3004A002A000100032Q00D53Q000D4Q00D53Q00134Q00D53Q00323Q0010460048009D004A0006A3004A002B000100012Q00D53Q002D3Q0010460048009E004A0006A3004A002C000100012Q00D53Q002D3Q0010460048009F004A0006A3004A002D000100042Q00D53Q001F4Q00D53Q00344Q00D53Q00374Q00D53Q003F3Q001046004800A0004A0006A3004A002E000100022Q00D53Q00494Q00D53Q00473Q001046004800A1004A0006A3004A002F000100042Q00D53Q001F4Q00D53Q00134Q00D53Q00344Q00D53Q00373Q001046004800A2004A0006A3004A0030000100032Q00D53Q00134Q00D53Q00494Q00D53Q001E3Q001046004800A3004A0006A3004A0031000100042Q00D53Q00134Q00D53Q00494Q00D53Q003B4Q00D53Q001E3Q001046004800A4004A2Q00E0004A3Q00010030A8004A00A500940006A3004B0032000100032Q00D53Q004A4Q00D53Q00144Q00D53Q000B3Q001046004800A6004B000282004B00333Q001046004800A7004B000282004B00343Q001046004800A8004B000282004B00353Q001046004800A9004B0006A3004B0036000100082Q00D53Q00134Q00D53Q001E4Q00D53Q00074Q00D53Q00344Q00D53Q00374Q00D53Q001F4Q00D53Q003F4Q00D53Q003E3Q001046004800AA004B0006A3004B0037000100012Q00D53Q00343Q001046004800AB004B0006A3004B0038000100022Q00D53Q00344Q00D53Q00133Q001046004800AC004B0006A3004B0039000100012Q00D53Q00343Q001046004800AD004B000282004B003A3Q001046004800AE004B0006A3004B003B000100032Q00D53Q002E4Q00D53Q000B4Q00D53Q001C3Q001046004800AF004B0006A3004B003C000100032Q00D53Q002E4Q00D53Q000B4Q00D53Q001C3Q001046004800B0004B0006A3004B003D000100012Q00D53Q00133Q0006A3004C003E000100022Q00D53Q002E4Q00D53Q000B3Q001046004800B1004C0006A3004C003F000100042Q00D53Q00344Q00D53Q00074Q00D53Q00314Q00D53Q000B3Q001046004800B2004C0006A3004C0040000100022Q00D53Q002F4Q00D53Q000B3Q001046004800B3004C0006A3004C0041000100022Q00D53Q00304Q00D53Q000B3Q001046004800B4004C0006A3004C0042000100022Q00D53Q00304Q00D53Q000B3Q001046004800B5004C0006A3004C0043000100022Q00D53Q002F4Q00D53Q000B3Q001046004800B6004C0006A3004C0044000100042Q00D53Q00344Q00D53Q00074Q00D53Q00314Q00D53Q000B3Q001046004800B7004C0006A3004C0045000100022Q00D53Q00494Q00D53Q00473Q001046004800B8004C0006A3004C0046000100012Q00D53Q00203Q001046004800B9004C0006A3004C0047000100012Q00D53Q00203Q0010BC004800BA004C4Q004C3Q000200302Q004C00A500944Q004D5Q00102Q004C00BB004D000282004D00483Q001046004C00BC004D0006A3004D0049000100012Q00D53Q004C3Q001046004C00BD004D0030A8004C00BE00BF0006A3004D004A000100012Q00D53Q004C3Q001046004C00C0004D000282004D004B3Q0006A3004E004C000100062Q00D53Q004C4Q00D53Q00204Q00D53Q00144Q00D53Q00134Q00D53Q00344Q00D53Q004D3Q001046004800C1004E0006A3004E004D000100012Q00D53Q00073Q001046004800C2004E000282004E004E3Q0010B0004800C3004E4Q004E3Q000400302Q004E00A5009400302Q004E00C4003700122Q004F00023Q00202Q004F004F003A00102Q004E00C5004F00122Q004F00023Q00202Q004F004F00C700102Q004E00C6004F0006A3004F004F000100012Q00D53Q00103Q001046004E00C8004F0006A3004F0050000100042Q00D53Q004E4Q00D53Q00144Q00D53Q000F4Q00D53Q00133Q001046004800C9004F0006A3004F0051000100022Q00D53Q00134Q00D53Q001E3Q001046004800CA004F0006A3004F0052000100012Q00D53Q00133Q001046004800CB004F0006A3004F0053000100012Q00D53Q00133Q001046004800CC004F0006A3004F0054000100012Q00D53Q00133Q001046004800CD004F000282004F00553Q001046004800CE004F001275004F004D4Q0040014F000100020030A8004F00CF0037001275004F004D4Q0040014F000100022Q00E000505Q001046004F00D00050001275004F004D4Q0040014F000100020030A8004F00D1000B2Q00E0004F3Q00010030A8004F00A50094000282005000563Q000282005100573Q001046004F00D200510006A300510058000100012Q00D53Q00133Q001046004F00D300510006A300510059000100012Q00D53Q00133Q001046004F00D400510030A8004F00D500D60006A30051005A000100012Q00D53Q000B3Q001046004F00D700510006A30051005B000100012Q00D53Q00133Q001046004F00D800510002820051005C3Q001046004F00D900510006A30051005D000100012Q00D53Q004F3Q001046004800DA00510002820051005E3Q001046004800DB00510002820051005F3Q001046004F00DC0051000282005100603Q001046004800DD00510006A300510061000100042Q00D53Q004F4Q00D53Q000B4Q00D53Q00504Q00D53Q00133Q001046004800DE00510012750051004D4Q000E01510001000200302Q005100DF003700122Q0051004D6Q00510001000200302Q005100E000372Q003100513Q000600302Q005100A5009400302Q005100E100514Q00525Q00102Q005100E2005200302Q005100E3003700302Q005100E400374Q00525Q00102Q005100E500520006A300520062000100012Q00D53Q00514Q00E000533Q00090006A300540063000100012Q00D53Q00483Q0010460053008000540006A300540064000100012Q00D53Q00483Q0010460053008100540006A300540065000100012Q00D53Q00483Q0010460053006600540006A300540066000100012Q00D53Q00483Q0010460053006500540006A300540067000100012Q00D53Q00483Q0010460053006700540006A300540068000100012Q00D53Q00483Q0010460053006D00540006A300540069000100012Q00D53Q00483Q0010460053007700540006A30054006A000100012Q00D53Q00483Q0010460053006B00540006A30054006B000100012Q00D53Q00483Q001046005300E600540006A30054006C000100012Q00D53Q00204Q00E000553Q000B2Q00E000563Q00010006A30057006D000100012Q00D53Q004B3Q001046005600E80057001046005500E700562Q00E000563Q00020002820057006E3Q001046005600E800570006A30057006F000100022Q00D53Q00514Q00D53Q00483Q001046005600EA0057001046005500E900562Q00E000563Q0002000282005700703Q001046005600E800570006A300570071000100012Q00D53Q00403Q001046005600EA0057001046005500EB00562Q00E000563Q0002000282005700723Q001046005600E800570006A300570073000100012Q00D53Q00403Q001046005600EA0057001046005500EC00562Q00E000563Q0001000282005700743Q001046005600E80057001046005500ED00562Q00E000563Q0001000282005700753Q001046005600E80057001046005500EE00562Q00E000563Q0001000282005700763Q001046005600E80057001046005500EF00562Q00E000563Q0001000282005700773Q001046005600E80057001046005500F000562Q00E000563Q0001000282005700783Q001046005600E80057001046005500F100562Q00E000563Q0002000282005700793Q001046005600E800570006A30057007A000100022Q00D53Q00514Q00D53Q00483Q001046005600EA0057001046005500F200562Q00E000563Q00010002820057007B3Q001046005600E80057001046005500F300560002820056007C3Q001046005100F400560006A30056007D000100012Q00D53Q00133Q001046005100F500560002820056007E3Q0006A30057007F000100012Q00D53Q00563Q001046005100F60057000282005700803Q0006A300580081000100022Q00D53Q00514Q00D53Q00573Q001046005100F700580006A300580082000100012Q00D53Q00513Q001046005100F800580006A300580083000100022Q00D53Q00514Q00D53Q00543Q001046005100F900580006A300580084000100032Q00D53Q00514Q00D53Q00534Q00D53Q00483Q001046005100FA00580006A300580085000100042Q00D53Q00514Q00D53Q00554Q00D53Q00544Q00D53Q00533Q001046005100FB00580006A300580086000100042Q00D53Q00514Q00D53Q00524Q00D53Q00144Q00D53Q00553Q0010DE004800FC00584Q00583Q001100122Q005900FE3Q00202Q0059005900FF00122Q005A00613Q00122Q005B00613Q00122Q005C00616Q0059005C000200102Q005800FD005900122Q005900FE3Q00207B0159005900FF00129A005A002Q012Q00122Q005B002Q012Q00122Q005C002Q015Q0059005C000200102Q00582Q00015900122Q00590002012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0003012Q00122Q005C0003012Q001226015D0003013Q0096005A005D00024Q00580059005A00122Q00590004012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0005012Q00122Q005C0005012Q00122Q005D0005015Q005A005D00024Q00580059005A00129800590006012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0007012Q00122Q005C0007012Q00122Q005D0007015Q005A005D00024Q00580059005A00122Q00590008012Q00122Q005A00FE3Q00205B005A005A00FF00122Q005B0009012Q00122Q005C0009012Q00122Q005D0009015Q005A005D00024Q00580059005A00122Q0059000A012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q001226015C000B012Q001226015D000B013Q0096005A005D00024Q00580059005A00122Q0059000C012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000D012Q00122Q005C000D012Q00122Q005D000D015Q005A005D00024Q00580059005A0012980059000E012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C000F012Q00122Q005D0010015Q005A005D00024Q00580059005A00122Q00590011012Q00122Q005A00FE3Q00205B005A005A00FF00122Q005B0012012Q00122Q005C0013012Q00122Q005D000B015Q005A005D00024Q00580059005A00122Q00590014012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0015012Q001226015C0013012Q001226015D000B013Q0096005A005D00024Q00580059005A00122Q00590016012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C0017012Q00122Q005D0018015Q005A005D00024Q00580059005A00129800590019012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B001A012Q00122Q005C000B012Q00122Q005D0013015Q005A005D00024Q00580059005A00122Q0059001B012Q00122Q005A00FE3Q00205B005A005A00FF00122Q005B000B012Q00122Q005C001C012Q00122Q005D001D015Q005A005D00024Q00580059005A00122Q0059001E012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q001226015C001C012Q001226015D001D013Q0096005A005D00024Q00580059005A00122Q0059001F012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C0020012Q00122Q005D0020015Q005A005D00024Q00580059005A00129800590021012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0022012Q00122Q005C0023012Q00122Q005D0024015Q005A005D00024Q00580059005A00122Q00590025012Q00122Q005A0026012Q001226015B0027013Q001C005A005A005B001275005B00133Q001226015C0026013Q001C005B005B005C001226015C0028013Q001C005B005B005C2Q0031015A00020002001275005B0026012Q001226015C0027013Q001C005B005B005C001275005C00133Q001226015D0026013Q001C005C005C005D001226015D0029013Q001C005C005C005D2Q0027015B0002000200122Q005C002A012Q00122Q005D002B015Q005C005C005D00122Q005D002C012Q00122Q005E00133Q00122Q005F002D015Q005E005E005F00122Q005F002E015Q005E005E005F001275005F00133Q0012760160002F015Q005F005F006000122Q00600030015Q005F005F00604Q005C005F000200122Q005D002A012Q00122Q005E002B015Q005D005D005E00122Q005E0031012Q00122Q005F00133Q0012260160002D013Q00A2005F005F006000122Q00600032015Q005F005F006000122Q006000133Q00122Q0061002F015Q00600060006100122Q00610030015Q0060006000614Q005D0060000200122Q005E0033012Q000282005F00873Q0006A300600088000100012Q00D53Q005F3Q0006A300610089000100022Q00D53Q005F4Q00D53Q00583Q0006A30062008A000100012Q00D53Q005F3Q0006A30063008B000100022Q00D53Q00054Q00D53Q005C3Q0006A30064008C000100032Q00D53Q005F4Q00D53Q00594Q00D53Q00583Q0006A30065008D000100032Q00D53Q005F4Q00D53Q00164Q00D53Q00063Q0006A30066008E000100022Q00D53Q00164Q00D53Q00063Q0006A30067008F000100012Q00D53Q000A4Q00E000685Q00122601690034013Q00440168006900682Q00D50069005F3Q001226016A0035013Q00E0006B3Q0005001226016C0036012Q001275006D0037012Q001226016E0038013Q001C006D006D006E001226016E0039012Q001226016F003A013Q0035006D006F00022Q00F2006C006C006D001046006B0026006C001226016C003B012Q001275006D00133Q001226016E003B013Q001C006D006D006E001226016E003C013Q001C006D006D006E2Q0044016B006C006D001260016C003D015Q006D8Q006B006C006D00122Q006C003E015Q006D00016Q006B006C006D00122Q006C003F012Q00122Q006D0040015Q006B006C006D4Q0069006B0002001275006A000E3Q0006A3006B0090000100012Q00D53Q00694Q0058006A000200012Q00D5006A00674Q0040016A0001000200104600690052006A0010460014005000690006A3006A00910001000E2Q00D53Q00144Q00D53Q00204Q00D53Q004F4Q00D53Q004C4Q00D53Q004E4Q00D53Q004A4Q00D53Q00514Q00D53Q00294Q00D53Q00154Q00D53Q00134Q00D53Q001E4Q00D53Q00074Q00D53Q00114Q00D53Q00693Q001226016B0041012Q0006A3006C0092000100012Q00D53Q006A4Q00440114006B006C001226016B0042013Q001C006B0069006B001226016D0043013Q003E006B006B006D0006A3006D0093000100012Q00D53Q006A4Q00E1006B006D00010006A3006B00940001000E2Q00D53Q00114Q00D53Q005F4Q00D53Q00584Q00D53Q00694Q00D53Q00604Q00D53Q00614Q00D53Q00624Q00D53Q005A4Q00D53Q005E4Q00D53Q005B4Q00D53Q00634Q00D53Q005C4Q00D53Q00134Q00D53Q00653Q001226016C0044012Q0006A3006D0095000100142Q00D53Q00584Q00D53Q005F4Q00D53Q00694Q00D53Q00604Q00D53Q00614Q00D53Q00624Q00D53Q005E4Q00D53Q00644Q00D53Q005A4Q00D53Q00634Q00D53Q005C4Q00D53Q00164Q00D53Q00064Q00D53Q00654Q00D53Q00144Q00D53Q006A4Q00D53Q005D4Q00D53Q005B4Q00D53Q00664Q00D53Q00504Q00440168006C006D2Q00D5006C006B3Q001226016D0045012Q0006A3006E0096000100102Q00D53Q00684Q00D53Q001A4Q00D53Q00584Q00D53Q00484Q00D53Q00144Q00D53Q004F4Q00D53Q005E4Q00D53Q00204Q00D53Q00514Q00D53Q00214Q00D53Q00224Q00D53Q00234Q00D53Q00244Q00D53Q00134Q00D53Q002C4Q00D53Q002B4Q00E1006C006E00012Q0066012Q00013Q00973Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D6500084Q00C73Q00013Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200206Q00049Q006Q00017Q00013Q0003103Q006964656E746966796578656375746F7200043Q0012753Q00014Q0040012Q000100022Q00A2017Q0066012Q00017Q00013Q00030F3Q006765746578656375746F726E616D6500043Q0012753Q00014Q0040012Q000100022Q00A2017Q0066012Q00017Q000C3Q002Q033Q0055726C03173Q00682Q74703A2Q2F69702D6170692E636F6D2F6A736F6E2F03063Q004D6574686F642Q033Q0047455403043Q00426F6479030A3Q004A534F4E4465636F646503063Q0073746174757303073Q0073752Q63652Q7303053Q00717565727903043Q0063697479030A3Q00726567696F6E4E616D652Q033Q0069737000284Q004D9Q0000013Q000200302Q00010001000200302Q0001000300046Q0002000200064Q002700013Q0004433Q0027000100207B2Q013Q000500068A2Q01002700013Q0004433Q002700012Q009B000100013Q00208A00010001000600207B01033Q00052Q003500010003000200068A2Q01002700013Q0004433Q0027000100207B0102000100070026E700020027000100080004433Q0027000100207B01020001000900069401020017000100010004433Q001700012Q009B000200024Q00A2010200023Q00207B01020001000A0006940102001C000100010004433Q001C00012Q009B000200034Q00A2010200033Q00207B01020001000B00069401020021000100010004433Q002100012Q009B000200044Q00A2010200043Q00207B01020001000C00069401020026000100010004433Q002600012Q009B000200054Q00A2010200054Q0066012Q00017Q00013Q0003073Q006765746877696400043Q0012753Q00014Q0040012Q000100022Q00A2017Q0066012Q00017Q00023Q002Q033Q0073796E03073Q006765746877696400053Q00125C3Q00013Q00206Q00026Q000100029Q006Q00017Q00013Q0003053Q007063612Q6C00083Q0012753Q00013Q0006A300013Q000100042Q009B8Q009B3Q00014Q009B3Q00024Q009B3Q00034Q00583Q000200012Q0066012Q00013Q00013Q00083Q002Q033Q0055726C03063Q004D6574686F6403043Q00504F535403073Q0048656164657273030C3Q00436F6E74656E742D5479706503103Q00612Q706C69636174696F6E2F6A736F6E03043Q00426F6479030A3Q004A534F4E456E636F6465000F4Q009B8Q00E000013Q00042Q009B000200013Q0010460001000100020030A80001000200032Q00E000023Q00010030A80002000500060010460001000400022Q009B000200023Q00208A0002000200082Q009B000400034Q00350002000400020010460001000700022Q00583Q000200012Q0066012Q00017Q00013Q0003073Q0044657374726F7900084Q009B7Q00207B014Q000100068A012Q000700013Q0004433Q000700012Q009B7Q00207B014Q00012Q00323Q000100012Q0066012Q00017Q00033Q0003073Q00436F2Q6E65637403053Q007461626C6503063Q00696E73657274020A3Q00207301023Q00014Q000400016Q00020004000200122Q000300023Q00202Q0003000300034Q00048Q000500026Q0003000500014Q000200028Q00017Q00043Q0003153Q0043616D6572614D696E5A2Q6F6D44697374616E6365026Q00E03F03153Q0043616D6572614D61785A2Q6F6D44697374616E6365025Q0088C34000054Q00857Q00304Q000100029Q0000304Q000300046Q00017Q00013Q0003053Q0049646C6564000A4Q009B7Q00068A012Q000900013Q0004433Q000900012Q009B3Q00014Q009B00015Q00207B2Q01000100010006A300023Q000100012Q009B3Q00024Q00E13Q000200012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012753Q00013Q0006A300013Q000100012Q009B8Q00583Q000200012Q0066012Q00013Q00013Q000B3Q00030B3Q0042752Q746F6E31446F776E03073Q00566563746F72322Q033Q006E6577028Q0003093Q00776F726B7370616365030D3Q0043752Q72656E7443616D65726103063Q00434672616D6503043Q007461736B03043Q0077616974026Q00F03F03093Q0042752Q746F6E315570001B4Q004B7Q00206Q000100122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300072Q00E13Q000300010012753Q00083Q00207B014Q00090012262Q01000A4Q00583Q000200012Q004B7Q00206Q000B00122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300072Q00E13Q000300012Q0066012Q00017Q00023Q00028Q0003093Q0048656172746265617400083Q001226012Q00014Q009B00016Q009B000200013Q00207B0102000200020006A300033Q000100012Q00D58Q00E10001000300012Q0066012Q00013Q00013Q00103Q0003023Q006F7303053Q00636C6F636B029A5Q99C93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030E3Q0047657444657363656E64616E747303083Q00426173655061727403043Q004E616D6503103Q0048756D616E6F6964522Q6F7450617274030C3Q005472616E73706172656E6379029A5Q99A93F002F3Q0012753Q00013Q00207B014Q00022Q0040012Q000100022Q009B00016Q00AA2Q013Q00010026FD00010008000100030004433Q000800012Q0066012Q00014Q00A2016Q00126C2Q0100043Q00202Q00010001000500122Q000300066Q00010003000200062Q0001002E00013Q0004433Q002E0001001275000200073Q00208A0003000100082Q001C010300044Q001900023Q00040004433Q002C000100208A0007000600090012260109000A4Q003500070009000200068A0107002C00013Q0004433Q002C0001001275000700073Q00208A00080006000B2Q001C010800094Q001900073Q00090004433Q002A000100208A000C000B0009001226010E000C4Q0035000C000E000200068A010C002A00013Q0004433Q002A000100207B010C000B000D00261B010C002A0001000E0004433Q002A000100207B010C000B000F0026FD000C002A000100100004433Q002A00010030A8000B000F00100006C00007001E000100020004433Q001E00010006C000020014000100020004433Q001400012Q0066012Q00017Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D65000D4Q0012016Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200064Q000C00013Q0004433Q000C000100207B2Q013Q000400068A2Q01000C00013Q0004433Q000C000100207B2Q013Q00042Q00A22Q0100014Q0066012Q00017Q00033Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400094Q009B7Q00207B014Q00010006550001000700013Q0004433Q0007000100208A00013Q0002001226010300034Q00350001000300022Q00D6000100024Q0066012Q00017Q000D3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F707303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03093Q004D61676E697475646500314Q009B8Q0040012Q00010002000694012Q0006000100010004433Q000600012Q00CA000100014Q00D6000100023Q001275000100013Q00208A000100010002001226010300034Q00350001000300020006942Q01000E000100010004433Q000E00012Q00CA000200024Q00D6000200024Q00CA000200023Q0012B7000300043Q00202Q00030003000500122Q000400063Q00202Q0005000100074Q000500066Q00043Q000600044Q002D000100208A000900080002001226010B00084Q00350009000B000200069401090023000100010004433Q0023000100208A000900080002001226010B00094Q00350009000B000200069401090023000100010004433Q0023000100208A00090008000A001226010B000B4Q00350009000B000200068A0109002D00013Q0004433Q002D000100207B010A3Q000C00207B010B0009000C2Q00AA010A000A000B00207B010A000A000D00067F010A002D000100030004433Q002D00012Q00D50003000A4Q00D5000200093Q0006C000040016000100020004433Q001600012Q00D6000200024Q0066012Q00017Q00013Q0003063Q0043616E63656C000B4Q009B7Q00068A012Q000800013Q0004433Q000800012Q009B7Q00208A5Q00012Q00583Q000200012Q00CA8Q00A2017Q00CA8Q00A2012Q00014Q0066012Q00017Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99A93F03073Q00456E61626C6564030E3Q00497344657363656E64616E744F6603093Q00776F726B737061636503083Q00506F736974696F6E03093Q004D61676E697475646503043Q006D6174682Q033Q006D617803053Q0053702Q6564026Q00F03F027B14AE47E17A843F03093Q0054772Q656E496E666F2Q033Q006E657703043Q00456E756D030B3Q00456173696E675374796C6503063Q004C696E65617203063Q0043726561746503063Q00434672616D6503093Q00436F6D706C6574656403073Q00436F2Q6E65637403043Q00506C617900584Q009B7Q00207B014Q000100068A012Q005700013Q0004433Q005700010012753Q00023Q002054014Q000300122Q000100048Q000200016Q00013Q00206Q000500064Q000F000100010004433Q000F00012Q009B3Q00024Q00323Q000100010004435Q00012Q009B3Q00034Q0040012Q00010002000694012Q0016000100010004433Q001600012Q009B000100024Q00320001000100010004435Q00012Q009B000100044Q00402Q010001000200068A2Q01005400013Q0004433Q0054000100208A000200010006001275000400074Q003500020004000200069401020022000100010004433Q002200012Q009B000200024Q00320002000100010004435Q000100207B01023Q000800203A0003000100084Q00020002000300202Q0002000200094Q000300053Q00062Q0001002C000100030004433Q002C00012Q009B000300063Q00069401033Q000100010004435Q00012Q009B000300024Q00CB0003000100014Q000100053Q00122Q0003000A3Q00202Q00030003000B00122Q0004000A3Q00202Q00040004000B4Q000500013Q00202Q00050005000C00122Q0006000D6Q0004000600024Q00040002000400122Q0005000E6Q00030005000200122Q0004000F3Q00202Q0004000400104Q000500033Q00122Q000600113Q00202Q00060006001200202Q0006000600134Q0004000600024Q000500073Q00202Q0005000500144Q00078Q000800046Q00093Q000100202Q000A0001001500102Q00090015000A4Q0005000900024Q000500066Q000500063Q00202Q00050005001600202Q0005000500170006A300073Q000100012Q009B3Q00024Q000A0105000700014Q000500063Q00202Q0005000500184Q00050002000100046Q00012Q009B000200024Q00320002000100010004435Q00012Q0066012Q00013Q00018Q00034Q009B8Q00323Q000100012Q0066012Q00017Q00043Q00028Q0003043Q006D61746803053Q00666C2Q6F72026Q00F03F01083Q000E172Q01000700013Q0004433Q00070001001275000100023Q00207B2Q0100010003001016010200044Q00312Q01000200022Q00A22Q016Q0066012Q00017Q000B3Q00024Q00652QCD4103063Q00737472696E6703063Q00666F726D617403053Q00252E326642024Q0080842E4103053Q00252E32664D025Q00408F4003053Q00252E31664B03083Q00746F737472696E6703043Q006D61746803053Q00666C2Q6F7201223Q000E2F2Q01000900013Q0004433Q00090001001275000100023Q0020A100010001000300122Q000200043Q00202Q00033Q00014Q000100036Q00015Q0004433Q001A0001000E2F0105001200013Q0004433Q00120001001275000100023Q0020A100010001000300122Q000200063Q00202Q00033Q00054Q000100036Q00015Q0004433Q001A0001000E2F0107001A00013Q0004433Q001A0001001275000100023Q0020A100010001000300122Q000200083Q00202Q00033Q00074Q000100036Q00015Q001275000100093Q0012750002000A3Q00207B01020002000B2Q00D500036Q001C010200034Q00182Q016Q00202Q016Q0066012Q00017Q00113Q0003043Q0047656D732Q033Q0047656D03083Q004469616D6F6E647303073Q004469616D6F6E6403093Q0047656D7356616C7565030E3Q0046696E6446697273744368696C64030B3Q006C65616465727374617473030B3Q004C6561646572737461747303063Q006970616972732Q033Q0049734103083Q00496E7456616C7565030B3Q004E756D62657256616C756503163Q00446F75626C65436F6E73747261696E656456616C7565030B3Q004765744368696C6472656E03063Q00466F6C646572030D3Q00436F6E66696775726174696F6E03053Q004D6F64656C005E4Q00F93Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500012Q009B00015Q00208A000100010006001226010300074Q00350001000300020006942Q010011000100010004433Q001100012Q009B00015Q00208A000100010006001226010300084Q003500010003000200068A2Q01002E00013Q0004433Q002E0001001275000200094Q00D500036Q00A60102000200040004433Q002C000100208A0007000100062Q00D5000900064Q003500070009000200068A0107002C00013Q0004433Q002C000100208A00080007000A001226010A000B4Q00350008000A00020006940108002B000100010004433Q002B000100208A00080007000A001226010A000C4Q00350008000A00020006940108002B000100010004433Q002B000100208A00080007000A001226010A000D4Q00350008000A000200068A0108002C00013Q0004433Q002C00012Q00D6000700023Q0006C000020017000100020004433Q00170001001275000200094Q002500035Q00202Q00030003000E4Q000300046Q00023Q000400044Q0059000100208A00070006000A0012260109000F4Q003500070009000200069401070043000100010004433Q0043000100208A00070006000A001226010900104Q003500070009000200069401070043000100010004433Q0043000100208A00070006000A001226010900114Q003500070009000200068A0107005900013Q0004433Q00590001001275000700094Q00D500086Q00A60107000200090004433Q0057000100208A000C000600062Q00D5000E000B4Q0035000C000E000200068A010C005700013Q0004433Q0057000100208A000D000C000A001226010F000B4Q0035000D000F0002000694010D0056000100010004433Q0056000100208A000D000C000A001226010F000C4Q0035000D000F000200068A010D005700013Q0004433Q005700012Q00D6000C00023Q0006C000070047000100020004433Q004700010006C000020034000100020004433Q003400012Q00CA000200024Q00D6000200024Q0066012Q00017Q000D3Q00030C3Q0057616974466F724368696C6403073Q0052656D6F746573026Q001440030A3Q004C69667457656967687403133Q0053652Q6C537472656E677468526571756573742Q033Q00505650030D3Q00412Q7461636B412Q74656D707403043Q0053686F70030D3Q0052657175657374427579412Q6C030F3Q0052657175657374507572636861736503093Q0045717569704265737403043Q0050657473030B3Q005075726368617365452Q67003D4Q0087016Q00206Q000100122Q000200023Q00122Q000300038Q0003000200064Q003C00013Q0004433Q003C000100208A00013Q0001001236000300043Q00122Q000400036Q0001000400024Q000100013Q00202Q00013Q000100122Q000300053Q00122Q000400036Q0001000400024Q000100023Q00202Q00013Q000100122Q000300063Q00122Q000400036Q00010004000200062Q0002001B000100010004433Q001B000100208A000200010001001226010400073Q001226010500034Q00350002000500022Q00A2010200033Q00209901023Q000100122Q000400083Q00122Q000500036Q00020005000200062Q0002003100013Q0004433Q0031000100208A000300020001001226010500093Q001226010600034Q00350003000600022Q00A2010300043Q00208A0003000200010012260105000A3Q001226010600034Q00350003000600022Q00A2010300053Q00208A0003000200010012260105000B3Q001226010600034Q00350003000600022Q00A2010300063Q00208A00033Q00010012260105000C3Q001226010600034Q00350003000600020006550004003B000100030004433Q003B000100208A0004000300010012260106000D3Q001226010700034Q00350004000700022Q00A2010400074Q0066012Q00017Q00073Q0003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004865616C7468028Q00030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400154Q009B7Q00207B014Q0001000694012Q0006000100010004433Q000600012Q00CA000100014Q00D6000100023Q00208A00013Q0002001226010300034Q003500010003000200068A2Q01001000013Q0004433Q0010000100207B0102000100040026DD00020010000100050004433Q001000012Q00CA000200024Q00D6000200023Q00208A00023Q0006001226010400074Q001D000200044Q002001026Q0066012Q00017Q00083Q0003093Q00436861726163746572030E3Q00436861726163746572412Q64656403043Q0057616974030C3Q0057616974466F724368696C6403083Q0048756D616E6F6964026Q00144003093Q0057616C6B53702Q6564029Q00144Q009B7Q00207B014Q0001000694012Q0008000100010004433Q000800012Q009B7Q00207B014Q000200208A5Q00032Q0031012Q0002000200208A00013Q0004001226010300053Q001226010400064Q003500010004000200068A2Q01001300013Q0004433Q0013000100207B010200010007000E1701080013000100020004433Q0013000100207B0102000100072Q00A2010200014Q0066012Q00017Q000A3Q0003043Q007461736B03043Q0077616974029A5Q99C93F03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B6579030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564011B3Q001271000100013Q00202Q00010001000200122Q000200036Q00010002000100202Q00013Q000400122Q000300056Q00010003000200062Q0001001A00013Q0004433Q001A0001001275000200064Q004001020001000200207B0102000200070006940102001A000100010004433Q001A0001001275000200064Q004001020001000200207B0102000200080006940102001A000100010004433Q001A0001001275000200064Q004001020001000200207B0102000200090006940102001A000100010004433Q001A000100207B01020001000A2Q00A201026Q0066012Q00017Q00083Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103063Q00466F6C64657203063Q00737472696E6703053Q006D6174636803043Q004E616D6503053Q005E25642B2400183Q0012753Q00014Q002500015Q00202Q0001000100024Q000100029Q00000200044Q0013000100208A000500040003001226010700044Q003500050007000200068A0105001300013Q0004433Q00130001001275000500053Q00201F00050005000600202Q00060004000700122Q000700086Q00050007000200062Q0005001300013Q0004433Q001300012Q00D6000400023Q0006C03Q0006000100020004433Q000600012Q00CA8Q00D63Q00024Q0066012Q00017Q000A3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403083Q0041697264726F707303063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D6503073Q0041697264726F7003103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727400263Q0012E53Q00013Q00206Q000200122Q000200038Q0002000200064Q0008000100010004433Q000800012Q00CA000100014Q00D6000100023Q001275000100043Q00208A00023Q00052Q001C010200034Q001900013Q00030004433Q0021000100207B0106000500060026E700060021000100070004433Q002100012Q009B00066Q001C00060006000500069401060021000100010004433Q0021000100208A000600050002001226010800084Q00350006000800020006940106001C000100010004433Q001C000100208A0006000500090012260108000A4Q003500060008000200068A0106002100013Q0004433Q002100012Q00D5000700054Q00D5000800064Q0072000700033Q0006C00001000D000100020004433Q000D00012Q00CA000100014Q00D6000100024Q0066012Q00017Q00123Q0003043Q004E616D6503083Q0047656D4D6F64656C030B3Q0042696747656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903083Q004D6573685061727403083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C6173746963030C3Q005472616E73706172656E6379028Q0003043Q004E656F6E01503Q000694012Q0004000100010004433Q000400012Q003C00016Q00D6000100023Q00207B2Q013Q000100261B2Q010011000100020004433Q0011000100207B2Q013Q000100261B2Q010011000100030004433Q00110001001275000100043Q00207B2Q010001000500207B01023Q0001001226010300064Q00350001000300020004433Q001200012Q009F00016Q003C000100013Q0006942Q010016000100010004433Q001600012Q003C00026Q00D6000200023Q00208A00023Q0007001226010400084Q003500020004000200068A0102001D00013Q0004433Q001D00010006390002002000013Q0004433Q0020000100208A00023Q0009001226010400084Q003500020004000200068A0102004D00013Q0004433Q004D000100207B01030002000A00207B01030003000B2Q009B00045Q00067F01030029000100040004433Q002900012Q003C00036Q00D6000300023Q00208A0003000200070012260105000C4Q003500030005000200069401030031000100010004433Q0031000100208A000300020007001226010500084Q003500030005000200207B01040002000D0012750005000E3Q00207B01050005000D00207B01050005000F0006840104003A000100050004433Q003A000100207B01040002001000261B0104003B000100110004433Q003B00012Q009F00046Q003C000400013Q00202B01050002000D00122Q0006000E3Q00202Q00060006000D00202Q00060006001200062Q00050045000100060004433Q0045000100207B01050002001000261B01050046000100110004433Q004600012Q009F00056Q003C000500013Q0006550006004C000100030004433Q004C00010006390006004C000100040004433Q004C00012Q00D5000600054Q00D6000600024Q003C00036Q00D6000300024Q0066012Q00017Q000F3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403043Q006D61746803043Q006875676503103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03093Q004D61676E697475646500504Q009B7Q00207B014Q000100068A012Q000900013Q0004433Q0009000100208A00013Q0002001226010300034Q00350001000300020006942Q01000B000100010004433Q000B00012Q00CA000100014Q00D6000100023Q00207B2Q013Q00032Q00CA000200023Q001275000300043Q00207B0103000300052Q00E000046Q001F010500013Q00202Q00050005000200122Q000700066Q00050007000200062Q0005001B00013Q0004433Q001B0001001275000600073Q00207B0106000600082Q00D5000700044Q00D5000800054Q00E10006000800012Q009B000600024Q004001060001000200068A0106002400013Q0004433Q00240001001275000700073Q00207B0107000700082Q00D5000800044Q00D5000900064Q00E1000700090001001275000700094Q00D5000800044Q00A60107000200090004433Q004C0001001275000C00093Q00208A000D000B000A2Q001C010D000E4Q0019000C3Q000E0004433Q004A00012Q009B001100034Q00D5001200104Q003101110002000200068A0111004A00013Q0004433Q004A000100208A00110010000B0012260113000C4Q003500110013000200068A0111003900013Q0004433Q003900010006390011003C000100100004433Q003C000100208A00110010000D0012260113000C4Q003500110013000200068A0111004A00013Q0004433Q004A00012Q009B001200044Q001C0012001200110006940112004A000100010004433Q004A000100207B01120001000E00207B01130011000E2Q00AA01120012001300207B01120012000F00067F0112004A000100030004433Q004A00012Q00D5000300124Q00D5000200113Q0006C0000C002D000100020004433Q002D00010006C000070028000100020004433Q002800012Q00D6000200024Q0066012Q00017Q00073Q002Q033Q0049734103083Q00426173655061727403083Q004D6174657269616C03043Q00456E756D03043Q004E656F6E03063Q00697061697273030E3Q0047657444657363656E64616E747301243Q00208A00013Q0001001226010300024Q003500010003000200068A2Q01000E00013Q0004433Q000E000100207B2Q013Q0003001275000200043Q00207B01020002000300207B0102000200050006842Q01000C000100020004433Q000C00012Q00D63Q00024Q00CA000100014Q00D6000100023Q001275000100063Q00208A00023Q00072Q001C010200034Q001900013Q00030004433Q001F000100208A000600050001001226010800024Q003500060008000200068A0106001F00013Q0004433Q001F000100207B010600050003001275000700043Q00207B01070007000300207B0107000700050006840106001F000100070004433Q001F00012Q00D6000500023Q0006C000010013000100020004433Q001300012Q00CA000100014Q00D6000100024Q0066012Q00017Q000C3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403103Q00436F6E73756D61626C65537061776E7303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D65030B3Q005475726B65794D6F64656C03083Q00506F736974696F6E03093Q004D61676E697475646500334Q009B7Q00207B014Q00010006550001000700013Q0004433Q0007000100208A00013Q0002001226010300034Q00350001000300020006942Q01000B000100010004433Q000B00012Q00CA000200024Q00D6000200024Q009B000200013Q00208A000200020002001226010400044Q003500020004000200069401020013000100010004433Q001300012Q00CA000300034Q00D6000300024Q00CA000300033Q0012B7000400053Q00202Q00040004000600122Q000500073Q00202Q0006000200084Q000600076Q00053Q000700044Q002F000100207B010A000900090026E7000A002F0001000A0004433Q002F00012Q009B000A00024Q00D5000B00094Q0031010A0002000200068A010A002F00013Q0004433Q002F00012Q009B000B00034Q001C000B000B000A000694010B002F000100010004433Q002F000100207B010B0001000B00207B010C000A000B2Q00AA010B000B000C00207B010B000B000C00067F010B002F000100040004433Q002F00012Q00D50004000B4Q00D50003000A3Q0006C00005001B000100020004433Q001B00012Q00D6000300024Q0066012Q00017Q001B3Q0003043Q006D61746803043Q006875676503093Q004D696E486569676874030E3Q0046696E6446697273744368696C6403103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q004D6573685061727403043Q004E616D6503083Q0047656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D03083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C617374696303043Q004E656F6E030E3Q0052656E646572466964656C69747903073Q0050726563697365030C3Q005472616E73706172656E6379028Q0003083Q00506F736974696F6E03013Q005903093Q004D61676E697475646500674Q009B8Q0040012Q00010002000694012Q0006000100010004433Q000600012Q00CA000100014Q00D6000100024Q00CA000100013Q00128E010200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q00046Q001F010500023Q00202Q00050005000400122Q000700056Q00050007000200062Q0005001700013Q0004433Q00170001001275000600063Q00207B0106000600072Q00D5000700044Q00D5000800054Q00E10006000800012Q009B000600034Q004001060001000200068A0106002000013Q0004433Q00200001001275000700063Q00207B0107000700072Q00D5000800044Q00D5000900064Q00E1000700090001001275000700084Q00D5000800044Q00A60107000200090004433Q00630001001275000C00083Q00208A000D000B00092Q001C010D000E4Q0019000C3Q000E0004433Q0061000100208A00110010000A0012260113000B4Q003500110013000200068A0111006100013Q0004433Q0061000100207B01110010000C00261B011100380001000D0004433Q003800010012750011000E3Q00201F00110011000F00202Q00120010000C00122Q001300106Q00110013000200062Q0011006100013Q0004433Q0061000100207B011100100011001275001200123Q00207B01120012001100207B01120012001300068B0011003F000100120004433Q003F00012Q009F00116Q003C001100013Q00202B01120010001100122Q001300123Q00202Q00130013001100202Q00130013001400062Q0012004F000100130004433Q004F000100207B011200100015001275001300123Q00207B01130013001500207B0113001300160006840112004F000100130004433Q004F000100207B01120010001700261B01120050000100180004433Q005000012Q009F00126Q003C001200013Q00069401110055000100010004433Q0055000100068A0112006100013Q0004433Q0061000100207B01130010001900207B01130013001A00067F01030061000100130004433Q0061000100207B01130010001900207B01143Q00192Q00AA01130013001400207B01130013001B00067F01130061000100020004433Q006100012Q00D5000200134Q00D5000100103Q0006C0000C0029000100020004433Q002900010006C000070024000100020004433Q002400012Q00D6000100024Q0066012Q00017Q000C3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203083Q004B4F54484172656103043Q0052696E672Q033Q0049734103083Q00426173655061727403063Q00434672616D6503053Q004D6F64656C03083Q004765745069766F74003F3Q00126C012Q00013Q00206Q000200122Q000200038Q0002000200064Q000B00013Q0004433Q000B00010012753Q00013Q00207B014Q000300208A5Q0002001226010200044Q00353Q000200020006550001001000013Q0004433Q0010000100208A00013Q0002001226010300054Q003500010003000200065500020015000100010004433Q0015000100208A000200010002001226010400064Q003500020004000200068A0102003C00013Q0004433Q003C000100208A000300020002001226010500074Q003500030005000200068A0103002C00013Q0004433Q002C000100208A000400030008001226010600094Q003500040006000200068A0104002400013Q0004433Q0024000100207B01040003000A2Q00D6000400023Q0004433Q002C000100208A0004000300080012260106000B4Q003500040006000200068A0104002C00013Q0004433Q002C000100208A00040003000C2Q001D000400054Q002001045Q00208A000400020008001226010600094Q003500040006000200068A0104003400013Q0004433Q0034000100207B01040002000A2Q00D6000400023Q0004433Q003C000100208A0004000200080012260106000B4Q003500040006000200068A0104003C00013Q0004433Q003C000100208A00040002000C2Q001D000400054Q002001046Q00CA000300034Q00D6000300024Q0066012Q00017Q00133Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00737472696E6703053Q006C6F77657203043Q00542Q6F6C03043Q0066696E6403043Q004E616D65026Q00F03F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q00497341030C3Q00556E6571756970542Q6F6C7303043Q007461736B03043Q0077616974029A5Q99A93F03093Q004571756970542Q6F6C01574Q003D00015Q00202Q0001000100014Q00025Q00202Q00020002000200122Q000400036Q00020004000200062Q0003000B000100010004433Q000B000100208A000300010004001226010500054Q003500030005000200068A2Q01001100013Q0004433Q0011000100068A0102001100013Q0004433Q0011000100069401030013000100010004433Q001300012Q003C00046Q00D6000400023Q001275000400063Q00207B0104000400072Q00D500056Q003101040002000200208A000500010004001226010700084Q003500050007000200068A0105002A00013Q0004433Q002A0001001275000600063Q00206300060006000900122Q000700063Q00202Q00070007000700202Q00080005000A4Q0007000200024Q000800043Q00122Q0009000B6Q000A00016Q0006000A000200062Q0006002A00013Q0004433Q002A00012Q003C000600014Q00D6000600024Q00CA000600063Q0012110107000C3Q00202Q00080002000D4Q000800096Q00073Q000900044Q0043000100208A000C000B000E001226010E00084Q0035000C000E000200068A010C004300013Q0004433Q00430001001275000C00063Q002063000C000C000900122Q000D00063Q00202Q000D000D000700202Q000E000B000A4Q000D000200024Q000E00043Q00122Q000F000B6Q001000016Q000C0010000200062Q000C004300013Q0004433Q004300012Q00D50006000B3Q0004433Q004500010006C000070030000100020004433Q0030000100069401060049000100010004433Q004900012Q003C00076Q00D6000700023Q00068A0105005100013Q0004433Q0051000100208A00070003000F2Q008800070002000100122Q000700103Q00202Q00070007001100122Q000800126Q00070002000100208A0007000300132Q00D5000900064Q00E10007000900012Q003C000700014Q00D6000700024Q0066012Q00017Q00013Q00030D3Q0050726553696D756C6174696F6E00074Q009B8Q009B000100013Q00207B2Q01000100010006A300023Q000100012Q009B3Q00024Q00E13Q000200012Q0066012Q00013Q00013Q00133Q0003093Q0043686172616374657203073Q0067657467656E7603063Q004E6F636C697003063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C696465010003153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C756500334Q009B7Q00207B014Q0001000694012Q0005000100010004433Q000500012Q0066012Q00013Q001275000100024Q00402Q010001000200207B2Q010001000300068A2Q01001A00013Q0004433Q001A0001001275000100043Q00208A00023Q00052Q001C010200034Q001900013Q00030004433Q0018000100208A000600050006001226010800074Q003500060008000200068A0106001800013Q0004433Q0018000100207B01060005000800068A0106001800013Q0004433Q001800010030A80005000800090006C00001000F000100020004433Q000F000100208A00013Q000A0012260103000B4Q003500010003000200068A2Q01003200013Q0004433Q00320001001275000200024Q004001020001000200207B01020002000C00068A0102002800013Q0004433Q00280001001275000200024Q004001020001000200207B01020002000E0010460001000D0002001275000200024Q004001020001000200207B01020002000F00068A0102003200013Q0004433Q003200010030A8000100100011001275000200024Q004001020001000200207B0102000200130010460001001200022Q0066012Q00017Q00093Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030B3Q004368616E6765537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503073Q004A756D70696E6700143Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001300013Q0004433Q001300012Q009B7Q00207B014Q00030006550001000C00013Q0004433Q000C000100208A00013Q0004001226010300054Q003500010003000200068A2Q01001300013Q0004433Q0013000100208A000200010006001275000400073Q00207B01040004000800207B0104000400092Q00E10002000400012Q0066012Q00017Q00093Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E563203043Q007461736B03043Q0077616974027Q004003083Q0054656C65706F727403043Q0067616D6503073Q00506C616365496400153Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001400013Q0004433Q001400010012753Q00014Q0040012Q0001000200207B014Q0003000694012Q0014000100010004433Q001400010012753Q00043Q00200C014Q000500122Q000100068Q000200019Q0000206Q000700122Q000200083Q00202Q0002000200094Q000300018Q000300012Q0066012Q00017Q001C3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B657903093Q0043686172616374657203063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C6964650100030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00476574537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503083Q0046722Q6566612Q6C03163Q00412Q73656D626C794C696E65617256656C6F6369747903013Q0059026Q00344003073Q00566563746F72332Q033Q006E657703013Q0058026Q0049C003013Q005A026Q004EC0026Q0034C0004A3Q0012753Q00014Q0040012Q0001000200207B014Q0002000694012Q000B000100010004433Q000B00010012753Q00014Q0040012Q0001000200207B014Q0003000694012Q000B000100010004433Q000B00012Q0066012Q00014Q009B7Q00207B014Q0004000694012Q0010000100010004433Q001000012Q0066012Q00013Q001275000100053Q00208A00023Q00062Q001C010200034Q001900013Q00030004433Q001B000100208A000600050007001226010800084Q003500060008000200068A0106001B00013Q0004433Q001B00010030A800050009000A0006C000010015000100020004433Q0015000100208A00013Q000B0012AB0103000C6Q00010003000200202Q00023Q000D00122Q0004000E6Q00020004000200062Q0001004900013Q0004433Q0049000100068A0102004900013Q0004433Q0049000100208A00030002000F2Q0031010300020002001275000400103Q00207B01040004001100207B01040004001200068B00030032000100040004433Q0032000100207B01030001001300207B010300030014000E170115003C000100030004433Q003C0001001275000300163Q00201100030003001700202Q00040001001300202Q00040004001800122Q000500193Q00202Q00060001001300202Q00060006001A4Q00030006000200102Q0001001300030004433Q0049000100207B01030001001300207B0103000300140026FD000300490001001B0004433Q00490001001275000300163Q00201100030003001700202Q00040001001300202Q00040004001800122Q0005001C3Q00202Q00060001001300202Q00060006001A4Q00030006000200102Q0001001300032Q0066012Q00017Q000E3Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B657903063Q007475726B657903103Q005475726B657953702Q656456616C7565030A3Q0053702Q656456616C756503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564003B4Q009B7Q00207B014Q000100068A012Q003A00013Q0004433Q003A00010012753Q00023Q0020EA5Q000300122Q000100048Q0002000100124Q00058Q0001000200206Q000600122Q000100056Q00010001000200202Q00010001000700064Q0012000100010004433Q0012000100068A2Q013Q00013Q0004435Q00012Q00CA000200023Q00068A012Q002300013Q0004433Q0023000100068A2Q01002300013Q0004433Q002300012Q009B000300013Q0026E70003001F000100080004433Q001F0001001275000300054Q004001030001000200207B01030003000900063900020022000100030004433Q00220001001275000300054Q004001030001000200207B01020003000A0004433Q002C000100068A2Q01002900013Q0004433Q00290001001275000300054Q004001030001000200207B0102000300090004433Q002C0001001275000300054Q004001030001000200207B01020003000A2Q009B000300023Q00207B01030003000B00065500040033000100030004433Q0033000100208A00040003000C0012260106000D4Q003500040006000200068A01043Q00013Q0004435Q000100207B01050004000E00068B00053Q000100020004435Q00010010460004000E00020004435Q00012Q0066012Q00017Q002C3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B6579030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F7003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00506F736974696F6E03093Q004D61676E697475646503063Q007475726B65792Q033Q0067656D03073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A026Q00184003023Q006F7303053Q00636C6F636B026Q0008402Q0103043Q007461736B03053Q0064656C6179026Q004E40026Q00E03F03043Q00556E697403043Q004C65727003043Q006D61746803053Q00636C616D70026Q002440026Q00F03F03043Q004D6F7665026Q000C4003063Q00434672616D6503063Q006C2Q6F6B417403013Q0059026Q002040026Q00104003113Q0066697265746F756368696E74657265737403043Q007A65726F01E93Q0012D2000100016Q00010001000200202Q00010001000200122Q000200016Q00020001000200202Q00020002000300062Q0001000B000100010004433Q000B00010006940102000B000100010004433Q000B00012Q0066012Q00013Q00068A2Q01002300013Q0004433Q00230001001275000300014Q004001030001000200207B0103000300040006940103001C000100010004433Q001C0001001275000300014Q004001030001000200207B0103000300050006940103001C000100010004433Q001C0001001275000300014Q004001030001000200207B01030003000600068A0103002300013Q0004433Q002300012Q009B00036Q004A01030001000400068A0103002300013Q0004433Q0023000100068A0104002300013Q0004433Q002300012Q0066012Q00014Q009B000300013Q00207B01030003000700069401030028000100010004433Q002800012Q0066012Q00013Q00208A0004000300080012AB010600096Q00040006000200202Q00050003000A00122Q0007000B6Q00050007000200062Q0004003200013Q0004433Q0032000100069401050033000100010004433Q003300012Q0066012Q00013Q00068A2Q01003900013Q0004433Q003900012Q009B000600024Q00400106000100020006940106003A000100010004433Q003A00012Q00CA000600063Q00068A0102004000013Q0004433Q004000012Q009B000700034Q004001070001000200069401070041000100010004433Q004100012Q00CA000700074Q00CA000800093Q00068A0106005800013Q0004433Q0058000100068A0107005800013Q0004433Q0058000100207B010A0004000C002065010B0006000C4Q000A000A000B00202Q000A000A000D00202Q000B0004000C00202Q000C0007000C4Q000B000B000C00202Q000B000B000D00062Q000A00540001000B0004433Q005400012Q00D5000A00064Q009B000900044Q00D50008000A3Q0004433Q006300012Q00D5000A00074Q009B000900054Q00D50008000A3Q0004433Q0063000100068A0106005E00013Q0004433Q005E00012Q00D5000A00064Q009B000900044Q00D50008000A3Q0004433Q0063000100068A0107006300013Q0004433Q006300012Q00D5000A00074Q009B000900054Q00D50008000A3Q00068A0108006D00013Q0004433Q006D00012Q009B000A00053Q0006840109006B0001000A0004433Q006B0001001226010A000E3Q000694010A006C000100010004433Q006C0001001226010A000F4Q00A2010A00063Q00068A010800D600013Q0004433Q00D6000100207B010A0008000C00207B010B0004000C2Q00AA010A000A000B001275000B00103Q00207B010B000B001100207B010C000A0012001226010D00133Q00207B010E000A00142Q0035000B000E000200207B010C000B000D0026DD000C0098000100150004433Q009800012Q009B000D00073Q00068B000D0085000100080004433Q008500012Q00D5000D00083Q001275000E00163Q00207B010E000E00172Q0040010E000100022Q00A2010E00084Q00A2010D00073Q0004433Q009A0001001275000D00163Q002080000D000D00174Q000D000100024Q000E00086Q000D000D000E000E2Q0018009A0001000D0004433Q009A000100207E010900080019001275000D001A3Q00207B010D000D001B001226010E001C3Q0006A3000F3Q000100022Q00D53Q00094Q00D53Q00084Q00E1000D000F00012Q00CA000D000D4Q00A2010D00074Q0066012Q00013Q0004433Q009A00012Q00CA000D000D4Q00A2010D00073Q000E17011D00C60001000C0004433Q00C6000100207B010D000B001E2Q0083010E00093Q00202Q000E000E001F4Q0010000D3Q00122Q001100203Q00202Q00110011002100202Q00123Q002200122Q001300133Q00122Q001400236Q001100146Q000E3Q00022Q00A2010E00093Q00208A000E000500242Q009B001000094Q003C00116Q00E1000E00110001000E17012500C60001000C0004433Q00C60001001275000E00263Q00207B010E000E002700207B010F0004000C001275001000103Q00207B01100010001100207B01110008000C00207B01110011001200207B01120004000C00207B01120012002800207B01130008000C00207B0113001300142Q0013011000134Q0075010E3Q000200202Q000F0004002600202Q000F000F001F4Q0011000E3Q00122Q001200203Q00202Q00120012002100202Q00133Q002900122Q001400133Q00122Q001500236Q001200154Q002C010F3Q000200104600040026000F0026DD000C00E80001002A0004433Q00E80001001275000D002B3Q00068A010D00E800013Q0004433Q00E80001001275000D002B4Q00D9000E00046Q000F00083Q00122Q001000136Q000D0010000100122Q000D002B6Q000E00046Q000F00083Q00122Q001000236Q000D0010000100044Q00E800012Q00CA000A000A4Q004F010A00076Q000A00093Q00202Q000A000A001F00122Q000C00103Q00202Q000C000C002C00122Q000D00203Q00202Q000D000D002100202Q000E3Q002900122Q000F00133Q00122Q001000236Q000D00106Q000A3Q00024Q000A00093Q00202Q000A000500244Q000C00096Q000D8Q000A000D00012Q0066012Q00013Q00013Q00015Q00044Q009B8Q009B000100013Q00207E012Q000100012Q0066012Q00017Q00153Q0003083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03053Q00416C697665026Q00444003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00104003063Q004D6F7665546F03043Q007461736B03043Q0077616974029A5Q99B93F03043Q004D6F766503043Q007A65726F024D3Q00207B01023Q0001001275000300023Q00207B0103000300032Q00400103000100022Q00D5000400014Q004001040001000200068A0104003A00013Q0004433Q003A00012Q009B00045Q00207B01040004000400068A0104003A00013Q0004433Q003A0001001275000400023Q00207B0104000400032Q00400104000100022Q00AA0104000400030026FD0004003A000100050004433Q003A00012Q009B000400014Q00400104000100022Q009B000500023Q00207B01050005000600068A0105001D00013Q0004433Q001D00012Q009B000500023Q00207B01050005000600208A000500050007001226010700084Q003500050007000200068A0104003A00013Q0004433Q003A000100069401050022000100010004433Q002200010004433Q003A0001001275000600093Q00204D01060006000A00202Q00070002000B00202Q00080004000100202Q00080008000B4Q00070007000800122Q0008000C3Q00202Q00090002000D00202Q000A0004000100202Q000A000A000D4Q00090009000A2Q003500060009000200207B01060006000E0026DD000600320001000F0004433Q003200010004433Q003A000100208A0007000500102Q00CF000900026Q00070009000100122Q000700113Q00202Q00070007001200122Q000800136Q00070002000100044Q000400012Q009B000400023Q00207B01040004000600068A0104004300013Q0004433Q004300012Q009B000400023Q00207B01040004000600208A000400040007001226010600084Q003500040006000200068A0104004900013Q0004433Q0049000100208A000500040014001275000700093Q00207B0107000700152Q00E10005000700012Q00D5000500014Q0005000500014Q002001056Q0066012Q00017Q000C3Q0003063Q0069706169727303073Q0067657467656E76030E3Q0046696E6446697273744368696C642Q033Q0049734103083Q00426173655061727403063Q00434672616D6503083Q004765745069766F74028Q0003043Q007461736B03043Q0077616974029A5Q99B93F010003363Q001275000300014Q00D5000400014Q00A60103000200050004433Q00300001001275000800024Q00400108000100022Q001C0008000800020006940108000A000100010004433Q000A00010004433Q0032000100208A00083Q00032Q00D5000A00074Q00350008000A000200068A0108003000013Q0004433Q0030000100208A000900080004001226010B00054Q00350009000B000200068A0109001700013Q0004433Q0017000100207B01090008000600069401090019000100010004433Q0019000100208A0009000800072Q00310109000200022Q009B000A6Q00D5000B00093Q0006A3000C3Q000100012Q00D53Q00024Q0035000A000C0002000694010A0021000100010004433Q002100010004433Q00320001001226010A00084Q009B000B00013Q00067F010A00300001000B0004433Q00300001001275000B00024Q0040010B000100022Q001C000B000B000200068A010B003000013Q0004433Q00300001001275000B00093Q0020AC000B000B000A00122Q000C000B6Q000B0002000100202Q000A000A000B00044Q002200010006C000030004000100020004433Q00040001001275000300024Q004001030001000200207E01030002000C2Q0066012Q00013Q00013Q00013Q0003073Q0067657467656E7600063Q0012143Q00018Q000100024Q00019Q003Q00016Q00028Q00017Q00043Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E010003063Q004E6F636C6970000C3Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q000B00013Q0004433Q000B00010012753Q00014Q000E012Q0001000200304Q0002000300124Q00018Q0001000200304Q000400032Q0066012Q00017Q00043Q0003073Q0067657467656E7603083Q004175746F4C69667403043Q007461736B03053Q00737061776E010D3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000C00013Q0004433Q000C0001001275000100033Q00207B2Q01000100040006A300023Q000100032Q009B8Q009B3Q00014Q009B3Q00024Q00580001000200012Q0066012Q00013Q00013Q000F3Q0003053Q007063612Q6C03073Q0067657467656E7603083Q004175746F4C69667403093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03163Q0046696E6446697273744368696C64576869636849734103083Q0048756D616E6F696403093Q004571756970542Q6F6C030A3Q004669726553657276657203043Q007461736B03043Q0077616974029A5Q99B93F00333Q0012753Q00013Q0006A300013Q000100012Q009B8Q00583Q000200010012753Q00024Q0040012Q0001000200207B014Q000300068A012Q003200013Q0004433Q003200012Q009B3Q00013Q00200D014Q00044Q000100013Q00202Q00010001000500122Q000300066Q00010003000200064Q002100013Q0004433Q0021000100068A2Q01002100013Q0004433Q0021000100208A00023Q0007001226010400084Q003500020004000200069401020021000100010004433Q0021000100208A000300010009001226010500084Q003500030005000200068A0103002100013Q0004433Q0021000100207B01043Q000A00208A00040004000B2Q00D5000600034Q00E10004000600012Q009B000200023Q00068A0102002800013Q0004433Q002800012Q009B000200023Q00208A00020002000C2Q00580002000200010004433Q002C0001001275000200013Q0006A300030001000100012Q00D58Q00580002000200010012750002000D3Q00206D00020002000E00122Q0003000F6Q0002000200019Q0000044Q000400012Q0066012Q00013Q00023Q00083Q00030C3Q0053656E644B65794576656E7403043Q00456E756D03073Q004B6579436F64652Q033Q004F6E6503043Q0067616D6503043Q007461736B03043Q0077616974029A5Q99A93F00174Q00407Q00206Q00014Q000200013Q00122Q000300023Q00202Q00030003000300202Q0003000300044Q00045Q00122Q000500058Q0005000100124Q00063Q00207B014Q00070012262Q0100084Q00583Q000200012Q009B7Q00208A5Q00012Q003C00025Q001275000300023Q00207B01030003000300207B0103000300042Q003C00045Q001275000500054Q00E13Q000500012Q0066012Q00017Q00033Q0003153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03083Q004163746976617465000C4Q009B7Q00068A012Q000700013Q0004433Q000700012Q009B7Q00208A5Q0001001226010200024Q00353Q0002000200068A012Q000B00013Q0004433Q000B000100208A00013Q00032Q00580001000200012Q0066012Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F50756E636803043Q007461736B03053Q00737061776E010B3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000A00013Q0004433Q000A0001001275000100033Q00207B2Q01000100040006A300023Q000100012Q009B8Q00580001000200012Q0066012Q00013Q00013Q00083Q0003073Q0067657467656E7603093Q004175746F50756E6368030A3Q004669726553657276657203053Q0050756E6368026Q00F03F03043Q007461736B03043Q0077616974029A5Q99A93F00133Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001200013Q0004433Q001200012Q009B7Q00068A012Q000D00013Q0004433Q000D00012Q009B7Q00208A5Q0003001226010200043Q001226010300054Q00E13Q000300010012753Q00063Q00207B014Q00070012262Q0100084Q00583Q000200010004435Q00012Q0066012Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F53746F6D7003043Q007461736B03053Q00737061776E010B3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000A00013Q0004433Q000A0001001275000100033Q00207B2Q01000100040006A300023Q000100012Q009B8Q00580001000200012Q0066012Q00013Q00013Q00073Q0003073Q0067657467656E7603093Q004175746F53746F6D70030A3Q004669726553657276657203053Q0053746F6D7003043Q007461736B03043Q0077616974029A5Q99A93F00123Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001100013Q0004433Q001100012Q009B7Q00068A012Q000C00013Q0004433Q000C00012Q009B7Q00208A5Q0003001226010200044Q00E13Q000200010012753Q00053Q00207B014Q00060012262Q0100074Q00583Q000200010004435Q00012Q0066012Q00017Q00083Q0003073Q0067657467656E76030B3Q004175746F41697264726F70030F3Q004175746F54652Q7269746F72696573010003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01153Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q001400013Q0004433Q00140001001275000100014Q001D2Q010001000200302Q00010003000400122Q000100053Q00202Q0001000100064Q00028Q00010002000100122Q000100073Q00202Q0001000100080006A300023Q000100042Q009B3Q00014Q009B3Q00024Q009B8Q009B3Q00034Q00580001000200012Q0066012Q00013Q00013Q000D3Q0003073Q0067657467656E76030B3Q004175746F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F030C3Q004175746F47656D54772Q656E03063Q00434672616D652Q033Q006E6577028Q00026Q000840026Q002E402Q01029A5Q99C93F003D3Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q003C00013Q0004433Q003C00010012753Q00033Q002036014Q000400122Q000100058Q000200019Q006Q000100024Q000100016Q00010001000200062Q00013Q00013Q0004435Q000100068A01023Q00013Q0004435Q000100068A014Q00013Q0004435Q0001001275000300014Q004001030001000200207B01030003000600069401033Q000100010004435Q000100207B0103000200070012A0000400073Q00202Q00040004000800122Q000500093Q00122Q0006000A3Q00122Q000700096Q0004000700024Q00030003000400104Q0007000300122Q000300033Q00202Q0003000300040012260104000B4Q008F0003000200014Q000300023Q00202Q00030001000C4Q000300036Q0003000100024Q00048Q00040001000200062Q00033Q00013Q0004435Q000100068A01043Q00013Q0004435Q0001001275000500073Q00207B010500050008001226010600093Q0012260107000A3Q001226010800094Q00350005000800022Q002D010500030005001046000400070005001265000500033Q00202Q00050005000400122Q0006000D6Q00050002000100046Q00012Q0066012Q00017Q00073Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F72696573030C3Q004175746F47656D4272696E670100030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01133Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q001200013Q0004433Q001200012Q009B00016Q005A2Q010001000100122Q000100016Q00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100063Q00202Q0001000100070006A300023Q000100012Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q000A3Q0003023Q00543103023Q00543203023Q00543303023Q00543403023Q00543503093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0054652Q7269746F72696573030F3Q004175746F54652Q7269746F72696573001A4Q00F93Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q00050001001275000100063Q00208A000100010007001226010300084Q003500010003000200068A2Q01001200013Q0004433Q00120001001275000100063Q00207B2Q010001000800208A000100010007001226010300094Q003500010003000200068A2Q01001900013Q0004433Q001900012Q009B00026Q00D5000300014Q00D500045Q0012260105000A4Q00E10002000500012Q0066012Q00017Q00063Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01123Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q001100013Q0004433Q00110001001275000100033Q00207B2Q01000100042Q009B00026Q0058000100020001001275000100053Q00207B2Q01000100060006A300023Q000100042Q009B3Q00014Q009B3Q00024Q009B3Q00034Q009B8Q00580001000200012Q0066012Q00013Q00013Q00133Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00506172656E7403083Q00506F736974696F6E03093Q004D61676E6974756465026Q00144003063Q004D6F7665546F029A5Q99C93F03043Q004D6F766503073Q00566563746F723303043Q007A65726F026Q002E402Q01004C3Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q004B00013Q0004433Q004B00010012753Q00033Q0020215Q000400122Q000100058Q000200019Q0000206Q00064Q000100016Q00010001000200062Q0002001200013Q0004433Q0012000100208A00023Q0007001226010400084Q00350002000400022Q009B000300024Q004A01030001000400068A01033Q00013Q0004435Q000100068A01043Q00013Q0004435Q000100068A2Q013Q00013Q0004435Q000100068A01023Q00013Q0004435Q0001001275000500014Q004001050001000200207B01050005000200068A0105003200013Q0004433Q0032000100207B01050003000900068A0105003200013Q0004433Q0032000100207B01050001000A00207B01060004000A2Q00AA01050005000600207B01050005000B000E17010C0032000100050004433Q0032000100208A00050002000D00207B01070004000A2Q00E1000500070001001265000500033Q00202Q00050005000400122Q0006000E6Q00050002000100044Q001C0001001275000500014Q004001050001000200207B01050005000200068A01053Q00013Q0004435Q000100207B01050003000900068A01053Q00013Q0004435Q000100207B01050001000A00207B01060004000A2Q00AA01050005000600207B01050005000B0026DD00053Q0001000C0004435Q000100208A00050002000F001220000700103Q00202Q0007000700114Q00050007000100122Q000500033Q00202Q00050005000400122Q000600126Q0005000200014Q000500033Q00202Q00050003001300046Q00012Q0066012Q00017Q000D3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D4272696E67010003093Q0057616C6B53702Q6564030A3Q0053702Q656456616C7565030C3Q0057616C6B546F5475726B657903043Q004D6F766503073Q00566563746F723303043Q007A65726F01263Q0012E8000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004433Q000A000100208A000200010004001226010400054Q003500020004000200068A012Q001800013Q0004433Q001800012Q009B000300014Q0032000300010001001275000300014Q00400103000100020030A800030006000700068A0102002500013Q0004433Q00250001001275000300014Q004001030001000200207B0103000300090010460002000800030004433Q0025000100068A0102002500013Q0004433Q00250001001275000300014Q004001030001000200207B01030003000A00069401030025000100010004433Q0025000100208A00030002000B0012F00005000C3Q00202Q00050005000D4Q0003000500014Q000300023Q00102Q0002000800032Q0066012Q00017Q000F3Q0003073Q0067657467656E76030C3Q0057616C6B546F5475726B657903093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D4272696E67010003053Q007461626C6503053Q00636C65617203093Q0057616C6B53702Q656403103Q005475726B657953702Q656456616C7565030B3Q004175746F47656D57616C6B03043Q004D6F766503073Q00566563746F723303043Q007A65726F012A3Q0012E8000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004433Q000A000100208A000200010004001226010400054Q003500020004000200068A012Q001C00013Q0004433Q001C00012Q009B000300014Q0032000300010001001275000300014Q00400103000100020030A8000300060007001275000300083Q00207B0103000300092Q009B000400024Q005800030002000100068A0102002900013Q0004433Q00290001001275000300014Q004001030001000200207B01030003000B0010460002000A00030004433Q0029000100068A0102002900013Q0004433Q00290001001275000300014Q004001030001000200207B01030003000C00069401030029000100010004433Q0029000100208A00030002000D0012F00005000E3Q00202Q00050005000F4Q0003000500014Q000300033Q00102Q0002000A00032Q0066012Q00017Q00063Q0003073Q0067657467656E76030F3Q004175746F4461696C7952657761726403073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01163Q0012052Q0100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D000100010004433Q000D00012Q0066012Q00013Q001275000200053Q00207B0102000200060006A300033Q000100042Q009B3Q00014Q00D53Q00014Q009B8Q009B3Q00024Q00580002000200012Q0066012Q00013Q00013Q00053Q0003053Q007063612Q6C026Q00F03F025Q00C0724003043Q007461736B03043Q0077616974001C3Q0006A35Q000100032Q009B8Q009B3Q00014Q009B3Q00024Q00D500016Q00402Q010001000200068A2Q01001B00013Q0004433Q001B0001001275000100013Q0006A300020001000100012Q009B3Q00034Q00580001000200010012262Q0100023Q001226010200033Q001226010300023Q0004AB0001001A00012Q00D500056Q004001050001000200069401050015000100010004433Q001500010004433Q00040001001275000500043Q00207B010500050005001226010600024Q00580005000200010004F60001001000010004433Q000400012Q0066012Q00013Q00023Q00043Q0003073Q0067657467656E76030F3Q004175746F4461696C7952657761726403053Q00416C69766503073Q0073652Q73696F6E00123Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001000013Q0004433Q001000012Q009B7Q00207B014Q000300068A012Q001000013Q0004433Q001000012Q009B3Q00014Q009B000100023Q00207B2Q010001000400068B3Q000F000100010004433Q000F00012Q009F8Q003C3Q00014Q00D63Q00024Q0066012Q00017Q00053Q0003073Q0052656D6F74657303063Q0054696D657273030E3Q004765744576656E74526577617264030C3Q00496E766F6B65536572766572030C3Q005468616E6B73676976696E6700084Q003D016Q00206Q000100206Q000200206Q000300202Q00013Q000400122Q000300056Q0001000300016Q00017Q00023Q0003073Q0067657467656E76030A3Q0053702Q656456616C756501043Q001275000100014Q00402Q0100010002001046000100024Q0066012Q00017Q00023Q0003073Q0067657467656E7603103Q005475726B657953702Q656456616C756501043Q001275000100014Q00402Q0100010002001046000100024Q0066012Q00017Q00023Q0003073Q0067657467656E76030D3Q0047656D54772Q656E53702Q656401043Q001275000100014Q00402Q0100010002001046000100024Q0066012Q00017Q00113Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03063Q004E6F636C6970030C3Q004175746F47656D4272696E670100030F3Q004175746F54652Q7269746F72696573030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B657903093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403043Q007461736B03053Q00737061776E01383Q001275000100014Q00402Q0100010002001046000100023Q001275000100014Q00402Q0100010002001046000100033Q00068A012Q003700013Q0004433Q00370001001275000100014Q000E2Q010001000200302Q00010004000500122Q000100016Q00010001000200302Q000100060005001275000100014Q00402Q010001000200207B2Q01000100070006942Q010016000100010004433Q00160001001275000100014Q00402Q010001000200207B2Q0100010008001275000200014Q000E01020001000200302Q00020007000500122Q000200016Q00020001000200302Q00020008000500068A2Q01002D00013Q0004433Q002D00012Q009B00025Q00207B01020002000900065500030025000100020004433Q0025000100208A00030002000A0012260105000B4Q003500030005000200068A0103002D00013Q0004433Q002D000100208A00040003000C0012F00006000D3Q00202Q00060006000E4Q0004000600014Q000400013Q00102Q0003000F0004001275000200103Q00207B0102000200110006A300033Q000100062Q009B3Q00024Q009B3Q00034Q009B3Q00044Q009B3Q00054Q009B3Q00064Q009B3Q00074Q00580002000200012Q0066012Q00013Q00013Q00173Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03093Q0048656172746265617403043Q0057616974030B3Q004175746F41697264726F7003063Q00434672616D652Q033Q006E6577028Q00026Q00084003163Q00412Q73656D626C794C696E65617256656C6F6369747903073Q00566563746F723303173Q00412Q73656D626C79416E67756C617256656C6F6369747903043Q007461736B03043Q0077616974026Q002E402Q01029A5Q99C93F03083Q00506F736974696F6E030D3Q0047656D54772Q656E53702Q656403093Q004D61676E697475646503043Q00556E697403063Q004E6F636C6970012Q00783Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q006F00013Q0004433Q006F00012Q009B7Q00207B014Q000300208A5Q00042Q0031012Q000200022Q009B000100014Q00402Q010001000200068A2Q013Q00013Q0004435Q00012Q009B000200024Q00BD00020001000300122Q000400016Q00040001000200202Q00040004000500062Q0004004A00013Q0004433Q004A000100068A0102004A00013Q0004433Q004A000100068A0103004A00013Q0004433Q004A000100207B010400030006001233010500063Q00202Q00050005000700122Q000600083Q00122Q000700093Q00122Q000800086Q0005000800024Q00040004000500102Q00010006000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000A000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000C000400122Q0004000D3Q00202Q00040004000E00122Q0005000F6Q0004000200014Q000400033Q00202Q0004000200104Q000400046Q0004000100024Q000500016Q00050001000200062Q00043Q00013Q0004435Q000100068A01053Q00013Q0004435Q0001001275000600063Q00207B010600060007001226010700083Q001226010800093Q001226010900084Q00350006000900022Q002D0106000400060010460005000600060012650006000D3Q00202Q00060006000E00122Q000700116Q00060002000100046Q00012Q009B000400054Q004001040001000200068A01043Q00013Q0004435Q000100207B0105000400120020390106000100124Q00050005000600122Q000600016Q00060001000200202Q0006000600134Q000600063Q00202Q00070005001400062Q0007005B000100060004433Q005B000100207B0107000400060010460001000600070004433Q0060000100207B01070001000600207B0108000500152Q002D0108000800062Q00B50007000700080010460001000600070012750007000B3Q00202700070007000700122Q000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000A000700122Q0007000B3Q00202Q00070007000700122Q000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000C000700046Q00010012753Q00014Q0040012Q0001000200207B014Q0002000694012Q0077000100010004433Q007700010012753Q00014Q0040012Q000100020030A83Q001600172Q0066012Q00017Q000E3Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03053Q00737061776E03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F726564010001273Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100012Q009B8Q00580001000200010004433Q00260001001275000100053Q00208A000100010006001226010300074Q003500010003000200068A2Q01002600013Q0004433Q00260001001275000100083Q001209010200053Q00202Q00020002000700202Q0002000200094Q000200036Q00013Q000300044Q0024000100208A00060005000A0012260108000B4Q003500060008000200068A0106002400013Q0004433Q0024000100208A0006000500060012260108000C4Q003500060008000200068A0106002400013Q0004433Q0024000100207B01060005000C0030A80006000D000E0006C000010018000100020004433Q001800012Q0066012Q00013Q00013Q00143Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403063Q00434672616D652Q033Q006E6577028Q00026Q001AC0026Q001EC003083Q00416E63686F7265642Q0100343Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q003300013Q0004433Q003300010012753Q00033Q0020175Q000400122Q000100058Q000200019Q006Q0001000200066Q00013Q0004435Q0001001275000100063Q00208A000100010007001226010300084Q003500010003000200068A2Q013Q00013Q0004435Q0001001275000100093Q001209010200063Q00202Q00020002000800202Q00020002000A4Q000200036Q00013Q000300044Q0030000100208A00060005000B0012260108000C4Q003500060008000200068A0106003000013Q0004433Q0030000100208A0006000500070012260108000D4Q003500060008000200068A0106003000013Q0004433Q0030000100207B01060005000D00207B01073Q000E0012750008000E3Q00207B01080008000F001226010900103Q001226010A00113Q001226010B00124Q00350008000B00022Q002D0107000700080010460006000E000700207B01060005000D0030A80006001300140006C00001001A000100020004433Q001A00010004435Q00012Q0066012Q00017Q00043Q0003073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303043Q007461736B03053Q00737061776E010C3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100022Q009B8Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q001B3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q0052696768744C6F7765724C656703103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E657703083Q00506F736974696F6E03073Q00566563746F7233026Q002E40027Q0040028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004D6F7665546F00723Q0012E53Q00013Q00206Q000200122Q000200038Q0002000200064Q0007000100010004433Q000700012Q0066012Q00014Q00CA000100013Q001211010200043Q00202Q00033Q00054Q000300046Q00023Q000400044Q0023000100208A000700060006001226010900074Q003500070009000200068A0107002300013Q0004433Q0023000100208A000700060002001226010900084Q003500070009000200063900010020000100070004433Q0020000100208A000700060002001226010900094Q003500070009000200063900010020000100070004433Q0020000100208A00070006000A0012260109000B4Q00350007000900022Q00D5000100073Q00068A2Q01002300013Q0004433Q002300010004433Q002500010006C00002000D000100020004433Q000D00012Q009B00026Q004001020001000200068A0102003B00013Q0004433Q003B000100068A2Q01003B00013Q0004433Q003B00010012750003000C3Q0020AD00030003000D00202Q00040001000E00122Q0005000F3Q00202Q00050005000D00122Q000600103Q00122Q000700113Q00122Q000800126Q0005000800024Q0004000400054Q00030002000200102Q0002000C000300122Q000300133Q00202Q00030003001400122Q000400156Q000300020001001275000300164Q004001030001000200207B01030003001700068A0103007100013Q0004433Q00710001001275000300133Q00203400030003001400122Q000400156Q0003000200014Q000300013Q00202Q00030003001800062Q0004004B000100030004433Q004B000100208A0004000300190012260106001A4Q00350004000600022Q00CA000500053Q001211010600043Q00202Q00073Q00054Q000700086Q00063Q000800044Q0067000100208A000B000A0006001226010D00074Q0035000B000D000200068A010B006700013Q0004433Q0067000100208A000B000A0002001226010D00084Q0035000B000D0002000639000500640001000B0004433Q0064000100208A000B000A0002001226010D00094Q0035000B000D0002000639000500640001000B0004433Q0064000100208A000B000A000A001226010D000B4Q0035000B000D00022Q00D50005000B3Q00068A0105006700013Q0004433Q006700010004433Q006900010006C000060051000100020004433Q0051000100068A0104003B00013Q0004433Q003B000100068A0105003B00013Q0004433Q003B000100208A00060004001B00207B01080005000E2Q00E10006000800010004433Q003B00012Q0066012Q00017Q00063Q0003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C030A3Q0057616C6B546F426F2Q73010003043Q007461736B03053Q00737061776E010E3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000D00013Q0004433Q000D0001001275000100014Q00402Q01000100020030A8000100030004001275000100053Q00207B2Q01000100060006A300023Q000100012Q009B8Q00580001000200012Q0066012Q00013Q00013Q00153Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03043Q007461736B03043Q0077616974027B14AE47E17A843F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E6577028Q00026Q000C4003083Q0056656C6F6369747903073Q00566563746F723300413Q0012E53Q00013Q00206Q000200122Q000200038Q0002000200064Q0007000100010004433Q000700012Q0066012Q00013Q001275000100044Q00402Q010001000200207B2Q010001000500068A2Q01004000013Q0004433Q00400001001275000100063Q0020472Q010001000700122Q000200086Q0001000200014Q00018Q0001000100024Q000200023Q00122Q000300093Q00202Q00043Q000A4Q000400056Q00033Q000500044Q0029000100208A00080007000B001226010A000C4Q00350008000A000200068A0108002900013Q0004433Q0029000100208A000800070002001226010A000D4Q00350008000A000200063900020026000100080004433Q0026000100208A00080007000E001226010A000F4Q00350008000A00022Q00D5000200083Q00068A0102002900013Q0004433Q002900010004433Q002B00010006C000030018000100020004433Q0018000100068A2Q01000700013Q0004433Q0007000100068A0102000700013Q0004433Q0007000100207B010300020010001206010400103Q00202Q00040004001100122Q000500123Q00122Q000600123Q00122Q000700136Q0004000700024Q00030003000400102Q00010010000300122Q000300153Q00202Q00030003001100122Q000400123Q00122Q000500123Q00122Q000600126Q00030006000200102Q00010014000300044Q000700012Q0066012Q00017Q00023Q0003073Q0067657467656E7603103Q0053656C6563746564452Q67496E64657801043Q001275000100014Q00402Q0100010002001046000100024Q0066012Q00017Q00043Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q6703043Q007461736B03053Q00737061776E010D3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000C00013Q0004433Q000C0001001275000100033Q00207B2Q01000100040006A300023Q000100032Q009B8Q009B3Q00014Q009B3Q00024Q00580001000200012Q0066012Q00013Q00013Q000B3Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q003000013Q0004433Q003000012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002A00013Q0004433Q002A0001001275000100014Q00402Q010001000200207B2Q01000100070006942Q010023000100010004433Q002300010012262Q0100083Q001275000200093Q00207B01020002000A0006A300033Q000100022Q00D58Q00D53Q00014Q00580002000200012Q00D400015Q001275000100093Q0020EC00010001000B4Q000200026Q0001000200019Q0000046Q00012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012753Q00013Q0006A300013Q000100022Q009B8Q009B3Q00014Q00583Q000200012Q0066012Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00084003073Q0049736C616E647300074Q001E7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030F3Q004175746F48617463685065724F6E6503043Q007461736B03053Q00737061776E010D3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000C00013Q0004433Q000C0001001275000100033Q00207B2Q01000100040006A300023Q000100032Q009B8Q009B3Q00014Q009B3Q00024Q00580001000200012Q0066012Q00013Q00013Q000B3Q0003073Q0067657467656E76030F3Q004175746F48617463685065724F6E65030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q003000013Q0004433Q003000012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002A00013Q0004433Q002A0001001275000100014Q00402Q010001000200207B2Q01000100070006942Q010023000100010004433Q002300010012262Q0100083Q001275000200093Q00207B01020002000A0006A300033Q000100022Q00D58Q00D53Q00014Q00580002000200012Q00D400015Q001275000100093Q0020EC00010001000B4Q000200026Q0001000200019Q0000046Q00012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012753Q00013Q0006A300013Q000100022Q009B8Q009B3Q00014Q00583Q000200012Q0066012Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00F03F03073Q0049736C616E647300074Q001E7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00023Q0003053Q007063612Q6C2Q01000C3Q0012753Q00013Q0006A300013Q000100012Q009B8Q00A6012Q000200010006550002000A00013Q0004433Q000A000100261B2Q010009000100020004433Q000900012Q009F00026Q003C000200014Q00D6000200024Q0066012Q00013Q00013Q00043Q00030A3Q00506C617965724461746103083Q0047616D6570612Q7303053Q004F70656E3303053Q0056616C756500074Q0038016Q00206Q000100206Q000200206Q000300206Q00046Q00028Q00017Q00043Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q6703043Q007461736B03053Q00737061776E010C3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100022Q009B8Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q00093Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703043Q007461736B03053Q00737061776E03043Q007761697400283Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q002700013Q0004433Q002700012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002200013Q0004433Q00220001001275000100073Q00207B2Q01000100080006A300023Q000100012Q00D58Q0058000100020001001275000100073Q00207B2Q01000100092Q00320001000100012Q00D47Q0004435Q00012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012753Q00013Q0006A300013Q000100012Q009B8Q00583Q000200012Q0066012Q00013Q00013Q00043Q00030C3Q00496E766F6B65536572766572026Q00F03F026Q00084003093Q0043756265576F726C6400074Q0070016Q00206Q000100122Q000200023Q00122Q000300033Q00122Q000400048Q000400016Q00017Q00163Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001503Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q004A00013Q0004433Q004A0001001275000100033Q00208A000100010004001226010300054Q003500010003000200068A2Q01002100013Q0004433Q00210001001275000100033Q00207300010001000500202Q00010001000400122Q000300066Q00010003000200062Q0001002100013Q0004433Q00210001001275000100033Q00206100010001000500202Q00010001000600202Q00010001000400122Q000300076Q00010003000200068A2Q01002100013Q0004433Q00210001001275000100033Q00207A2Q010001000500202Q00010001000600202Q00010001000700202Q00010001000400122Q000300086Q0001000300022Q009B00026Q004001020001000200068A0102003F00013Q0004433Q003F000100068A2Q01003F00013Q0004433Q003F000100208A0003000100090012260105000A4Q003500030005000200068A0103003000013Q0004433Q0030000100208A00030001000B2Q003101030002000200069401030031000100010004433Q0031000100207B01030001000C0012750004000C3Q00208001040004000D00122Q0005000E3Q00122Q0006000F3Q00122Q0007000E6Q0004000700024Q00040003000400102Q0002000C000400122Q000400103Q00202Q00040004001100122Q000500126Q00040002000100302Q00020013001400044Q0042000100068A0102004200013Q0004433Q004200010030A8000200130014001275000300103Q00207B0103000300150006A300043Q000100032Q009B3Q00014Q009B3Q00024Q009B3Q00034Q00580003000200010004433Q004F00012Q009B00016Q00402Q010001000200068A2Q01004F00013Q0004433Q004F00010030A80001001300162Q0066012Q00013Q00013Q00083Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001F00013Q0004433Q001F00012Q009B7Q002090014Q000300206Q00046Q000200016Q00013Q00064Q0017000100010004433Q001700012Q009B3Q00023Q00208A5Q0005001226010200064Q00353Q0002000200068A012Q001700013Q0004433Q001700012Q009B3Q00023Q00207B014Q000600208A5Q0005001226010200074Q00353Q0002000200068A012Q001D00013Q0004433Q001D0001001275000100083Q0006A300023Q000100012Q00D58Q00580001000200012Q00D47Q0004435Q00012Q0066012Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q009B7Q00208A5Q00012Q00583Q000200012Q0066012Q00017Q00043Q0003073Q0067657467656E76030E3Q004175746F4275795765696768747303043Q007461736B03053Q00737061776E010C3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100022Q009B8Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q000A3Q0003073Q0067657467656E76030E3Q004175746F42757957656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q002800013Q0004433Q002800012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002200013Q0004433Q00220001001275000100073Q00207B2Q01000100080006A300023Q000100012Q00D58Q0058000100020001001275000100073Q00206D00010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012753Q00013Q0006A300013Q000100012Q009B8Q00583Q000200012Q0066012Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q0057656967687403073Q0049736C616E647300064Q002A016Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q00043Q0003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E010C3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100022Q009B8Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q000A3Q0003073Q0067657467656E76030A3Q004175746F427579444E41030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q002800013Q0004433Q002800012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002200013Q0004433Q00220001001275000100073Q00207B2Q01000100080006A300023Q000100012Q00D58Q0058000100020001001275000100073Q00206D00010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q0066012Q00013Q00013Q00063Q00026Q00F03F026Q005E4003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E00133Q001226012Q00013Q0012262Q0100023Q001226010200013Q0004AB3Q00120001001275000400034Q004001040001000200207B0104000400040006940104000A000100010004433Q000A00010004433Q00120001001275000400053Q00207B0104000400060006A300053Q000100022Q009B8Q00D53Q00034Q00580004000200012Q00D400035Q0004F63Q000400012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012753Q00013Q0006A300013Q000100022Q009B8Q009B3Q00014Q00583Q000200012Q0066012Q00013Q00013Q00033Q00030C3Q00496E766F6B655365727665722Q033Q00444E4103073Q0049736C616E647300074Q001E7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E010C3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100022Q009B8Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q000A3Q0003073Q0067657467656E76030D3Q004175746F427579426F64696573030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q002800013Q0004433Q002800012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002200013Q0004433Q00220001001275000100073Q00207B2Q01000100080006A300023Q000100012Q00D58Q0058000100020001001275000100073Q00206D00010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q0066012Q00013Q00013Q00073Q00027Q0040025Q00802Q40026Q00F03F03073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E00133Q001226012Q00013Q0012262Q0100023Q001226010200033Q0004AB3Q00120001001275000400044Q004001040001000200207B0104000400050006940104000A000100010004433Q000A00010004433Q00120001001275000400063Q00207B0104000400070006A300053Q000100022Q009B8Q00D53Q00034Q00580004000200012Q00D400035Q0004F63Q000400012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012753Q00013Q0006A300013Q000100022Q009B8Q009B3Q00014Q00583Q000200012Q0066012Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572030B3Q00426F64795570677261646503073Q0049736C616E647300074Q001E7Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B65745765696768747303043Q007461736B03053Q00737061776E010C3Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q000B00013Q0004433Q000B0001001275000100033Q00207B2Q01000100040006A300023Q000100022Q009B8Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q000A3Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B657457656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q002800013Q0004433Q002800012Q009B7Q000694012Q001B000100010004433Q001B00012Q009B3Q00013Q00208A5Q0003001226010200044Q00353Q0002000200068A012Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020735Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q0004433Q001B00012Q009B3Q00013Q0020615Q000400206Q000500206Q000300122Q000200068Q0002000200068A012Q002200013Q0004433Q00220001001275000100073Q00207B2Q01000100080006A300023Q000100012Q00D58Q0058000100020001001275000100073Q00206D00010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q0066012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012753Q00013Q0006A300013Q000100012Q009B8Q00583Q000200012Q0066012Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q00576569676874030B3Q0053757065726D61726B657400064Q002A016Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q001A3Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B657403053Q0053686F707303093Q0052696E67417265617303063Q00536572766572030B3Q0052616E676553797374656D030F3Q0053757065726D61726B657453652Q6C03043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001633Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q005D00013Q0004433Q005D0001001275000100033Q00208A000100010004001226010300054Q00350001000300020006550002000E000100010004433Q000E000100208A000200010004001226010400064Q00350002000400022Q00CA000300033Q00068A0102003400013Q0004433Q0034000100208A000400020004001226010600074Q003500040006000200069401040019000100010004433Q0019000100208A000400020004001226010600084Q003500040006000200065500050029000100040004433Q0029000100208A000500040004001226010700094Q003500050007000200069401050029000100010004433Q0029000100208A0005000400040012260107000A4Q003500050007000200068A0105002900013Q0004433Q0029000100207B01050004000A00208A000500050004001226010700094Q003500050007000200068A0105003400013Q0004433Q0034000100208A0006000500040012260108000B4Q003500060008000200063900030034000100060004433Q0034000100208A0006000500040012260108000C4Q00350006000800022Q00D5000300064Q009B00046Q004001040001000200068A0104005200013Q0004433Q0052000100068A0103005200013Q0004433Q0052000100208A00050003000D0012260107000E4Q003500050007000200068A0105004300013Q0004433Q0043000100208A00050003000F2Q003101050002000200069401050044000100010004433Q0044000100207B010500030010001275000600103Q00208001060006001100122Q000700123Q00122Q000800133Q00122Q000900126Q0006000900024Q00060005000600102Q00040010000600122Q000600143Q00202Q00060006001500122Q000700166Q00060002000100302Q00040017001800044Q0055000100068A0104005500013Q0004433Q005500010030A8000400170018001275000500143Q00207B0105000500190006A300063Q000100032Q009B3Q00014Q009B3Q00024Q009B3Q00034Q00580005000200010004433Q006200012Q009B00016Q00402Q010001000200068A2Q01006200013Q0004433Q006200010030A800010017001A2Q0066012Q00013Q00013Q00083Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001F00013Q0004433Q001F00012Q009B7Q002090014Q000300206Q00046Q000200016Q00013Q00064Q0017000100010004433Q001700012Q009B3Q00023Q00208A5Q0005001226010200064Q00353Q0002000200068A012Q001700013Q0004433Q001700012Q009B3Q00023Q00207B014Q000600208A5Q0005001226010200074Q00353Q0002000200068A012Q001D00013Q0004433Q001D0001001275000100083Q0006A300023Q000100012Q00D58Q00580001000200012Q00D47Q0004435Q00012Q0066012Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q009B7Q00208A5Q00012Q00583Q000200012Q0066012Q00017Q00073Q0003073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D4272696E670100030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01133Q001275000100014Q00402Q0100010002001046000100023Q00068A012Q001200013Q0004433Q001200012Q009B00016Q005A2Q010001000100122Q000100016Q00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100063Q00202Q0001000100070006A300023Q000100012Q009B3Q00014Q00580001000200012Q0066012Q00013Q00013Q00093Q0003023Q00543103023Q00543203023Q00543303093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B6574030B3Q0054652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573001B4Q00E93Q00033Q00122Q000100013Q00122Q000200023Q00122Q000300038Q00030001001275000100043Q00208A000100010005001226010300064Q00350001000300020006550002000E000100010004433Q000E000100208A000200010005001226010400074Q003500020004000200065500030013000100020004433Q0013000100208A000300020005001226010500084Q003500030005000200068A0103001A00013Q0004433Q001A00012Q009B00046Q00D5000500034Q00D500065Q001226010700094Q00E10004000700012Q0066012Q00017Q00043Q0003073Q00456E61626C656403073Q0067657467656E76030B3Q0057616C6B546F4375626573010001084Q009B00015Q001046000100013Q00068A012Q000700013Q0004433Q00070001001275000100024Q00402Q01000100020030A80001000300042Q0066012Q00017Q00013Q0003053Q0053702Q656401034Q009B00015Q001046000100014Q0066012Q00017Q00053Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E7303093Q0043756265576F726C6403063Q0047726F756E6400103Q0012753Q00013Q00208A5Q0002001226010200034Q00353Q000200020006550001000900013Q0004433Q0009000100208A00013Q0002001226010300044Q00350001000300020006550002000E000100010004433Q000E000100208A000200010002001226010400054Q00350002000400022Q00D6000200024Q0066012Q00017Q00083Q00030F3Q0067657447726F756E64466F6C6465722Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903043Q0053697A65027Q0040001E4Q009B7Q00207B014Q00012Q0040012Q00010002000694012Q0007000100010004433Q000700012Q00CA000100014Q00D6000100023Q00208A00013Q0002001226010300034Q003500010003000200068A2Q01000E00013Q0004433Q000E00010006390001001200013Q0004433Q0012000100208A00013Q0004001226010300034Q003C000400014Q00350001000400020006942Q010016000100010004433Q001600012Q00CA000200024Q00D6000200023Q00207B01020001000500207B01020002000600207B01030001000700207B0103000300060020220103000300082Q00B50002000200032Q00D6000200024Q0066012Q00017Q00133Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F7073030C3Q0067657447726F756E64546F7003063Q00697061697273030B3Q004765744368696C6472656E03093Q00626C61636B6C697374030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03013Q0059027Q00C003093Q006D617848656967687403053Q007461626C6503063Q00696E7365727403043Q006375626503043Q0070617274003B3Q0012AE3Q00013Q00206Q000200122Q000200038Q000200024Q00015Q00202Q0001000100044Q00010001000200064Q000B00013Q0004433Q000B00010006942Q01000D000100010004433Q000D00012Q00E000026Q00D6000200024Q00E000025Q001211010300053Q00202Q00043Q00064Q000400056Q00033Q000500044Q003700012Q009B00085Q00207B0108000800072Q001C00080008000700069401080037000100010004433Q0037000100208A000800070002001226010A00084Q00350008000A000200069401080025000100010004433Q0025000100208A000800070002001226010A00094Q00350008000A000200069401080025000100010004433Q0025000100208A00080007000A001226010A000B4Q00350008000A000200068A0108003700013Q0004433Q0037000100207B01090008000C00207B01090009000D2Q00AA010900090001000E2F010E0037000100090004433Q003700012Q009B000A5Q00207B010A000A000F0006D3000900370001000A0004433Q00370001001275000A00103Q00207B010A000A00112Q00D5000B00024Q00E0000C3Q0002001046000C00120007001046000C001300082Q00E1000A000C00010006C000030013000100020004433Q001300012Q00D6000200024Q0066012Q00017Q00063Q0003073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465020D3Q0012E2000200013Q00202Q00020002000200202Q00033Q000300202Q0004000100034Q00030003000400122Q000400043Q00202Q00053Q000500202Q0006000100054Q0005000500064Q00020005000200202Q0002000200064Q000200028Q00017Q00083Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303073Q0073652Q73696F6E026Q00F03F03073Q00456E61626C6564010003043Q007461736B03053Q00737061776E011A3Q0012052Q0100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D000100010004433Q000D00012Q0066012Q00014Q009B000200013Q0030A8000200050006001275000200073Q00207B0102000200080006A300033Q000100062Q009B3Q00024Q00D53Q00014Q009B8Q009B3Q00034Q009B3Q00044Q009B3Q00054Q00580002000200012Q0066012Q00013Q00013Q00203Q0003043Q006D61746803043Q0068756765028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q006375626503063Q00506172656E7403043Q007061727403063Q00697061697273030D3Q00676574466C2Q6F72437562657303083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03073Q0067657467656E76030D3Q0057616C6B437562654465627567030C3Q0067657447726F756E64546F7003053Q007072696E7403353Q005B57616C6B437562655D2074617267657420257320646973743D252E31662068656967687441626F766547726F756E643D252E316603063Q00666F726D617403043Q004E616D6503013Q005903063Q004D6F7665546F027Q0040026Q001840026Q00244003043Q004D6F766503073Q00566563746F723303043Q007A65726F00983Q001275000100013Q00207B2Q0100010002001226010200033Q0006A300033Q000100032Q009B8Q009B3Q00014Q009B3Q00023Q0006A300040001000100012Q009B3Q00024Q00D5000500034Q004001050001000200068A0105008800013Q0004433Q00880001001275000500043Q00203400050005000500122Q000600066Q0005000200014Q000500033Q00202Q00050005000700062Q00060018000100050004433Q0018000100208A000600050008001226010800094Q00350006000800022Q009B000700044Q004001070001000200068A0106001E00013Q0004433Q001E000100069401070020000100010004433Q002000012Q00CA7Q0004433Q0009000100068A012Q002B00013Q0004433Q002B000100207B01083Q000A00207B01080008000B00068A0108002A00013Q0004433Q002A000100207B01083Q000C00207B01080008000B0006940108002B000100010004433Q002B00012Q00CA7Q000694012Q0063000100010004433Q006300012Q00CA000800083Q00120A000900013Q00202Q00090009000200122Q000A000D6Q000B00023Q00202Q000B000B000E4Q000B00016Q000A3Q000C00044Q004000012Q009B000F00053Q00207B01100007000F00207B0111000E000C00207B01110011000F2Q0035000F0011000200067F010F0040000100090004433Q004000012Q00D50010000E4Q00D50009000F4Q00D5000800103Q0006C0000A0036000100020004433Q0036000100068A0108006300013Q0004433Q006300012Q00D5000A00084Q0056010B00093Q00122Q000C00103Q00202Q000C000C00114Q000C000100024Q0002000C6Q0001000B8Q000A3Q00122Q000A00126Q000A0001000200202Q000A000A001300062Q000A006300013Q0004433Q006300012Q009B000A00023Q00207B010A000A00142Q0040010A00010002000694010A0057000100010004433Q00570001001226010A00033Q001275000B00153Q001281010C00163Q00202Q000C000C001700202Q000E0008000A00202Q000E000E00184Q000F00093Q00202Q00100008000C00202Q00100010000F00202Q0010001000194Q00100010000A4Q000C00106Q000B3Q000100068A012Q008400013Q0004433Q008400012Q009B000800053Q00209A01090007000F00202Q000A3Q000C00202Q000A000A000F4Q0008000A000200202Q00090006001A00202Q000B3Q000C00202Q000B000B000F4Q0009000B000100202Q00090001001B00062Q00080078000100090004433Q007800012Q00D5000900083Q001248000A00103Q00202Q000A000A00114Q000A000100024Q0002000A6Q000100093Q00044Q00090001001275000900103Q00207B0109000900112Q00400109000100022Q00AA010900090002000E17011C0009000100090004433Q000900012Q00D5000900043Q002081000A3Q000A00122Q000B001D6Q0009000B00019Q0000044Q0009000100208A00080006001A00207B010A0007000F2Q00E10008000A00010004433Q000900012Q009B000500033Q00207B01050005000700068A0105009100013Q0004433Q009100012Q009B000500033Q00207B01050005000700208A000500050008001226010700094Q003500050007000200068A0105009700013Q0004433Q0097000100208A00060005001E0012750008001F3Q00207B0108000800202Q00E10006000800012Q0066012Q00013Q00023Q00043Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303053Q00416C69766503073Q0073652Q73696F6E00123Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001000013Q0004433Q001000012Q009B7Q00207B014Q000300068A012Q001000013Q0004433Q001000012Q009B3Q00014Q009B000100023Q00207B2Q010001000400068B3Q000F000100010004433Q000F00012Q009F8Q003C3Q00014Q00D63Q00024Q0066012Q00017Q000B3Q0003073Q0067657467656E76030D3Q0057616C6B43756265446562756703053Q007072696E74031E3Q005B57616C6B437562655D20736B692Q70696E6720737475636B206375626503043Q004E616D652Q033Q00666F7203013Q007303093Q00626C61636B6C6973742Q0103043Q007461736B03053Q0064656C617902173Q001275000200014Q004001020001000200207B01020002000200068A0102000C00013Q0004433Q000C0001001275000200033Q0012A5010300043Q00202Q00043Q000500122Q000500066Q000600013Q00122Q000700076Q0002000700012Q009B00025Q00207B01020002000800207E01023Q00090012750002000A3Q00207B01020002000B2Q00D5000300013Q0006A300043Q000100022Q009B8Q00D58Q00E10002000400012Q0066012Q00013Q00013Q00023Q0003093Q00626C61636B6C6973742Q00054Q00527Q00206Q00014Q000100013Q00204Q000100026Q00017Q00033Q0003073Q0067657467656E7603093Q0044697361626C65334403153Q00536574336452656E646572696E67456E61626C656401083Q001275000100014Q00402Q0100010002001046000100024Q009B00015Q00208A0001000100032Q007B00036Q00E10001000300012Q0066012Q00017Q00023Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E01043Q001275000100014Q00402Q0100010002001046000100024Q0066012Q00017Q00033Q00034Q0003053Q007063612Q6C2Q0100153Q001226012Q00013Q001275000100023Q0006A300023Q000100022Q00D58Q009B8Q005800010002000100261B012Q000A000100010004433Q000A00012Q003C000100014Q00D6000100023Q001275000100023Q000282000200014Q00A62Q010002000200065500030013000100010004433Q0013000100261B01020012000100030004433Q001200012Q009F00036Q003C000300014Q00D6000300024Q0066012Q00013Q00023Q00013Q00030F3Q00476574452Q726F724D652Q7361676500054Q003E012Q00013Q00206Q00016Q000200029Q006Q00017Q00083Q0003043Q0067616D65030A3Q004765745365727669636503073Q00436F7265477569030E3Q0046696E6446697273744368696C64030F3Q00526F626C6F7850726F6D7074477569030D3Q0070726F6D70744F7665726C617900030B3Q00452Q726F7250726F6D707400173Q0012753Q00013Q00208A5Q0002001226010200034Q00353Q0002000200208A00013Q0004001226010300054Q00350001000300020006550002000C000100010004433Q000C000100208A000200010004001226010400064Q003500020004000200261B01020013000100070004433Q0013000100208A000300020004001226010500084Q00350003000500020026E700030014000100070004433Q001400012Q009F00036Q003C000300014Q00D6000300024Q0066012Q00017Q00083Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203073Q0073652Q73696F6E026Q00F03F03043Q0062757379010003043Q007461736B03053Q00737061776E01193Q0012ED000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100032Q009B00025Q0030A8000200050006000694012Q000F000100010004433Q000F00012Q0066012Q00013Q001275000200073Q00207B0102000200080006A300033Q000100052Q009B3Q00014Q00D53Q00014Q009B8Q009B3Q00024Q009B3Q00034Q00580002000200012Q0066012Q00013Q00013Q00173Q0003043Q007461736B03043Q0077616974026Q00F03F030E3Q006973446973636F2Q6E656374656403043Q00627573792Q01028Q0003123Q0054656C65706F7274496E69744661696C656403073Q00436F2Q6E65637403113Q0071756575655F6F6E5F74656C65706F727403073Q0067657467656E7603123Q0056656C6F636974794C6F6164737472696E6703053Q007063612Q6C03023Q006F7303053Q00636C6F636B026Q002E40026Q00E03F030A3Q00446973636F2Q6E65637403043Q006D6174682Q033Q006D696E026Q000840026Q003E40012Q005C3Q0006A35Q000100032Q009B8Q009B3Q00014Q009B3Q00024Q00D500016Q00402Q010001000200068A2Q01005B00013Q0004433Q005B0001001275000100013Q00201200010001000200122Q000200036Q0001000200014Q000100023Q00202Q0001000100044Q00010001000200062Q0001000400013Q0004433Q000400012Q009B000100023Q00207B2Q01000100050006942Q010004000100010004433Q000400012Q009B000100023Q0030A80001000500060012262Q0100074Q00D500026Q004001020001000200068A0102005800013Q0004433Q005800010020912Q01000100032Q003C00026Q009B000300033Q00207B01030003000800208A0003000300090006A300050001000100012Q00D53Q00024Q00350003000500020012750004000A3Q00068A0104003200013Q0004433Q003200010012750004000B4Q004001040001000200207B01040004000C00068A0104003200013Q0004433Q003200010012750004000D3Q0012790105000A3Q00122Q0006000B6Q00060001000200202Q00060006000C4Q0004000600010012750004000D3Q0006A300050002000100032Q009B3Q00034Q009B3Q00024Q009B3Q00044Q00580004000200010012750004000E3Q00207B01040004000F2Q00400104000100022Q00D500056Q004001050001000200068A0105004C00013Q0004433Q004C00010006940102004C000100010004433Q004C00010012750005000E3Q00207B01050005000F2Q00400105000100022Q00AA0105000500040026FD0005004C000100100004433Q004C0001001275000500013Q00207B010500050002001226010600114Q00580005000200010004433Q003B000100208A0005000300122Q004301050002000100122Q000500013Q00202Q00050005000200122Q000600133Q00202Q00060006001400202Q00070001001500122Q000800166Q000600086Q00053Q00014Q00025Q0004433Q001800012Q009B000200023Q0030A80002000500170004433Q000400012Q0066012Q00013Q00033Q00043Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203053Q00416C69766503073Q0073652Q73696F6E00123Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001000013Q0004433Q001000012Q009B7Q00207B014Q000300068A012Q001000013Q0004433Q001000012Q009B3Q00014Q009B000100023Q00207B2Q010001000400068B3Q000F000100010004433Q000F00012Q009F8Q003C3Q00014Q00D63Q00024Q0066012Q00019Q003Q00034Q003C3Q00014Q00A2017Q0066012Q00017Q00033Q0003173Q0054656C65706F7274546F506C616365496E7374616E636503073Q00706C616365496403053Q006A6F62496400094Q000F7Q00206Q00014Q000200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q000400028Q000400016Q00017Q00073Q0003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C756501163Q0012E8000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004433Q000A000100208A000200010004001226010400054Q003500020004000200068A0102001500013Q0004433Q0015000100068A012Q001300013Q0004433Q00130001001275000300014Q004001030001000200207B0103000300070010460002000600030004433Q001500012Q009B000300013Q0010460002000600032Q0066012Q00017Q00073Q0003073Q0067657467656E76030E3Q0057616C6B53702Q656456616C7565030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q0012482Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q0004433Q001200012Q009B00015Q00207B2Q01000100040006550002000F000100010004433Q000F000100208A000200010005001226010400064Q003500020004000200068A0102001200013Q0004433Q00120001001046000200074Q0066012Q00017Q000A3Q0003073Q0067657467656E76030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C7565026Q00494001163Q0012E8000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A000100010004433Q000A000100208A000200010004001226010400054Q003500020004000200068A0102001500013Q0004433Q0015000100068A012Q001400013Q0004433Q001400010030A80002000600070012C8000300016Q00030001000200202Q00030003000900102Q00020008000300044Q001500010030A800020008000A2Q0066012Q00017Q00093Q0003073Q0067657467656E76030E3Q004A756D70506F77657256616C7565030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F77657201143Q0012482Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001300013Q0004433Q001300012Q009B00015Q00207B2Q01000100040006550002000F000100010004433Q000F000100208A000200010005001226010400064Q003500020004000200068A0102001300013Q0004433Q001300010030A8000200070008001046000200094Q0066012Q00017Q00023Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7001043Q001275000100014Q00402Q0100010002001046000100024Q0066012Q00017Q00043Q0003083Q00746F737472696E6703043Q0067737562030C3Q005E25732A282E2D2925732A2403023Q00253101093Q0012EE000100016Q00028Q00010002000200202Q00010001000200122Q000300033Q00122Q000400046Q0001000400024Q000100028Q00017Q00123Q00028Q0003063Q00737472696E6703053Q00752Q70657203043Q006773756203013Q002C034Q002Q033Q0025732B026Q00F03F03043Q0066696E6403013Q004B025Q00408F4003013Q004D024Q0080842E4103013Q0042024Q00652QCD4103013Q0054023Q00A0941A6D4203083Q00746F6E756D626572014D3Q000694012Q0004000100010004433Q000400010012262Q0100014Q00D6000100023Q001275000100023Q00207B2Q01000100032Q00D500026Q00312Q01000200022Q00D53Q00013Q00208A00013Q0004001226010300053Q001226010400064Q003500010004000200208A000100010004001226010300073Q001226010400064Q00DA0001000400026Q00013Q00122Q000100083Q00202Q00023Q000900122Q0004000A6Q00020004000200062Q0002001F00013Q0004433Q001F00010012262Q01000B3Q00202300023Q000400122Q0004000A3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100208A00023Q00090012260104000C4Q003500020004000200068A0102002B00013Q0004433Q002B00010012262Q01000D3Q00202300023Q000400122Q0004000C3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100208A00023Q00090012260104000E4Q003500020004000200068A0102003700013Q0004433Q003700010012262Q01000F3Q00202300023Q000400122Q0004000E3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100208A00023Q0009001226010400104Q003500020004000200068A0102004200013Q0004433Q004200010012262Q0100113Q00208A00023Q0004001226010400103Q001226010500064Q00350002000500022Q00D53Q00023Q001275000200124Q00D500036Q003101020002000200068A0102004A00013Q0004433Q004A00012Q002D0103000200010006940103004B000100010004433Q004B0001001226010300014Q00D6000300024Q0066012Q00017Q000E3Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D6503063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A65637403053Q007461626C6503063Q00696E7365727403043Q004E616D6503043Q00736F7274003A4Q00707Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q0004433Q001E000100208A00013Q0001001226010300034Q003500010003000200068A2Q01001E00013Q0004433Q001E000100207B2Q013Q000300208A000100010001001226010300044Q003500010003000200068A2Q01001E00013Q0004433Q001E000100207B2Q013Q000300207300010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q0004433Q001E000100207B2Q013Q000300206100010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006942Q010022000100010004433Q002200012Q00CA000200024Q00D6000200024Q00E000025Q001211010300073Q00202Q0004000100084Q000400056Q00033Q000500044Q0032000100208A000800070009001226010A000A4Q00350008000A000200068A0108003200013Q0004433Q003200010012750008000B3Q00207B01080008000C2Q00D5000900023Q00207B010A0007000D2Q00E10008000A00010006C000030028000100020004433Q002800010012750003000B3Q0020FC00030003000E4Q000400026Q0003000200014Q000200028Q00017Q00163Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D65028Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A656374026Q00F03F030E3Q0047657444657363656E64616E747303073Q0056697369626C6503043Q004E616D6503063Q004C6F636B656403043Q004C6F636B03073Q00556E6B6E6F776E030A3Q00496D6167654C6162656C030B3Q00496D616765436F6C6F723303063Q00436F6C6F72332Q033Q006E657700634Q00707Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q0004433Q001E000100208A00013Q0001001226010300034Q003500010003000200068A2Q01001E00013Q0004433Q001E000100207B2Q013Q000300208A000100010001001226010300044Q003500010003000200068A2Q01001E00013Q0004433Q001E000100207B2Q013Q000300207300010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q0004433Q001E000100207B2Q013Q000300206100010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006942Q010022000100010004433Q002200012Q00CA000200024Q00D6000200023Q001226010200073Q00120B000300073Q00122Q000400083Q00202Q0005000100094Q000500066Q00043Q000600044Q005D000100208A00090008000A001226010B000B4Q00350009000B000200068A0109005D00013Q0004433Q005D000100209101030003000C2Q000801095Q00122Q000A00083Q00202Q000B0008000D4Q000B000C6Q000A3Q000C00044Q0058000100208A000F000E000A0012260111000B4Q0035000F0011000200068A010F004800013Q0004433Q0048000100207B010F000E000E00068A010F004800013Q0004433Q0048000100207B010F000E000F00261B010F0046000100100004433Q0046000100207B010F000E000F00261B010F0046000100110004433Q0046000100207B010F000E000F0026E7000F0048000100120004433Q004800012Q003C000900013Q0004433Q005A000100208A000F000E000A001226011100134Q0035000F0011000200068A010F005800013Q0004433Q0058000100207B010F000E0014001275001000153Q00207B011000100016001226011100073Q001226011200073Q001226011300074Q0035001000130002000684010F0058000100100004433Q005800012Q003C000900013Q0004433Q005A00010006C0000A0035000100020004433Q003500010006940109005D000100010004433Q005D000100209101020002000C0006C000040029000100020004433Q002900012Q00D5000400024Q00D5000500034Q0072000400034Q0066012Q00017Q00043Q00030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657200114Q001F016Q00206Q000100122Q000200028Q0002000200064Q000A00013Q0004433Q000A000100208A00013Q0001001226010300034Q00350001000300022Q00D53Q00013Q0006550001000F00013Q0004433Q000F000100208A00013Q0001001226010300044Q00350001000300022Q00D6000100024Q0066012Q00017Q00193Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q6665727303063Q0069706169727303053Q00546965723103053Q00546965723203053Q0054696572332Q033Q005065742Q033Q0049734103093Q00546578744C6162656C03073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03043Q006E616D6503043Q005465787403053Q00707269636503013Q003F03093Q00617661696C61626C6503073Q0056697369626C65034Q0003023Q00783000674Q00707Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q0004433Q001E000100208A00013Q0001001226010300034Q003500010003000200068A2Q01001E00013Q0004433Q001E000100207B2Q013Q000300208A000100010001001226010300044Q003500010003000200068A2Q01001E00013Q0004433Q001E000100207B2Q013Q000300207300010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q0004433Q001E000100207B2Q013Q000300206100010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006942Q010022000100010004433Q002200012Q00CA000200024Q00D6000200024Q00E000025Q001275000300074Q00E9000400033Q00122Q000500083Q00122Q000600093Q00122Q0007000A6Q0004000300012Q00A60103000200050004433Q0063000100208A0008000100012Q00D5000A00074Q00350008000A000200065500090033000100080004433Q0033000100208A000900080001001226010B000B4Q00350009000B000200068A0109006300013Q0004433Q0063000100208A000A0009000C001226010C000D4Q0035000A000C000200068A010A006300013Q0004433Q0063000100208A000A00080001001226010C000E4Q0035000A000C000200208A000B00080001001226010D000F4Q0035000B000D000200208A000C00080001001226010E00104Q0035000C000E0002000655000D00480001000C0004433Q0048000100208A000D000C0001001226010F00114Q0035000D000F00022Q00E0000E3Q000300207B010F00090013001046000E0012000F00068A010D005000013Q0004433Q0050000100207B010F000D0013000694010F0051000100010004433Q00510001001226010F00153Q001046000E0014000F00068A010A005700013Q0004433Q0057000100207B010F000A0017000694010F005F000100010004433Q005F000100068A010B005C00013Q0004433Q005C000100207B010F000B0013000694010F005D000100010004433Q005D0001001226010F00183Q0026E7000F0060000100190004433Q006000012Q009F000F6Q003C000F00013Q001046000E0016000F2Q004401020007000E0006C00003002B000100020004433Q002B00012Q00D6000200024Q0066012Q00017Q00033Q0003063Q0069706169727303073Q0067657467656E7603123Q0053656C656374656454617267657450657473010F3Q0012B3000100013Q00122Q000200026Q00020001000200202Q0002000200034Q00010002000300044Q000A00010006840105000A00013Q0004433Q000A00012Q003C000600014Q00D6000600023Q0006C000010006000100020004433Q000600012Q003C00016Q00D6000100024Q0066012Q00017Q00093Q0003073Q0067657467656E7603123Q0053656C65637465645461726765745065747303063Q0069706169727303053Q007461626C6503063Q0072656D6F76652Q033Q006D617803043Q004D617820030E3Q00207065747320726561636865642103063Q00696E7365727401263Q001275000100014Q00402Q010001000200207B2Q0100010002001275000200034Q00D5000300014Q00A60102000200040004433Q001000010006840106001000013Q0004433Q00100001001275000700043Q0020850107000700054Q000800016Q000900056Q0007000900014Q000700016Q000700023Q0006C000020007000100020004433Q000700012Q0077010200014Q009B00035Q00207B0103000300060006D30003001E000100020004433Q001E00012Q003C00025Q001218000300076Q00045Q00202Q00040004000600122Q000500086Q0003000300054Q000200033Q001275000200043Q0020850102000200094Q000300016Q00048Q0002000400014Q000200016Q000200024Q0066012Q00017Q00043Q0003053Q007461626C6503053Q00636C65617203073Q0067657467656E7603123Q0053656C65637465645461726765745065747300073Q0012753Q00013Q00207B014Q0002001275000100034Q00402Q010001000200207B2Q01000100042Q00583Q000200012Q0066012Q00017Q00063Q0003073Q0067657467656E7603123Q0053656C656374656454617267657450657473028Q0003043Q004E6F6E65026Q00F03F2Q033Q00202B2000143Q0012753Q00014Q0040012Q0001000200207B014Q00022Q00772Q015Q0026E700010008000100030004433Q000800010012262Q0100044Q00D6000100024Q00772Q015Q0026E70001000D000100050004433Q000D000100207B2Q013Q00052Q00D6000100023Q00207B2Q013Q0005001226010200064Q007701035Q0020780003000300052Q00F20001000100032Q00D6000100024Q0066012Q00017Q00033Q0003073Q0067657467656E7603143Q004D65726368616E744D6178507269636554657874034Q0001073Q001275000100014Q00402Q01000100020006390002000500013Q0004433Q00050001001226010200033Q0010460001000200022Q0066012Q00017Q00063Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01173Q0012052Q0100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D000100010004433Q000D00012Q0066012Q00013Q001275000200053Q00207B0102000200060006A300033Q000100052Q00D53Q00014Q009B8Q009B3Q00014Q009B3Q00024Q009B3Q00034Q00580002000200012Q0066012Q00013Q00013Q00283Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657203063Q0069706169727303123Q0053656C65637465645461726765745065747303063Q00737472696E6703053Q00752Q7065722Q0103043Q006E65787403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q66657273030A3Q007061727365507269636503143Q004D65726368616E744D617850726963655465787403053Q00546965723103053Q00546965723203053Q0054696572332Q033Q0050657403073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03073Q0056697369626C6503043Q0054657874034Q002Q033Q0049734103093Q00546578744C6162656C03023Q007830028Q0003053Q007063612Q6C0003043Q007461736B03043Q0077616974026Q33D33F00BF4Q00E08Q00CA000100013Q001275000200014Q004001020001000200207B01020002000200068A010200BE00013Q0004433Q00BE00012Q009B00026Q009B000300013Q00207B010300030003000684010200BE000100030004433Q00BE00010006942Q01001E000100010004433Q001E00012Q009B000200023Q00208A000200020004001226010400054Q003500020004000200068A0102001800013Q0004433Q0018000100208A000300020004001226010500064Q00350003000500022Q00D5000200033Q0006550001001E000100020004433Q001E000100208A000300020004001226010500074Q00350003000500022Q00D5000100034Q00E000025Q0012B3000300083Q00122Q000400016Q00040001000200202Q0004000400094Q00030002000500044Q002C00010012750008000A3Q00206D01080008000B4Q000900036Q000A00076Q0009000A6Q00083Q000200202Q00020008000C0006C000030025000100020004433Q0025000100068A2Q0100B900013Q0004433Q00B900010012750003000D4Q00D5000400024Q003101030002000200068A010300B900013Q0004433Q00B900012Q009B000300043Q00208A0003000300040012260105000E4Q003500030005000200065500040053000100030004433Q0053000100208A0004000300040012260106000F4Q003500040006000200068A0104005300013Q0004433Q0053000100207B01040003000F00208A000400040004001226010600104Q003500040006000200068A0104005300013Q0004433Q0053000100207B01040003000F00207300040004001000202Q00040004000400122Q000600116Q00040006000200062Q0004005300013Q0004433Q0053000100207B01040003000F00206100040004001000202Q00040004001100202Q00040004000400122Q000600126Q00040006000200068A010400B700013Q0004433Q00B700012Q009B000500013Q00202800050005001300122Q000600016Q00060001000200202Q0006000600144Q00050002000200122Q000600084Q00E9000700033Q00122Q000800153Q00122Q000900163Q00122Q000A00176Q0007000300012Q00A60106000200080004433Q00B4000100208A000B000400042Q00D5000D000A4Q0035000B000D00022Q003C000C5Q00068A010B00B000013Q0004433Q00B0000100208A000D000B0004001226010F00184Q0035000D000F000200208A000E000B0004001226011000194Q0035000E0010000200208A000F000B00040012260111001A4Q0035000F0011000200208A0010000B00040012260112001B4Q00350010001200020006550011007A000100100004433Q007A000100208A0011001000040012260113001C4Q00350011001300020006550012007D0001000E0004433Q007D000100207B0112000E001D00068A010F008200013Q0004433Q0082000100207B0113000F001E00069401130083000100010004433Q008300010012260113001F3Q00068A010D00B000013Q0004433Q00B0000100208A0014000D0020001226011600214Q003500140016000200068A011400B000013Q0004433Q00B00001000694011200B0000100010004433Q00B0000100261B011300B0000100220004433Q00B000010012750014000A3Q00200D00140014000B4Q001500033Q00202Q0016000D001E4Q001500166Q00143Q00024Q00140002001400062Q001400B000013Q0004433Q00B0000100068A0111009F00013Q0004433Q009F00012Q009B001400013Q00207B01140014001300207B01150011001E2Q0031011400020002000694011400A0000100010004433Q00A00001001226011400233Q002650000500A4000100230004433Q00A400010006D3001400B0000100050004433Q00B000012Q003C000C00014Q001C00153Q000A000694011500B0000100010004433Q00B00001001275001500243Q0006A300163Q000100022Q00D53Q00014Q00D53Q000A4Q003101150002000200068A011500B000013Q0004433Q00B0000100207E012Q000A000C000694010C00B3000100010004433Q00B3000100207E012Q000A00252Q00D400095Q0006C000060063000100020004433Q006300010004433Q00B900012Q00E000056Q00D53Q00053Q001275000300263Q00207B010300030027001226010400284Q00580003000200010004433Q000200012Q0066012Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q00BE7Q00206Q00014Q000200018Q000200016Q00017Q00053Q0003023Q00756903063Q007374617475732Q033Q00536574034Q0003073Q0063752Q72656E74021B4Q009B00025Q00207B01020002000100207B01020002000200068A0102000D00013Q0004433Q000D00012Q009B00025Q00207B01020002000100207B01020002000200208A0002000200030006390004000C00013Q0004433Q000C0001001226010400044Q00E10002000400012Q009B00025Q00207B01020002000100207B01020002000500068A0102001A00013Q0004433Q001A000100068A2Q01001A00013Q0004433Q001A00012Q009B00025Q00207B01020002000100207B01020002000500208A0002000200032Q00D5000400014Q00E10002000400012Q0066012Q00017Q00013Q0003173Q005365744175746F486174636853656C6563746564452Q6701054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q0003123Q005365744175746F48617463685065724F6E6501054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030C3Q005365744175746F50756E636801054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030B3Q005365744175746F4C69667401054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030C3Q005365744175746F53746F6D7001054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030F3Q005365744175746F47656D54772Q656E01054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030B3Q005365744175746F53652Q6C01054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q0003123Q005365744175746F54652Q7269746F7269657301054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030C3Q005365744175746F437562657301054Q008400015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00043Q0003093Q004175746F437562657303073Q00456E61626C65642Q0103073Q0067657467656E7601123Q0026E73Q0009000100010004433Q000900012Q009B00015Q00207B2Q010001000200261B2Q010007000100030004433Q000700012Q009F00016Q003C000100014Q00D6000100023Q001275000100044Q00402Q01000100022Q001C000100013Q00261B2Q01000F000100030004433Q000F00012Q009F00016Q003C000100014Q00D6000100024Q0066012Q00017Q00023Q0003143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E65000D4Q009B8Q0040012Q0001000200068A012Q000800013Q0004433Q000800012Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q00E03Q00013Q0012262Q0100024Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q0003093Q004175746F50756E636800054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q000E3Q0003083Q0074704D616E75616C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03073Q006F776E656454702Q01030F3Q005365745470546F426F2Q734B692Q6C012Q003A4Q009B7Q00207B014Q000100068A012Q000500013Q0004433Q000500012Q0066012Q00013Q0012753Q00023Q002069014Q000300122Q000200048Q000200024Q00015Q00064Q001A00013Q0004433Q001A0001001275000200053Q00208A00033Q00062Q001C010300044Q001900023Q00040004433Q0018000100208A000700060007001226010900084Q003500070009000200068A0107001800013Q0004433Q001800012Q003C000100013Q0004433Q001A00010006C000020011000100020004433Q0011000100068A2Q01002800013Q0004433Q00280001001275000200094Q004001020001000200207B01020002000A00069401020028000100010004433Q002800012Q009B00025Q0030AD0102000B000C4Q000200013Q00202Q00020002000D4Q000300016Q00020002000100044Q003900010006942Q010039000100010004433Q003900012Q009B00025Q00207B01020002000B00068A0102003900013Q0004433Q00390001001275000200094Q004001020001000200207B01020002000A00068A0102003900013Q0004433Q003900012Q009B00025Q0030A80002000B000E2Q009B000200013Q00207B01020002000D2Q003C00036Q00580002000200012Q0066012Q00017Q00013Q0003083Q004175746F4C69667400054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q0003063Q0049736C616E6400044Q009B7Q0012262Q0100014Q00583Q000200012Q0066012Q00017Q00013Q0003083Q004175746F4C69667400054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q0003063Q004D61726B657400044Q009B7Q0012262Q0100014Q00583Q000200012Q0066012Q00017Q00013Q0003083Q004175746F4C69667400054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q0003093Q004175746F50756E636800054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q0003093Q004175746F53746F6D7000054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q00030C3Q004175746F47656D54772Q656E00054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q0003083Q004175746F53652Q6C00054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00013Q00030F3Q004175746F54652Q7269746F7269657300054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q00073Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F7269657303043Q006F776E7303053Q007461626C6503063Q00696E7365727403053Q006F776E656403123Q005365744175746F54652Q7269746F7269657300163Q0012753Q00014Q0040012Q0001000200207B014Q0002000694012Q0015000100010004433Q001500012Q009B7Q00207B014Q00030012262Q0100024Q0031012Q00020002000694012Q0011000100010004433Q001100010012753Q00043Q00200F014Q00054Q00015Q00202Q00010001000600122Q000200028Q000200012Q009B3Q00013Q00207B014Q00072Q003C000100014Q00583Q000200012Q0066012Q00017Q00013Q0003093Q004175746F437562657300054Q00E03Q00013Q0012262Q0100014Q00C43Q000100012Q00D63Q00024Q0066012Q00017Q001D3Q0003063Q00737472696E6703053Q006C6F776572034Q0003043Q0066696E6403053Q0068617463682Q033Q00652Q6703063Q00706C617965722Q033Q0070767003043Q00626F2Q7303103Q0025665B25615D706C617925665B25415D03083Q00706C617974696D6503043Q00686F757203063Q006D696E75746503063Q006F6E6C696E6503073Q0070612Q7369766503043Q006C69667403063Q0069736C616E64030B3Q006C6966745F69736C616E6403063Q006D61726B6574030B3Q0073757065726D61726B6574030B3Q006C6966745F6D61726B657403053Q0070756E636803053Q0073746F6D702Q033Q0067656D03073Q00636F2Q6C65637403043Q0073652Q6C03083Q0074652Q7269746F7203093Q0074652Q7269746F727903043Q0063756265018C3Q001275000100013Q00207B2Q01000100020006390002000500013Q0004433Q00050001001226010200034Q00312Q010002000200208A000200010004001226010400054Q003500020004000200069401020010000100010004433Q0010000100208A000200010004001226010400064Q003500020004000200068A0102001200013Q0004433Q00120001001226010200054Q00D6000200023Q00208A000200010004001226010400074Q00350002000400020006940102001C000100010004433Q001C000100208A000200010004001226010400084Q003500020004000200068A0102001E00013Q0004433Q001E0001001226010200084Q00D6000200023Q00208A000200010004001226010400094Q003500020004000200068A0102002500013Q0004433Q00250001001226010200094Q00D6000200023Q00208A0002000100040012260104000A4Q00350002000400020006940102003E000100010004433Q003E000100208A0002000100040012260104000B4Q00350002000400020006940102003E000100010004433Q003E000100208A0002000100040012260104000C4Q00350002000400020006940102003E000100010004433Q003E000100208A0002000100040012260104000D4Q00350002000400020006940102003E000100010004433Q003E000100208A0002000100040012260104000E4Q003500020004000200068A0102004000013Q0004433Q004000010012260102000F4Q00D6000200023Q00208A000200010004001226010400104Q003500020004000200068A0102005A00013Q0004433Q005A000100208A000200010004001226010400114Q003500020004000200068A0102004C00013Q0004433Q004C0001001226010200124Q00D6000200023Q00208A000200010004001226010400134Q003500020004000200069401020056000100010004433Q0056000100208A000200010004001226010400144Q003500020004000200068A0102005800013Q0004433Q00580001001226010200154Q00D6000200023Q001226010200104Q00D6000200023Q00208A000200010004001226010400164Q003500020004000200068A0102006100013Q0004433Q00610001001226010200164Q00D6000200023Q00208A000200010004001226010400174Q003500020004000200068A0102006800013Q0004433Q00680001001226010200174Q00D6000200023Q00208A000200010004001226010400184Q003500020004000200069401020072000100010004433Q0072000100208A000200010004001226010400194Q003500020004000200068A0102007400013Q0004433Q00740001001226010200184Q00D6000200023Q00208A0002000100040012260104001A4Q003500020004000200068A0102007B00013Q0004433Q007B00010012260102001A4Q00D6000200023Q00208A0002000100040012260104001B4Q003500020004000200068A0102008200013Q0004433Q008200010012260102001C4Q00D6000200023Q00208A0002000100040012260104001D4Q003500020004000200068A0102008900013Q0004433Q008900010012260102001D4Q00D6000200024Q00CA000200024Q00D6000200024Q0066012Q00017Q00063Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030A3Q005175657374734C697374030E3Q005363726F2Q6C696E674672616D6503043Q004C697374001A4Q00707Q00206Q000100122Q000200028Q0002000200062Q0001000900013Q0004433Q0009000100208A00013Q0001001226010300034Q00350001000300020006550002000E000100010004433Q000E000100208A000200010001001226010400044Q003500020004000200065500030013000100020004433Q0013000100208A000300020001001226010500054Q003500030005000200065500040018000100030004433Q0018000100208A000400030001001226010600064Q00350004000600022Q00D6000400024Q0066012Q00017Q000A3Q0003043Q006773756203013Q002C034Q0003043Q0066696E6403013Q003A028Q0003063Q00676D617463682Q033Q0025642B026Q004E4003083Q00746F6E756D626572011C3Q00208A00013Q0001001226010300023Q001226010400034Q00350001000400022Q00D53Q00013Q00208A00013Q0004001226010300054Q003500010003000200068A2Q01001700013Q0004433Q001700010012262Q0100063Q00208A00023Q0007001226010400084Q004B0102000400040004433Q001400010020BB0006000100090012750007000A4Q00D5000800054Q00310107000200022Q00B50001000600070006C00002000F000100010004433Q000F00012Q00D6000100023Q0012750001000A4Q00D500026Q001D000100024Q00202Q016Q0066012Q00017Q00033Q0003063Q00737472696E6703053Q006D61746368031D3Q00285B25643A2C252E5D2B2925732A2F25732A285B25643A2C252E5D2B2901113Q0012672Q0100013Q00202Q0001000100024Q00025Q00122Q000300036Q00010003000200062Q00010009000100010004433Q000900012Q00CA000300034Q00D6000300024Q009B00036Q009C010400016Q0003000200024Q00048Q000500026Q000400056Q00039Q0000017Q00043Q002Q033Q0049734103093Q004775694F626A65637403073Q0056697369626C6503063Q00506172656E7402144Q00D500025Q00068A0102001100013Q0004433Q0011000100068B00020011000100010004433Q0011000100208A000300020001001226010500024Q003500030005000200068A0103000F00013Q0004433Q000F000100207B0103000200030006940103000F000100010004433Q000F00012Q003C00036Q00D6000300023Q00207B0102000200040004433Q000100012Q003C000300014Q00D6000300024Q0066012Q00017Q00243Q0003073Q006765744C69737403063Q00697061697273030B3Q004765744368696C6472656E03063Q00737472696E6703053Q006D6174636803043Q004E616D65030A3Q005E4461696C7943617264030E3Q0046696E6446697273744368696C64030A3Q00436F6E74656E74526F7703043Q00496E666F03093Q0051756573744E616D65030E3Q0051756573744E616D65426F6E7573034Q0003073Q0056697369626C6503043Q0054657874030B3Q00426F6E75734368726F6D652Q033Q0049734103093Q004775694F626A656374030E3Q0047657444657363656E64616E747303093Q00546578744C6162656C030D3Q00706172736550726F6772652Q73028Q0003053Q006C6F77657203043Q0066696E6403073Q00636C61696D656403093Q00636F6D706C6574656403053Q007461626C6503063Q00696E7365727403023Q00696403013Q007C03043Q006E616D6503043Q006465736303043Q006B696E6403083Q00636C612Q7369667903053Q00626F6E757303043Q00646F6E65008D4Q0088016Q00206Q00016Q000100024Q00015Q00064Q0007000100010004433Q000700012Q00D6000100023Q001275000200023Q00208A00033Q00032Q001C010300044Q001900023Q00040004433Q00890001001275000700043Q00201F00070007000500202Q00080006000600122Q000900076Q00070009000200062Q0007008900013Q0004433Q0089000100208A000700060008001226010900094Q003500070009000200068A0107001C00013Q0004433Q001C000100207B01070006000900208A0007000700080012260109000A4Q003500070009000200065500080021000100070004433Q0021000100208A000800070008001226010A000B4Q00350008000A000200065500090026000100070004433Q0026000100208A000900070008001226010B000C4Q00350009000B0002001226010A000D3Q00068A0108002E00013Q0004433Q002E000100207B010B0008000E00068A010B002E00013Q0004433Q002E000100207B010A0008000F0004433Q0038000100068A0109003500013Q0004433Q0035000100207B010B0009000E00068A010B003500013Q0004433Q0035000100207B010A0009000F0004433Q0038000100068A0108003800013Q0004433Q0038000100207B010A0008000F00208A000B00060008001226010D00104Q0035000B000D000200068A010B004500013Q0004433Q0045000100208A000C000B0011001226010E00124Q0035000C000E000200068A010C004500013Q0004433Q0045000100207B010C000B000E000694010C0046000100010004433Q004600012Q003C000C6Q003C000D5Q001211010E00023Q00202Q000F000600134Q000F00106Q000E3Q001000044Q0073000100208A001300120011001226011500144Q003500130015000200068A0113007300013Q0004433Q007300012Q009B001300014Q00D5001400124Q00D5001500064Q003500130015000200068A0113007300013Q0004433Q007300012Q009B00135Q00207B01130013001500207B01140012000F2Q00A601130002001400068A0113006400013Q0004433Q0064000100068A0114006400013Q0004433Q00640001000E1701160064000100140004433Q006400010006D300140064000100130004433Q006400012Q003C000D00013Q001275001500043Q00207B01150015001700207B01160012000F2Q003101150002000200208A001600150018001226011800194Q003500160018000200069401160072000100010004433Q0072000100208A0016001500180012260118001A4Q003500160018000200068A0116007300013Q0004433Q007300012Q003C000D00013Q0006C0000E004C000100020004433Q004C0001001275000E001B3Q002061010E000E001C4Q000F00016Q00103Q000600202Q00110006000600122Q0012001E6Q0013000A6Q00110011001300102Q0010001D001100202Q00110006000600102Q0010001F001100102Q00100020000A4Q00115Q00202Q0011001100224Q0012000A6Q00110002000200102Q00100021001100102Q00100023000C00102Q00100024000D4Q000E001000010006C00002000C000100020004433Q000C00012Q00D6000100024Q0066012Q00017Q00023Q0003063Q0069706169727303053Q006F776E6564010E3Q001275000100014Q009B00025Q00207B0102000200022Q00A62Q01000200030004433Q000900010006840105000900013Q0004433Q000900012Q003C000600014Q00D6000600023Q0006C000010005000100020004433Q000500012Q003C00016Q00D6000100024Q0066012Q00017Q00093Q0003053Q006F776E6564026Q00F03F026Q00F0BF03053Q007461626C6503063Q0072656D6F766503073Q006F776E6564547003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C012Q00204Q00F17Q00206Q00019Q0000122Q000100023Q00122Q000200033Q00044Q001400012Q009B000400014Q007C01055Q00202Q0005000500014Q0005000500034Q00040002000200062Q00040013000100010004433Q00130001001275000400043Q0020B80004000400054Q00055Q00202Q0005000500014Q000600036Q0004000600010004F63Q000600012Q009B7Q00207B014Q000600068A012Q001F00013Q0004433Q001F00010012753Q00074Q0040012Q0001000200207B014Q0008000694012Q001F000100010004433Q001F00012Q009B7Q0030A83Q000600092Q0066012Q00017Q00073Q0003053Q006F776E656403073Q006F776E6564547003063Q006163746976650003063Q0069706169727303053Q007063612Q6C030F3Q005365745470546F426F2Q734B692Q6C00224Q00777Q00206Q00014Q00015Q00202Q0001000100024Q00028Q00038Q00048Q00058Q00065Q00302Q000400030004001046000300020006001046000200010005001275000200054Q00D500036Q00A60102000200040004433Q001800012Q009B000700014Q001C00070007000600068A0107001800013Q0004433Q00180001001275000800064Q00D5000900074Q003C000A6Q00E10008000A00010006C000020010000100020004433Q0010000100068A2Q01002100013Q0004433Q00210001001275000200064Q009B000300023Q00207B0103000300072Q003C00046Q00E10002000400012Q0066012Q00017Q000D3Q0003043Q0073746F7003063Q0061637469766503083Q0074704D616E75616C03073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C2Q0103063Q0069706169727303053Q00666C61677303053Q007461626C6503063Q00696E7365727403053Q006F776E656403053Q007063612Q6C03043Q007469636B01344Q00292Q015Q00202Q0001000100014Q0001000100014Q000100016Q000100013Q00062Q00010008000100010004433Q000800012Q0066012Q00014Q009B00025Q001046000200024Q009B00025Q001275000300044Q004001030001000200207B01030003000500261B01030011000100060004433Q001100012Q009F00036Q003C000300013Q00101401020003000300122Q000200073Q00202Q0003000100084Q000300016Q00023Q000400044Q002B00012Q009B000700024Q00D5000800064Q00310107000200020006940107002B000100010004433Q002B0001001275000700093Q00205101070007000A4Q00085Q00202Q00080008000B4Q000900066Q0007000900014Q000700036Q00070007000600062Q0007002B00013Q0004433Q002B00010012750008000C4Q00D5000900074Q003C000A00014Q00E10008000A00010006C000020018000100020004433Q0018000100207B01020001000D00068A0102003300013Q0004433Q003300010012750002000C3Q00207B01030001000D2Q00580002000200012Q0066012Q00017Q00093Q0003073Q0067657467656E7603093Q004175746F517565737403073Q0073652Q73696F6E026Q00F03F03043Q0073746F702Q033Q004F2Q6603013Q002D03043Q007461736B03053Q00737061776E011E3Q0012ED000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100032Q009B00025Q00207B0102000200052Q0032000200010001000694012Q0014000100010004433Q001400012Q009B000200013Q001226010300063Q001226010400074Q00E10002000400012Q0066012Q00013Q001275000200083Q00207B0102000200090006A300033Q000100052Q009B3Q00024Q00D53Q00014Q009B8Q009B3Q00014Q009B3Q00034Q00580002000200012Q0066012Q00013Q00013Q00253Q00028Q00025Q00208C4003123Q0072656C656173655573657253746F2Q70656403093Q0072656164436172647303193Q004F70656E2074686520517565737473206C697374206F6E636503013Q002D03063Q0069706169727303073Q0067657467656E76030A3Q005175657374446562756703053Q007072696E74032A3Q005B51756573745D202573207C202573207C206B696E643D257320626F6E75733D257320646F6E653D257303063Q00666F726D617403043Q006E616D6503043Q006465736303083Q00746F737472696E6703043Q006B696E6403053Q00626F6E757303043Q00646F6E6503073Q0070612Q736976652Q033Q0070767003023Q00696403053Q00737461727403023Q006F7303053Q00636C6F636B03073Q00576F726B696E6703043Q007469636B03053Q007063612Q6C2Q0103043Q007461736B03053Q0064656C617903043Q0073746F70031F3Q004F6E6C7920706C617974696D65202F2050765020717565737473206C65667403073Q00536B692Q70656403173Q00556E6B6E6F776E2071756573742074797065206C656674030F3Q00412Q6C2071756573747320646F6E6503043Q0077616974026Q00F03F00AC3Q0006A35Q000100032Q009B8Q009B3Q00014Q009B3Q00024Q00CA000100013Q001226010200014Q00E000035Q001226010400024Q00D500056Q004001050001000200068A010500A800013Q0004433Q00A800012Q009B000500023Q00207B0105000500032Q00320005000100012Q009B000500023Q00207B0105000500042Q00400105000100022Q0077010600053Q0026E700060019000100010004433Q001900012Q009B000600033Q001226010700053Q001226010800064Q00E10006000800012Q00CA000600064Q00F400078Q00088Q00095Q00122Q000A00076Q000B00056Q000A0002000C00044Q00550001001275000F00084Q0040010F0001000200207B010F000F000900068A010F003600013Q0004433Q00360001001275000F000A3Q0012070010000B3Q00202Q00100010000C00202Q0012000E000D00202Q0013000E000E00122Q0014000F3Q00202Q0015000E00104Q00140002000200122Q0015000F3Q00202Q0016000E00114Q00150002000200122Q0016000F3Q00202Q0017000E00124Q001600176Q00108Q000F3Q000100207B010F000E0011000694010F0055000100010004433Q0055000100207B010F000E0012000694010F0055000100010004433Q0055000100207B010F000E00100026E7000F0041000100130004433Q004100012Q003C000700013Q0004433Q0055000100207B010F000E00100026E7000F0046000100140004433Q004600012Q003C000900013Q0004433Q0055000100207B010F000E001000068A010F005100013Q0004433Q0051000100207B010F000E00152Q001C000F0003000F000694010F0051000100010004433Q0051000100069401060050000100010004433Q005000012Q00D50006000E3Q0004433Q0055000100207B010F000E0010000694010F0055000100010004433Q005500012Q003C000800013Q0006C0000A0021000100020004433Q0021000100068A0106008B00013Q0004433Q008B000100207B010A0006001500068B000100660001000A0004433Q006600012Q009B000A00023Q00206F010A000A001600202Q000B000600104Q000A0002000100202Q000A0006001500122Q000B00173Q00202Q000B000B00184Q000B000100024Q0002000B6Q0001000A4Q009B000A00033Q001245010B00193Q00202Q000C0006000E4Q000A000C00014Q000A00043Q00202Q000B000600104Q000A000A000B00062Q000A007500013Q0004433Q0075000100207B010B000A001A00068A010B007500013Q0004433Q00750001001275000B001B3Q00207B010C000A001A2Q0058000B00020001001275000B00173Q00207B010B000B00182Q0040010B000100022Q00AA010B000B000200067F010400A30001000B0004433Q00A3000100207B010B0006001500207E0103000B001C00207B010B00060015001275000C001D3Q00207B010C000C001E2Q00D5000D00043Q0006A3000E0001000100022Q00D53Q00034Q00D53Q000B4Q00E4000C000E00014Q000C00023Q00202Q000C000C001F4Q000C000100014Q000100016Q000B5Q00044Q00A300012Q009B000A00023Q00207B010A000A001F2Q0032000A000100012Q00CA000100013Q00069401070093000100010004433Q0093000100068A0109009800013Q0004433Q009800012Q009B000A00033Q001226010B00203Q001226010C00214Q00E1000A000C00010004433Q00A3000100068A0108009F00013Q0004433Q009F00012Q009B000A00033Q001226010B00223Q001226010C00064Q00E1000A000C00010004433Q00A300012Q009B000A00033Q001226010B00233Q001226010C00064Q00E1000A000C0001001275000A001D3Q00207B010A000A0024001226010B00254Q0058000A000200010004433Q000800012Q009B000500023Q00207B01050005001F2Q00320005000100012Q0066012Q00013Q00023Q00043Q0003073Q0067657467656E7603093Q004175746F517565737403053Q00416C69766503073Q0073652Q73696F6E00123Q0012753Q00014Q0040012Q0001000200207B014Q000200068A012Q001000013Q0004433Q001000012Q009B7Q00207B014Q000300068A012Q001000013Q0004433Q001000012Q009B3Q00014Q009B000100023Q00207B2Q010001000400068B3Q000F000100010004433Q000F00012Q009F8Q003C3Q00014Q00D63Q00024Q0066012Q00017Q00015Q00044Q009B8Q009B000100013Q00207E012Q000100012Q0066012Q00017Q00053Q0003083Q00496E7374616E63652Q033Q006E657703053Q00706169727303063Q00506172656E7403063Q00697061697273031D3Q001271010300013Q00202Q0003000300024Q00048Q00030002000200122Q000400036Q000500016Q00040002000600044Q000B000100261B0107000B000100040004433Q000B00012Q00440103000700080006C000040008000100020004433Q0008000100068A0102001600013Q0004433Q00160001001275000400054Q00D5000500024Q00A60104000200060004433Q001400010010460008000400030006C000040013000100020004433Q0013000100207B01040001000400068A0104001B00013Q0004433Q001B000100207B0104000100040010460003000400042Q00D6000300024Q0066012Q00017Q00073Q0003083Q005549436F726E6572030C3Q00436F726E657252616469757303043Q005544696D2Q033Q006E6577028Q00026Q00144003063Q00506172656E74020F4Q00CE00025Q00122Q000300016Q00043Q000200122Q000500033Q00202Q00050005000400122Q000600053Q00062Q00070009000100010004433Q00090001001226010700064Q00350005000700020010A701040002000500102Q000400076Q000200046Q00029Q0000017Q00083Q0003083Q0055495374726F6B6503053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q00E03F03093Q00546869636B6E652Q73026Q00F03F03063Q00506172656E7404144Q009B00045Q001226010500014Q00E000063Q000400063900070007000100010004433Q000700012Q009B000700013Q00207B0107000700030010460006000200070006390007000B000100020004433Q000B0001001226010700053Q0010460006000400070006390007000F000100030004433Q000F0001001226010700073Q001046000600060007001046000600084Q001D000400064Q002001046Q0066012Q00017Q00013Q0003053Q007063612Q6C030E3Q001275000300013Q0006A300043Q000100042Q009B8Q00D53Q00014Q00D53Q00024Q00D58Q00A601030002000400068A0103000B00013Q0004433Q000B00010006390005000C000100040004433Q000C00012Q00CA000500054Q00D6000500024Q0066012Q00013Q00013Q00093Q0003083Q005549536861646F77030A3Q00426C757252616469757303043Q005544696D2Q033Q006E6577028Q00026Q002840030C3Q005472616E73706172656E6379026Q00E03F03063Q00506172656E7400164Q00A47Q00122Q000100016Q00023Q000300122Q000300033Q00202Q00030003000400122Q000400056Q000500013Q00062Q0005000A000100010004433Q000A0001001226010500064Q00350003000500020010460002000200032Q009B000300023Q00069401030010000100010004433Q00100001001226010300083Q0010460002000700032Q0059000300033Q00102Q0002000900036Q00029Q008Q00017Q00023Q0003063Q0043726561746503043Q00506C6179030C4Q009B00035Q00208A0003000300012Q00D500055Q00063900060006000100010004433Q000600012Q009B000600014Q00D5000700024Q00A900030007000200202Q0004000300024Q0004000200014Q000300028Q00017Q00133Q0003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0054657874034Q0003083Q00466F6E744661636503043Q00466F6E742Q033Q006E657703043Q00456E756D030A3Q00466F6E7457656967687403043Q00426F6C6403073Q00526567756C6172030A3Q0054657874436F6C6F7233030A3Q00546578745363616C65642Q0103043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574026Q002C40042A4Q00B100045Q00122Q000500016Q00063Q000600302Q00060002000300062Q0007000700013Q0004433Q00070001001226010700053Q001046000600040007001275000700073Q00207B0107000700082Q009B000800013Q00068A0102001200013Q0004433Q00120001001275000900093Q00207B01090009000A00207B01090009000B00069401090015000100010004433Q00150001001275000900093Q00207B01090009000A00207B01090009000C2Q00350007000900020010460006000600070006390007001B000100030004433Q001B00012Q009B000700023Q00207B0107000700040010460006000D00070030A80006000E000F001275000700113Q00207B01070007001200063900080022000100010004433Q00220001001226010800133Q00063900090025000100010004433Q00250001001226010900134Q00350007000900020010460006001000072Q001D000400064Q002001046Q0066012Q00017Q00153Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q00284003083Q00506F736974696F6E03063Q00506172656E74026Q0028C003053Q007461626C6503063Q00696E7365727403063Q00697061697273030A3Q00496E707574426567616E03073Q00436F2Q6E656374030C3Q00496E7075744368616E67656402953Q00063900060003000100010004433Q000300012Q00E000066Q009B00075Q001226010800014Q00E000093Q00060030A80009000200030030A80009000400050030A8000900060007001275000A00093Q0020BF000A000A000A00122Q000B00073Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000C6Q000A000E000200104600090008000A001275000A00093Q0020BF000A000A000A00122Q000B000B3Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000B6Q000A000E00020010930009000D000A00102Q0009000E6Q0007000900024Q00085Q00122Q000900016Q000A3Q000600302Q000A0002000300302Q000A0004000500302Q000A0006000700122Q000B00093Q0020BF000B000B000A00122Q000C00073Q00122Q000D000B3Q00122Q000E000B3Q00122Q000F000C6Q000B000F0002001046000A0008000B001275000B00093Q0020BF000B000B000A00122Q000C000B3Q00122Q000D000B3Q00122Q000E00073Q00122Q000F000F6Q000B000F0002001093000A000D000B00102Q000A000E6Q0008000A00024Q00095Q00122Q000A00016Q000B3Q000600302Q000B0002000300302Q000B0004000500302Q000B0006000700122Q000C00093Q0020BF000C000C000A00122Q000D000B3Q00122Q000E000C3Q00122Q000F00073Q00122Q0010000B6Q000C00100002001046000B0008000C001275000C00093Q0020BF000C000C000A00122Q000D000B3Q00122Q000E000B3Q00122Q000F000B3Q00122Q0010000B6Q000C00100002001093000B000D000C00102Q000B000E6Q0009000B00024Q000A5Q00122Q000B00016Q000C3Q000600302Q000C0002000300302Q000C0004000500302Q000C0006000700122Q000D00093Q0020BF000D000D000A00122Q000E000B3Q00122Q000F000C3Q00122Q001000073Q00122Q0011000B6Q000D00110002001046000C0008000D001275000D00093Q0020BF000D000D000A00122Q000E00073Q00122Q000F000F3Q00122Q0010000B3Q00122Q0011000B6Q000D00110002001046000C000D000D001046000C000E4Q0035000A000C0002001275000B00103Q00207B010B000B00112Q00D5000C00064Q00D5000D00074Q00E1000B000D0001001275000B00103Q00207B010B000B00112Q00D5000C00064Q00D5000D00084Q00E1000B000D0001001275000B00103Q00207B010B000B00112Q00D5000C00064Q00D5000D00094Q00E1000B000D0001001275000B00103Q00207B010B000B00112Q00D5000C00064Q00D5000D000A4Q00E1000B000D0001001275000B00124Q00D5000C00064Q00A6010B0002000D0004433Q0088000100207B0110000F001300208A0010001000140006A300123Q000100042Q00D53Q00024Q00D53Q00044Q00D53Q00054Q00D58Q00E100100012000100207B0110000F001500208A0010001000140006A300120001000100012Q00D53Q00034Q00E10010001200010006C0000B007B000100020004433Q007B00012Q009B000B00014Q009B000C00023Q00207B010C000C00150006A3000D0002000100052Q00D53Q00034Q00D53Q00024Q00D53Q00044Q00D58Q00D53Q00054Q00E1000B000D00012Q0066012Q00013Q00033Q00073Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636803083Q00506F736974696F6E03073Q004368616E67656403073Q00436F2Q6E656374011A3Q00203B00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004433Q000C000100207B2Q013Q0001001275000200023Q00207B01020002000100207B0102000200040006842Q010019000100020004433Q001900012Q003C000100014Q00A22Q015Q00207B2Q013Q00052Q00A22Q0100014Q009B000100033Q00207B2Q01000100052Q00A22Q0100023Q00207B2Q013Q000600208A0001000100070006A300033Q000100022Q00D58Q009B8Q00E10001000300012Q0066012Q00013Q00013Q00033Q00030E3Q0055736572496E707574537461746503043Q00456E756D2Q033Q00456E64000A4Q00027Q00206Q000100122Q000100023Q00202Q00010001000100202Q00010001000300064Q0009000100010004433Q000900012Q003C8Q00A2012Q00014Q0066012Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F756368010E3Q00203B00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004433Q000C000100207B2Q013Q0001001275000200023Q00207B01020002000100207B0102000200040006842Q01000D000100020004433Q000D00012Q00A2017Q0066012Q00017Q00073Q0003083Q00506F736974696F6E03053Q005544696D322Q033Q006E657703013Q005803053Q005363616C6503063Q004F2Q6673657403013Q0059011F4Q009B00015Q000684012Q001E000100010004433Q001E00012Q009B000100013Q00068A2Q01001E00013Q0004433Q001E000100207B2Q013Q00012Q009B000200024Q00AA2Q01000100022Q009B000200033Q001275000300023Q00207B0103000300032Q009B000400043Q00207B01040004000400207B0104000400052Q009B000500043Q00207B01050005000400207B01050005000600207B0106000100042Q00B50005000500062Q009B000600043Q00207B01060006000700207B0106000600052Q009B000700043Q00207B01070007000700207B01070007000600207B0108000100072Q00B50007000700082Q00350003000700020010460002000100032Q0066012Q00017Q00043Q00030A3Q00496E707574426567616E03073Q00436F2Q6E656374030A3Q00496E707574456E646564030C3Q00496E7075744368616E67656402184Q003C00025Q0006A300033Q000100022Q00D58Q00D53Q00013Q00207B01043Q000100208A0004000400020006A300060001000100022Q00D53Q00024Q00D53Q00034Q00E10004000600012Q009B00046Q009B000500013Q00207B0105000500030006A300060002000100012Q00D53Q00024Q00E10004000600012Q009B00046Q009B000500013Q00207B0105000500040006A300060003000100022Q00D53Q00024Q00D53Q00034Q00E10004000600012Q0066012Q00013Q00043Q00093Q0003103Q004162736F6C757465506F736974696F6E030C3Q004162736F6C75746553697A6503043Q006D61746803053Q00636C616D7003083Q00506F736974696F6E03013Q0058028Q00026Q00F03F03013Q0059011F4Q009B00015Q00207B2Q01000100012Q009B00025Q00207B010200020002001275000300033Q00207B01030003000400207B01043Q000500207B01040004000600207B0105000100062Q00AA01040004000500207B0105000200062Q005D000400040005001226010500073Q001226010600084Q0035000300060002001275000400033Q00207B01040004000400207B01053Q000500207B01050005000900207B0106000100092Q00AA01050005000600207B0106000200092Q009700050005000600122Q000600073Q00122Q000700086Q0004000700024Q000500016Q000600036Q000700046Q0005000700016Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636801123Q00203B00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004433Q000C000100207B2Q013Q0001001275000200023Q00207B01020002000100207B0102000200040006842Q010011000100020004433Q001100012Q003C000100014Q00A22Q016Q009B000100014Q00D500026Q00580001000200012Q0066012Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F756368010F3Q00203B00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C000100020004433Q000C000100207B2Q013Q0001001275000200023Q00207B01020002000100207B0102000200040006842Q01000E000100020004433Q000E00012Q003C00016Q00A22Q016Q0066012Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F75636801134Q009B00015Q00068A2Q01001200013Q0004433Q0012000100207B2Q013Q0001001275000200023Q00207B01020002000100207B01020002000300068B0001000F000100020004433Q000F000100207B2Q013Q0001001275000200023Q00207B01020002000100207B0102000200040006842Q010012000100020004433Q001200012Q009B000100014Q00D500026Q00580001000200012Q0066012Q00017Q00093Q0003063Q0067657468756903053Q007063612Q6C030B3Q004C6F63616C506C61796572030C3Q0057616974466F724368696C6403093Q00506C61796572477569026Q00144003043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900223Q0012753Q00013Q00068A012Q000B00013Q0004433Q000B00010012753Q00023Q001275000100014Q00A6012Q0002000100068A012Q000B00013Q0004433Q000B000100068A2Q01000B00013Q0004433Q000B00012Q00D6000100023Q0012753Q00023Q00028200016Q00A6012Q0002000100068A012Q001300013Q0004433Q0013000100068A2Q01001300013Q0004433Q001300012Q00D6000100024Q009B00025Q00207B01020002000300068A0102001C00013Q0004433Q001C000100208A000300020004001226010500053Q001226010600064Q001D000300064Q002001035Q001275000300073Q00204E00030003000800122Q000500096Q000300056Q00039Q0000013Q00013Q00043Q0003083Q00636C6F6E6572656603043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900113Q0012753Q00013Q00068A012Q000B00013Q0004433Q000B00010012753Q00013Q0012592Q0100023Q00202Q00010001000300122Q000300046Q000100039Q00000200064Q000F000100010004433Q000F00010012753Q00023Q00208A5Q0003001226010200044Q00353Q000200022Q00D63Q00024Q0066012Q00017Q00023Q002Q033Q0073796E030B3Q0070726F746563745F677569000C3Q0012753Q00013Q00068A012Q000B00013Q0004433Q000B00010012753Q00013Q00207B014Q000200068A012Q000B00013Q0004433Q000B00010012753Q00013Q00207B014Q00022Q009B00016Q00583Q000200012Q0066012Q00017Q00433Q0003053Q00416C697665010003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C65030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B6579030F3Q004A756D70506F776572546F2Q676C6503083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E6703093Q0044697361626C65334403063Q0069706169727303083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030F3Q004175746F4461696C79526577617264030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E6503103Q004175746F486174636843756265452Q6703063Q004E6F636C6970030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030C3Q00496E66696E6974654A756D70030E3Q004D65726368616E74536E69706572030B3Q0057616C6B546F437562657303093Q004175746F517565737403073Q00456E61626C656403073Q0073652Q73696F6E026Q00F03F03043Q0073746F7003053Q007063612Q6C03053Q007461626C6503053Q00636C65617203093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403093Q004A756D70506F776572026Q004940030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F72656403093Q00776F726B7370616365030A3Q00426F2Q734D6F64656C73030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q004B657953797374656D426C757203073Q0044657374726F7903133Q0056656C6F63697479487562496E7374616E63650001C64Q009B00015Q00207B2Q01000100010006942Q010005000100010004433Q000500012Q0066012Q00014Q009B00015Q0030A8000100010002001275000100034Q00402Q010001000200207B01020001000400069401020010000100010004433Q0010000100207B01020001000500069401020010000100010004433Q0010000100207B01020001000600207B01030001000700207B01040001000800069401040015000100010004433Q0015000100207B01040001000900207B01050001000A0020D100060001000B00122Q0007000C6Q000800193Q00122Q0009000D3Q00122Q000A000E3Q00122Q000B000F3Q00122Q000C00103Q00122Q000D00113Q00122Q000E00123Q00122Q000F00133Q00122Q001000143Q00122Q001100153Q00122Q001200163Q00122Q001300053Q00122Q001400063Q00122Q001500173Q00122Q001600083Q00122Q001700093Q00122Q0018000A3Q00122Q001900183Q00122Q001A00193Q00122Q001B001A3Q00122Q001C001B3Q00122Q001D001C3Q00122Q001E001D3Q00122Q001F001E3Q00122Q0020001F3Q00122Q002100203Q00122Q002200213Q00122Q0023000B3Q00122Q002400223Q00122Q002500233Q00122Q002600043Q00122Q002700073Q00122Q002800243Q00122Q002900253Q00122Q002A00263Q00122Q002B00276Q0008002300012Q00A60107000200090004433Q0040000100207E2Q01000B00020006C00007003F000100020004433Q003F00012Q009B000700013Q0030500107002800024Q000700026Q000800023Q00202Q00080008002900202Q00080008002A00102Q0007002900084Q000700036Q000800033Q00202Q00080008002900202Q00080008002A00102Q0007002900084Q000700046Q000800043Q00202Q00080008002900202Q00080008002A00102Q0007002900084Q000700056Q000800053Q00202Q00080008002900202Q00080008002A00102Q0007002900084Q000700066Q000800063Q00202Q00080008002900202Q00080008002A00102Q0007002900084Q000700063Q00202Q00070007002B4Q0007000100014Q000700076Q00070001000100122Q0007000C6Q000800086Q00070002000900044Q006B0001001275000C002C3Q0006A3000D3Q000100012Q00D53Q000B4Q0058000C000200012Q00D4000A5Q0006C000070066000100020004433Q006600010012750007002D3Q00209E00070007002E4Q000800086Q0007000200014Q000700093Q00202Q00070007002F00062Q00080078000100070004433Q0078000100208A000800070030001226010A00314Q00350008000A000200068A0108008500013Q0004433Q0085000100068A0102008200013Q0004433Q0082000100208A0009000800320012F0000B00333Q00202Q000B000B00344Q0009000B00014Q0009000A3Q00102Q00080035000900068A0103008500013Q0004433Q008500010030A800080036003700068A0104008F00013Q0004433Q008F00010006550009008C000100070004433Q008C000100208A000900070038001226010B00394Q00350009000B000200068A0109008F00013Q0004433Q008F00010030A80009003A000200068A010500A900013Q0004433Q00A900010012750009003B3Q00208A000900090038001226010B003C4Q00350009000B000200068A010900A900013Q0004433Q00A90001001275000A000C3Q00208A000B0009003D2Q001C010B000C4Q0019000A3Q000C0004433Q00A7000100208A000F000E003E0012260111003F4Q0035000F0011000200068A010F00A400013Q0004433Q00A4000100208A000F000E0038001226011100394Q0035000F0011000200068A010F00A700013Q0004433Q00A700010030A8000F003A00020006C0000A009C000100020004433Q009C000100068A010600AF00013Q0004433Q00AF00010012750009002C3Q0006A3000A0001000100012Q009B3Q000B4Q00580009000200012Q009B0009000C3Q00208A000900090038001226010B00404Q00350009000B000200068A010900B700013Q0004433Q00B7000100208A000A000900412Q0058000A00020001001275000A00034Q0040010A0001000200207B010A000A00422Q009B000B5Q000684010A00C00001000B0004433Q00C00001001275000A00034Q0040010A000100020030A8000A00420043000694012Q00C5000100010004433Q00C500012Q009B000A000D3Q00208A000A000A00412Q0058000A000200012Q0066012Q00013Q00023Q00013Q00030A3Q00446973636F2Q6E65637400044Q009B7Q00208A5Q00012Q00583Q000200012Q0066012Q00017Q00013Q0003153Q00536574336452656E646572696E67456E61626C656400054Q009B7Q00208A5Q00012Q003C000200014Q00E13Q000200012Q0066012Q00019Q003Q00044Q009B8Q003C00016Q00583Q000200012Q0066012Q00019Q003Q00044Q009B8Q003C000100014Q00583Q000200012Q0066012Q00017Q003C3Q0003083Q00496E7374616E63652Q033Q006E6577030A3Q00426C7572452Q6665637403043Q004E616D65030D3Q004B657953797374656D426C757203043Q0053697A65026Q00344003063Q00506172656E74028Q0003053Q004672616D65030E3Q004B657953797374656D4672616D65030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E03053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240025Q00C0624003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03063Q004163746976652Q01026Q00184003063Q005374726F6B65026Q00304003093Q00546578744C6162656C03043Q005465787403173Q004B65792053797374656D20566572696669636174696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40026Q002840026Q00F03F03093Q005365636F6E64617279025Q00804640026Q006E40026Q003E40026Q00104003073Q0054657874426F78034Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q005375625465787403103Q00436C656172546578744F6E466F6375730100026Q0024C0026Q001440030A3Q005465787442752Q746F6E030A3Q005375626D6974204B657903063Q00412Q63656E74025Q00805640030F3Q004175746F42752Q746F6E436F6C6F7203093Q0041637469766174656403073Q00436F2Q6E65637402E23Q001290000200013Q00202Q00020002000200122Q000300036Q00020002000200302Q00020004000500302Q0002000600074Q00035Q00102Q00020008000300122Q000300096Q000400013Q00122Q0005000A6Q00063Q000900302Q00060004000B00122Q0007000D3Q00202Q00070007000200122Q0008000E3Q00122Q0009000E6Q00070009000200102Q0006000C000700122Q000700103Q00202Q00070007000200122Q0008000E3Q00122Q000900093Q00122Q000A000E3Q00122Q000B00096Q0007000B000200102Q0006000F000700122Q000700103Q00202Q00070007001100122Q000800123Q00122Q000900136Q00070009000200102Q0006000600074Q000700023Q00202Q00070007001500102Q00060014000700302Q00060016001700302Q00060018000900302Q00060019001A4Q000700033Q00102Q0006000800074Q0004000600024Q000500046Q000600043Q00122Q0007001B6Q0005000700014Q000500056Q000600046Q000700023Q00202Q00070007001C00122Q0008000E6Q0005000800014Q000500066Q000600043Q00122Q0007001D3Q00122Q0008000E6Q0005000800014Q000500013Q00122Q0006001E6Q00073Q000800302Q0007001F00204Q000800073Q00102Q0007002100084Q000800023Q00202Q00080008001F00102Q00070022000800302Q00070023002400122Q000800103Q00202Q00080008000200122Q000900093Q00122Q000A00093Q00122Q000B00093Q00122Q000C00256Q0008000C000200102Q0007000F000800122Q000800103Q00202Q00080008000200122Q000900263Q00122Q000A00093Q00122Q000B00093Q001226010C00074Q00350008000C00020010460007000600080030A80007001600260010460007000800042Q00350005000700022Q009B000600013Q0012260107000A4Q00C300083Q00064Q000900023Q00202Q00090009002700102Q00080014000900122Q0009000D3Q00202Q00090009000200122Q000A000E3Q00122Q000B00096Q0009000B000200102Q0008000C0009001275000900103Q0020BF00090009000200122Q000A000E3Q00122Q000B00093Q00122Q000C00093Q00122Q000D00286Q0009000D00020010460008000F0009001275000900103Q0020BF00090009000200122Q000A00093Q00122Q000B00293Q00122Q000C00093Q00122Q000D002A6Q0009000D000200109301080006000900302Q00080018000900102Q0008000800044Q0006000800024Q000700046Q000800063Q00122Q0009002B6Q0007000900014Q000700056Q000800064Q009B000900023Q00204901090009001C4Q000A00086Q0007000A00024Q000800013Q00122Q0009002C6Q000A3Q000B00302Q000A0016002600302Q000A001F002D00302Q000A002E002F4Q000B00023Q00207B010B000B0031001046000A0030000B2Q009B000B00093Q001046000A0021000B2Q009B000B00023Q00207B010B000B001F001046000A0022000B0030A8000A002300250030A8000A00320033001275000B00103Q0020BF000B000B000200122Q000C00263Q00122Q000D00343Q00122Q000E00263Q00122Q000F00096Q000B000F0002001046000A0006000B001275000B00103Q0020BF000B000B000200122Q000C00093Q00122Q000D00353Q00122Q000E00093Q00122Q000F00096Q000B000F0002001046000A000F000B001046000A000800062Q00350008000A00022Q009B000900013Q001226010A00364Q00E0000B3Q000A0030A8000B001F00372Q009B000C00073Q001046000B0021000C2Q009B000C00023Q00207B010C000C001F001062010B0022000C00302Q000B002300254Q000C00023Q00202Q000C000C003800102Q000B0014000C00122Q000C000D3Q00202Q000C000C000200122Q000D000E3Q00122Q000E00096Q000C000E0002001046000B000C000C001275000C00103Q0020BF000C000C000200122Q000D000E3Q00122Q000E00093Q00122Q000F00093Q00122Q001000396Q000C00100002001046000B000F000C001275000C00103Q0020BF000C000C000200122Q000D00093Q00122Q000E00293Q00122Q000F00093Q00122Q0010002A6Q000C00100002001046000B0006000C0030A8000B003A0033001046000B000800042Q00350009000B00022Q009B000A00044Q00D5000B00093Q001226010C002B4Q00E1000A000C00010006A3000A3Q0001000B2Q00D53Q00084Q00D58Q00D53Q00024Q009B3Q000A4Q00D53Q00044Q009B3Q000B4Q00D53Q00014Q00D53Q00034Q009B3Q000C4Q00D53Q00074Q009B3Q00023Q002044000B0009003B00202Q000B000B003C4Q000D000A6Q000B000D00014Q000B000D6Q000C00046Q000D00016Q000E00056Q000D000100012Q00E1000B000D00012Q0066012Q00013Q00013Q00173Q0003043Q005465787403073Q0044657374726F7903043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240028Q0003043Q007461736B03043Q0077616974029A5Q99C93F026Q00F03F026Q00084003043Q004B69636B030B3Q00496E76616C6964204B6579034Q00030F3Q00506C616365686F6C6465725465787403063Q00737472696E6703063Q00666F726D617403133Q00496E76616C6964204B657921202825642F332903053Q00436F6C6F7203073Q005761726E696E6703053Q0064656C6179026Q00F83F004A4Q009B7Q00207B014Q00012Q009B000100013Q000684012Q001D000100010004433Q001D00012Q009B3Q00023Q00206E014Q00026Q000200016Q00036Q000100046Q000200056Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300046Q0003000100124Q00083Q00206Q000900122Q0001000A8Q000200016Q00043Q00206Q00026Q000200016Q00068Q0001000100044Q004900012Q009B3Q00073Q002091014Q000B2Q00A2012Q00074Q009B3Q00073Q000E2F010C002E00013Q0004433Q002E00012Q009B3Q00023Q00068A012Q002900013Q0004433Q002900012Q009B3Q00023Q00208A5Q00022Q00583Q000200012Q009B3Q00083Q00208A5Q000D0012260102000E4Q00E13Q000200012Q0066012Q00014Q009B7Q0030033Q0001000F9Q0000122Q000100113Q00202Q00010001001200122Q000200136Q000300076Q00010003000200104Q001000016Q00036Q000100096Q000200056Q00033Q00014Q0004000A3Q00202Q00040004001500102Q0003001400046Q0003000100124Q00083Q00206Q001600122Q000100173Q0006A300023Q000100052Q009B8Q009B3Q00034Q009B3Q00094Q009B3Q00054Q009B3Q000A4Q00E13Q000200012Q0066012Q00013Q00013Q00043Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03053Q00436F6C6F7203063Q005374726F6B65000B4Q009B7Q0030A83Q000100022Q005C012Q00016Q000100026Q000200036Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00643Q0003063Q00412Q63656E7403043Q005461627303083Q005F63752Q72656E740003083Q005F746F2Q676C657303053Q004672616D6503043Q004E616D6503063Q0057696E646F77030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407A40026Q00744003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03103Q00436C69707344657363656E64616E74732Q0103063Q0041637469766503063Q00506172656E74026Q00184003063Q005374726F6B65026Q00304003063Q00546F7042617203093Q005365636F6E64617279026Q00F03F026Q00414003063Q005A496E646578026Q001440026Q0020400285EB51B81E85EB3F03043Q006D2Q6F6E026Q00324003063Q00507572706C6503093Q00546578744C6162656C03053Q005469746C6503043Q0054657874030C3Q0056656C6F636974792048756203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q002Q40026Q0059C0026Q00344003013Q0078026Q0020C003063Q00436F6C6F723303073Q0066726F6D524742025Q00E06F40025Q0080564003053Q006D696E7573026Q0041C003083Q006B6579626F617264026Q004EC003093Q0041637469766174656403073Q00436F2Q6E65637403043Q00426F6479030E3Q005363726F2Q6C696E674672616D6503073Q005461624C697374029A5Q99C93F025Q00C05C40030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q7303103Q005363726F2Q6C696E67456E61626C6564030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D026Q00084003093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E67526967687403073Q00436F6E74656E74026Q005D40026Q005DC0030B3Q005369646544697669646572027Q004003043Q007461736B03053Q00737061776E030A3Q00496E707574426567616E03093Q0043726561746554616202A4012Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100023Q00207B01020001000100068A0102000A00013Q0004433Q000A00012Q009B00025Q00207B0103000100010010460002000100032Q00E000023Q00032Q00E000035Q0010460002000200030030A80002000300042Q00E000035Q0010460002000500032Q009B000300013Q001226010400064Q00E000053Q000A0030A80005000700080012960106000A3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000C6Q00060008000200102Q00050009000600122Q0006000E3Q0020BF00060006000B00122Q0007000C3Q00122Q0008000F3Q00122Q0009000C3Q00122Q000A000F6Q0006000A00020010B40005000D000600122Q0006000E3Q00202Q00060006001100122Q000700123Q00122Q000800134Q00350006000800020010460005001000062Q009B00065Q00207B0106000600150010460005001400060030A80005001600170030A800050018000F0030A800050019001A0030A80005001B001A2Q009B000600023Q0010460005001C00062Q00350003000500022Q009B000400034Q00D5000500033Q0012260106001D4Q00E10004000600012Q009B000400044Q00FB000500036Q00065Q00202Q00060006001E00122Q0007000C6Q0004000700014Q000400056Q000500033Q00122Q0006001F3Q00122Q0007000C6Q0004000700012Q009B000400013Q001226010500064Q00E000063Q00080030A80006000700202Q009B00075Q00207B0107000700210010460006001400070030A80006001600170012750007000E3Q0020BF00070007000B00122Q000800223Q00122Q0009000F3Q00122Q000A000F3Q00122Q000B00236Q0007000B000200109E01060010000700302Q00060018000F00302Q00060024002500302Q0006001B001A00102Q0006001C00034Q0004000600024Q000500046Q000600046Q00075Q00202Q00070007001E2Q009B000800064Q00E10005000800012Q00F7000500056Q000600043Q00122Q000700263Q00122Q000800276Q0005000800014Q000500073Q00122Q000600283Q00122Q000700296Q000800016Q00095Q00202Q00090009002A4Q00050009000200122Q0006000A3Q00202Q00060006000B00122Q0007000F3Q00122Q0008000C6Q00060008000200102Q00050009000600122Q0006000E3Q00202Q00060006000B00122Q0007000F3Q00122Q000800263Q00122Q0009000C3Q00122Q000A000F6Q0006000A000200102Q0005000D000600102Q0005001C00044Q000600013Q00122Q0007002B6Q00083Q000C00302Q00080007002C00202Q00090001002C00062Q0009007F000100010004433Q007F00010012260109002E3Q0010460008002D00092Q0051000900083Q00102Q0008002F00094Q00095Q00202Q00090009002D00102Q00080030000900302Q00080031003200122Q000900343Q00202Q00090009003300202Q00090009003500102Q000800330009001275000900343Q00204200090009003600202Q00090009003700102Q00080036000900302Q00080016002200122Q0009000A3Q00202Q00090009000B00122Q000A000F3Q00122Q000B000C6Q0009000B000200102Q0008000900090012750009000E3Q0020BF00090009000B00122Q000A000F3Q00122Q000B00383Q00122Q000C000C3Q00122Q000D000F6Q0009000D00020010460008000D00090012750009000E3Q0020BF00090009000B00122Q000A00223Q00122Q000B00393Q00122Q000C000F3Q00122Q000D003A6Q0009000D00020010460008001000090010460008001C00042Q00350006000800020006A300073Q000100072Q009B3Q00014Q009B8Q00D53Q00044Q009B3Q00034Q009B3Q00074Q009B3Q00094Q009B3Q000A4Q0078010800073Q00122Q0009003B3Q00122Q000A003C3Q00122Q000B003D3Q00202Q000B000B003E00122Q000C003F3Q00122Q000D00403Q00122Q000E00406Q000B000E6Q00083Q00024Q000900073Q00122Q000A00413Q00122Q000B00426Q000C5Q00202Q000C000C002D4Q0009000C00024Q000A00073Q00122Q000B00433Q00122Q000C00446Q000D5Q00202Q000D000D002A4Q000A000D00024Q000B5Q00202Q000C000A004500202Q000C000C00460006A3000E00010001000D2Q009B3Q00024Q00D53Q000B4Q009B3Q00014Q009B8Q009B3Q00034Q009B3Q00044Q009B3Q00054Q009B3Q00084Q009B3Q000B4Q009B3Q000C4Q009B3Q00094Q009B3Q000A4Q009B3Q000D4Q00E1000C000E00012Q009B000C00013Q001226010D00064Q00E0000E3Q00050030A8000E000700470030A8000E00160022001275000F000E3Q0020BF000F000F000B00122Q0010000F3Q00122Q0011000F3Q00122Q0012000F3Q00122Q001300236Q000F00130002001046000E000D000F001275000F000E3Q0020BF000F000F000B00122Q001000223Q00122Q0011000F3Q00122Q001200223Q00122Q001300426Q000F0013000200108D010E0010000F00102Q000E001C00034Q000C000E00024Q000D00013Q00122Q000E00486Q000F3Q000B00302Q000F000700494Q00105Q00202Q00100010002100102Q000F001400100030A8000F0016004A0030A8000F0018000F0012750010000E3Q0020BF00100010000B00122Q0011000F3Q00122Q0012004B3Q00122Q001300223Q00122Q0014000F6Q001000140002001046000F001000100012750010000E3Q00207B01100010000B2Q0040011000010002001046000F004C0010001275001000343Q00207B01100010004E00207B01100010004F001046000F004D00100030A8000F0050000F0030A8000F0051001A00303B010F001B001A00102Q000F001C000C4Q001000016Q001100013Q00122Q001200526Q00133Q000200122Q001400543Q00202Q00140014000B00122Q0015000F3Q00122Q001600554Q0035001400160002001046001300530014001275001400343Q00207B01140014005600207B0114001400570010460013005600142Q00350011001300022Q009B001200013Q001226011300584Q00E000143Q0003001296011500543Q00202Q00150015000B00122Q0016000F3Q00122Q0017001D6Q00150017000200102Q00140059001500122Q001500543Q00207B01150015000B0012260116000F3Q0012260117001D4Q003F00150017000200102Q0014005A001500122Q001500543Q00202Q00150015000B00122Q0016000F3Q00122Q0017001D6Q00150017000200102Q0014005B00154Q001200146Q00103Q00012Q0035000D001000022Q009B000E00013Q001226010F00064Q00E000103Q00050030A800100007005C0030A80010001600220012750011000E3Q0020BF00110011000B00122Q0012000F3Q00122Q0013005D3Q00122Q0014000F3Q00122Q0015000F6Q0011001500020010460010000D00110012750011000E3Q0020BF00110011000B00122Q001200223Q00122Q0013005E3Q00122Q001400223Q00122Q0015000F6Q00110015000200108D01100010001100102Q0010001C000C4Q000E001000024Q000F00013Q00122Q001000066Q00113Q000800302Q00110007005F4Q00125Q00202Q00120012001E00102Q0011001400122Q009B001200063Q0010460011001600120030A800110018000F0012750012000E3Q0020BF00120012000B00122Q0013000F3Q00122Q0014004B3Q00122Q0015000F3Q00122Q0016000F6Q0012001600020010460011000D00120012750012000E3Q0020BF00120012000B00122Q0013000F3Q00122Q001400223Q00122Q001500223Q00122Q0016000F6Q00120016000200108300110010001200302Q00110024006000102Q0011001C000C4Q000F001100014Q000F000D6Q001000036Q001100036Q001200046Q001300066Q001400054Q00C40011000300012Q00E1000F00110001001275000F00613Q00207B010F000F00620006A300100002000100022Q009B3Q000E4Q00D53Q00024Q0058000F0002000100207B010F0008004500208A000F000F00460006A300110003000100052Q009B3Q000F4Q009B3Q00094Q00D53Q00034Q009B3Q000A4Q009B3Q00024Q00E1000F001100012Q003C000F5Q00207B01100009004500208A0010001000460006A300120004000100052Q00D53Q000F4Q00D53Q000C4Q009B3Q00094Q00D53Q00034Q009B3Q00104Q008C0010001200014Q00108Q0011000B6Q0012000C3Q00202Q0012001200630006A300130005000100032Q00D53Q000B4Q00D53Q00104Q00D53Q00034Q00E10011001300010006A300110006000100102Q009B3Q00014Q009B8Q00D53Q000D4Q009B3Q00034Q009B3Q00074Q009B3Q00114Q00D53Q000E4Q00D53Q00024Q009B3Q00094Q009B3Q00104Q009B3Q000A4Q009B3Q00044Q009B3Q00064Q009B3Q00084Q009B3Q00124Q009B3Q00133Q0010460002006400112Q00D6000200024Q0066012Q00013Q00073Q001C3Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574026Q00364003063Q005A496E646578026Q00184003063Q00506172656E74026Q001440026Q002A4003073Q0053756254657874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503534Q00FF00035Q00122Q000400016Q00053Q000900302Q00050002000300302Q0005000400054Q000600013Q00202Q00060006000700102Q00050006000600302Q00050008000900122Q0006000B3Q0020CD00060006000C00122Q000700093Q00122Q0008000D6Q00060008000200102Q0005000A000600122Q0006000F3Q00207B01060006000C001226010700094Q00D5000800013Q0012260109000D3Q001226010A00104Q00350006000A00020010B40005000E000600122Q0006000F3Q00202Q00060006001200122Q000700133Q00122Q000800134Q00350006000800020010460005001100060030A80005001400152Q009B000600023Q0010460005001600062Q00350003000500022Q0069000400036Q000500033Q00122Q000600176Q0004000600014Q000400046Q00055Q00122Q000600186Q00078Q000800013Q00202Q0008000800192Q00350004000800020012960105000B3Q00202Q00050005000C00122Q0006000D3Q00122Q0007000D6Q00050007000200102Q0004000A000500122Q0005000F3Q0020BF00050005000C00122Q0006000D3Q00122Q000700103Q00122Q0008000D3Q00122Q000900106Q0005000900020010460004000E00050030A800040014001500104600040016000300207B01050003001A00208A00050005001B0006A300073Q000100062Q009B3Q00054Q00D53Q00034Q009B3Q00064Q00D53Q00044Q00D53Q00024Q009B3Q00014Q00E100050007000100207B01050003001C00208A00050005001B0006A300070001000100052Q009B3Q00054Q00D53Q00034Q009B3Q00064Q00D53Q00044Q009B3Q00014Q003C0105000700014Q000500036Q000600046Q000500038Q00013Q00023Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379028Q00030A3Q0054657874436F6C6F723303043Q005465787400124Q00229Q00000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00062Q0004000F000100010004433Q000F00012Q009B000400053Q00207B0104000400040010460003000300042Q00E13Q000300012Q0066012Q00017Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030A3Q0054657874436F6C6F723303073Q0053756254657874000F4Q009B8Q0056000100016Q000200026Q00033Q000100302Q0003000100026Q000300012Q005C019Q000100036Q000200026Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00463Q00030E3Q0046696E6446697273744368696C6403143Q004B657962696E645069636B65724F7665726C617903073Q0044657374726F7903053Q004672616D6503043Q004E616D65030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407040025Q00805B4003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E64030F3Q00426F7264657253697A65506978656C03063Q005A496E646578026Q00594003063Q004163746976652Q0103063Q00506172656E74026Q00204003063Q00507572706C65029A5Q99C93F026Q00F83F026Q003440029A5Q99D93F03093Q00546578744C6162656C03043Q005465787403173Q00437573746F6D204B657962696E642053652Q74696E677303083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40026Q002440026Q00F03F026Q00324003163Q004261636B67726F756E645472616E73706172656E6379025Q0040594003093Q005365636F6E64617279026Q004240025Q00806B40026Q003E40026Q001440030A3Q0043552Q52454E543A205B03063Q00737472696E6703053Q00752Q70657203073Q0067657467656E7603093Q00546F2Q676C654B657903123Q005D0A5052452Q5320414E59204B45593Q2E026Q002640030E3Q005465787458416C69676E6D656E7403043Q00456E756D03063Q0043656E746572025Q00805940030A3Q005465787442752Q746F6E03063Q0043616E63656C03073Q005375625465787403073Q00456C656D656E74025Q00805240026Q003640030F3Q004175746F42752Q746F6E436F6C6F720100026Q00104003093Q0041637469766174656403073Q00436F2Q6E656374030A3Q00496E707574426567616E00F14Q001F016Q00206Q000100122Q000200028Q0002000200064Q000B00013Q0004433Q000B000100208A00013Q00032Q00580001000200012Q003C00016Q00A22Q0100014Q0066012Q00014Q003C000100014Q00AC2Q0100016Q000100023Q00122Q000200046Q00033Q000900302Q00030005000200122Q000400073Q00202Q00040004000800122Q000500093Q00122Q000600096Q0004000600020010460003000600040012750004000B3Q0020BF00040004000800122Q000500093Q00122Q0006000C3Q00122Q000700093Q00122Q0008000C6Q0004000800020010B40003000A000400122Q0004000B3Q00202Q00040004000E00122Q0005000F3Q00122Q000600104Q00350004000600020010460003000D00042Q009B000400033Q00207B0104000400120010460003001100040030A800030013000C0030A80003001400150030A80003001600172Q009B00045Q0010460003001800042Q00350001000300022Q009B000200044Q00D5000300013Q001226010400194Q00E10002000400012Q009B000200054Q00D5000300014Q009B000400033Q00207B01040004001A0012260105001B3Q0012260106001C4Q00E10002000600012Q009B000200064Q00D5000300013Q0012260104001D3Q0012260105001E4Q00E10002000500012Q009B000200023Q0012600003001F6Q00043Q000900302Q0004002000214Q000500073Q00102Q0004002200054Q000500033Q00202Q00050005002000102Q00040023000500302Q00040024002500122Q0005000B3Q0020BF00050005000800122Q0006000C3Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900266Q0005000900020010460004000A00050012750005000B3Q0020BF00050005000800122Q000600273Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900286Q0005000900020010290004000D000500302Q00040029002700302Q00040014002A00102Q0004001800014Q0002000400024Q000300023Q00122Q000400046Q00053Q00074Q000600033Q00202Q00060006002B0010B400050011000600122Q000600073Q00202Q00060006000800122Q000700093Q00122Q0008000C4Q00350006000800020010460005000600060012750006000B3Q0020BF00060006000800122Q000700093Q00122Q0008000C3Q00122Q0009000C3Q00122Q000A002C6Q0006000A00020010460005000A00060012750006000B3Q0020BF00060006000800122Q0007000C3Q00122Q0008002D3Q00122Q0009000C3Q00122Q000A002E6Q0006000A00020010460005000D00060030A800050013000C0030A800050014002A0010460005001800012Q00350003000500022Q009B000400044Q00D5000500033Q0012260106002F4Q00E10004000600012Q009B000400054Q00D5000500034Q005D010600033Q00202Q00060006001A00122Q000700096Q0004000700024Q000500023Q00122Q0006001F6Q00073Q000900122Q000800303Q00122Q000900313Q00202Q000900090032001275000A00334Q0040010A0001000200207B010A000A003400207B010A000A00052Q0031010900020002001226010A00354Q00F200080008000A0010460007002000082Q0051000800073Q00102Q0007002200084Q000800033Q00202Q00080008001A00102Q00070023000800302Q00070024003600122Q000800383Q00202Q00080008003700202Q00080008003900102Q0007003700080012750008000B3Q0020BF00080008000800122Q000900273Q00122Q000A000C3Q00122Q000B00273Q00122Q000C000C6Q0008000C00020010460007000D00080030A80007002900270030A800070014003A0010460007001800032Q00350005000700022Q009B000600023Q0012260107003B4Q00E000083Q000B0030A800080020003C2Q009B000900073Q0010460008002200092Q009B000900033Q00207B01090009003D00106201080023000900302Q0008002400364Q000900033Q00202Q00090009003E00102Q00080011000900122Q000900073Q00202Q00090009000800122Q000A00093Q00122Q000B000C6Q0009000B00020010460008000600090012750009000B3Q0020BF00090009000800122Q000A00093Q00122Q000B000C3Q00122Q000C000C3Q00122Q000D003F6Q0009000D00020010460008000A00090012750009000B3Q0020BF00090009000800122Q000A000C3Q00122Q000B00153Q00122Q000C000C3Q00122Q000D00406Q0009000D00020010460008000D00090030A80008004100420030A800080014002A0010460008001800012Q00350006000800022Q009B000700044Q00D5000800063Q001226010900434Q00E10007000900012Q00CA000700073Q0006A300083Q000100032Q00D53Q00074Q00D53Q00014Q009B3Q00013Q00202600090006004400202Q0009000900454Q000B00086Q0009000B00014Q000900086Q000A00093Q00202Q000A000A00460006A3000B0001000100062Q009B3Q00014Q00D53Q00084Q00D53Q00054Q009B3Q000A4Q00D53Q00044Q009B3Q000B4Q00720109000B00024Q000700096Q0009000C6Q000A00016Q000B00016Q000C00026Q000B000100012Q00E10009000B00012Q0066012Q00013Q00023Q00023Q00030A3Q00446973636F2Q6E65637403073Q0044657374726F7900114Q009B7Q00068A012Q000800013Q0004433Q000800012Q009B7Q00208A5Q00012Q00583Q000200012Q00CA8Q00A2017Q009B3Q00013Q00068A012Q000E00013Q0004433Q000E00012Q009B3Q00013Q00208A5Q00022Q00583Q000200012Q003C8Q00A2012Q00024Q0066012Q00017Q00163Q00030D3Q0055736572496E7075745479706503043Q00456E756D03083Q004B6579626F61726403073Q0067657467656E7603093Q00546F2Q676C654B657903073Q004B6579436F646503043Q0054657874030E3Q004E4557204B455942494E443A205B03063Q00737472696E6703053Q00752Q70657203043Q004E616D6503013Q005D030A3Q0054657874436F6C6F723303063Q00436F6C6F723303073Q0066726F6D524742026Q005E40025Q00E06F40026Q00644003053Q00436F6C6F7203043Q007461736B03043Q0077616974026Q00E03F02354Q009B00025Q00069401020006000100010004433Q000600012Q009B000200014Q00320002000100012Q0066012Q00013Q00207B01023Q0001001275000300023Q00207B01030003000100207B01030003000300068401020034000100030004433Q00340001001275000200044Q00FA00020001000200202Q00033Q000600102Q0002000500034Q000200023Q00122Q000300083Q00122Q000400093Q00202Q00040004000A00202Q00053Q000600202Q00050005000B4Q0004000200020012260105000C4Q00F20003000300050010460002000700032Q009B000200023Q0012750003000E3Q00207B01030003000F001226010400103Q001226010500113Q001226010600124Q00350003000600020010950002000D00034Q000200036Q000300046Q000400056Q00053Q000100122Q0006000E3Q00207B01060006000F001226010700103Q001226010800113Q001226010900124Q00350006000900020010460005001300062Q00E1000200050001001234010200143Q00202Q00020002001500122Q000300166Q0002000200014Q000200016Q0002000100012Q0066012Q00017Q00083Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00D03F03063Q0069706169727303083Q005F746F2Q676C657303053Q007063612Q6C03043Q0053796E6300154Q009B7Q00207B014Q000100068A012Q001400013Q0004433Q001400010012753Q00023Q00208F014Q000300122Q000100048Q0002000100124Q00056Q000100013Q00202Q0001000100066Q0002000200044Q00110001001275000500073Q00207B0106000400082Q00D5000700044Q00E10005000700010006C03Q000D000100020004433Q000D00010004435Q00012Q0066012Q00017Q000A3Q0003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574030C3Q004162736F6C75746553697A6503013Q0058028Q0003043Q007461736B03043Q0077616974020AD7A3703D0AC73F03073Q0044657374726F7900184Q0024019Q000100018Q000200016Q00016Q000100026Q000200036Q00033Q000100122Q000400023Q00202Q0004000400034Q000500023Q00207B01050005000400207B010500050005001226010600064Q00350004000600020010460003000100042Q00E13Q000300010012753Q00073Q00207B014Q00080012262Q0100094Q00583Q000200012Q009B3Q00043Q00208A5Q000A2Q00583Q000200012Q0066012Q00017Q000C3Q0003073Q0056697369626C65010003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00407A40026Q004140026Q00744003043Q007461736B03043Q007761697402B81E85EB51B8BE3F2Q0100264Q008B019Q009Q009Q006Q00064Q001400013Q0004433Q001400012Q009B3Q00013Q003082012Q000100026Q00026Q000100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q0004000600020010460003000300042Q00E13Q000300010004433Q002500012Q009B3Q00024Q009B000100034Q009B000200044Q00E000033Q0001001275000400043Q00204C00040004000500122Q000500063Q00122Q000600086Q00040006000200102Q0003000300042Q00E13Q000300010012753Q00093Q00207B014Q000A0012262Q01000B4Q00583Q000200012Q009B3Q00013Q0030A83Q0001000C2Q0066012Q00017Q00063Q0003073Q004B6579436F646503073Q0067657467656E7603093Q00546F2Q676C654B657903043Q00456E756D03013Q004B03073Q0056697369626C6502193Q0006942Q010005000100010004433Q000500012Q009B00025Q00068A0102000600013Q0004433Q000600012Q0066012Q00013Q00207B01023Q0001001275000300024Q004001030001000200207B0103000300030006940103000F000100010004433Q000F0001001275000300043Q00207B01030003000100207B01030003000500068401020018000100030004433Q001800012Q009B000200014Q007B000200024Q00A2010200014Q009B000200024Q009B000300014Q007B000300033Q0010460002000600032Q0066012Q00017Q00593Q0003063Q005F6F72646572028Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577026Q003C4003073Q0056697369626C6503063Q0048692Q64656E03063Q00506172656E74026Q00144003093Q0049636F6E436F6C6F7203073Q005375625465787403043Q0049636F6E03063Q00636972636C65026Q002E40030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q00184003093Q00546578744C6162656C03043Q004E616D652Q033Q0054616203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q003A40026Q003EC003053Q004672616D6503043Q0050616765030F3Q00426F7264657253697A65506978656C030E3Q005363726F2Q6C696E674672616D65030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q73027Q004003143Q005363726F2Q6C426172496D616765436F6C6F723303063Q005374726F6B65031A3Q005363726F2Q6C426172496D6167655472616E73706172656E6379030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D03093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70026Q002040030D3Q0050612Q64696E67426F2Q746F6D030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E675269676874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403043Q005F62746E03053Q005F69636F6E03053Q005F6E616D6503053Q005F7061676503053Q005F7772617003073Q005F73656C656374030A3Q005F69636F6E436F6C6F7203053Q007461626C6503063Q00696E7365727403043Q0054616273030D3Q0043726561746553656374696F6E030C3Q00437265617465546F2Q676C65030C3Q00437265617465536C69646572030D3Q0043726561746554657874626F78030B3Q004372656174654C6162656C030C3Q0043726561746542752Q746F6E03123Q00437265617465436F726E657242752Q746F6E030C3Q00437265617465427579526F770257012Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100024Q00E000023Q00010030A80002000100022Q00FF00035Q00122Q000400036Q00053Q000700302Q00050004000500302Q0005000600074Q000600013Q00202Q00060006000900102Q00050008000600302Q0005000A000B00122Q0006000D3Q0020BF00060006000E00122Q0007000B3Q00122Q000800023Q00122Q000900023Q00122Q000A000F6Q0006000A00020010460005000C000600207B0106000100112Q007B000600063Q0010460005001000062Q009B000600023Q0010460005001200062Q00350003000500022Q009B000400034Q00D5000500033Q001226010600134Q00E100040006000100207B01040001001400069401040026000100010004433Q002600012Q009B000400013Q00207B0104000400152Q009B000500043Q00207B0106000100160006940106002B000100010004433Q002B0001001226010600173Q001226010700184Q004100088Q000900046Q00050009000200122Q0006001A3Q00202Q00060006000E00122Q000700023Q00122Q0008001B6Q00060008000200102Q00050019000600122Q0006000D3Q00202Q00060006000E00122Q000700023Q00122Q0008001D3Q00122Q0009001B3Q00122Q000A00026Q0006000A000200102Q0005001C000600102Q0005001200034Q00065Q00122Q0007001E6Q00083Q000B00302Q0008000A000B00202Q00090001001F00062Q00090046000100010004433Q00460001001226010900203Q0010460008000400092Q0051000900053Q00102Q0008002100094Q000900013Q00202Q00090009001500102Q00080022000900302Q00080023002400122Q000900263Q00202Q00090009002500202Q00090009002700102Q000800250009001275000900263Q00207B01090009002800207B0109000900290010B400080028000900122Q0009001A3Q00202Q00090009000E00122Q000A00023Q00122Q000B001B4Q00350009000B00020010460008001900090012750009000D3Q0020BF00090009000E00122Q000A00023Q00122Q000B002A3Q00122Q000C001B3Q00122Q000D00026Q0009000D00020010460008001C00090012750009000D3Q0020BF00090009000E00122Q000A000B3Q00122Q000B002B3Q00122Q000C000B3Q00122Q000D00026Q0009000D00020010930008000C000900102Q0008001200034Q0006000800024Q00075Q00122Q0008002C6Q00093Q000600302Q0009001F002D00302Q0009000A000B00302Q0009002E000200122Q000A000D3Q0020BF000A000A000E00122Q000B000B3Q00122Q000C00023Q00122Q000D000B3Q00122Q000E00026Q000A000E00020010890009000C000A00302Q0009001000074Q000A00063Q00102Q00090012000A4Q0007000900024Q00085Q00122Q0009002F6Q000A3Q000900302Q000A000A000B00302Q000A002E0002001275000B000D3Q0020BF000B000B000E00122Q000C000B3Q00122Q000D00023Q00122Q000E000B3Q00122Q000F00026Q000B000F0002001046000A000C000B001275000B000D3Q00207B010B000B000E2Q0040010B00010002001046000A0030000B001275000B00263Q00207B010B000B003200207B010B000B0033001046000A0031000B0030A8000A003400352Q009B000B00013Q002001000B000B003700102Q000A0036000B00302Q000A0038001B00102Q000A001200074Q000B00016Q000C5Q00122Q000D00396Q000E3Q000200122Q000F003B3Q00202Q000F000F000E001226011000023Q00126A0011001D6Q000F0011000200102Q000E003A000F00122Q000F00263Q00202Q000F000F003C00202Q000F000F003D00102Q000E003C000F4Q000C000E00024Q000D5Q00122Q000E003E4Q00E0000F3Q00040012960110003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F003F001000122Q0010003B3Q0020CD00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F0041001000122Q0010003B3Q00207B01100010000E001226011100023Q001226011200404Q003F00100012000200102Q000F0042001000122Q0010003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F004300104Q000D000F6Q000B3Q00012Q00350008000B00020006A300093Q0001000B2Q009B3Q00074Q00D53Q00024Q00D53Q00014Q00D53Q00074Q009B3Q00084Q009B3Q00094Q009B3Q000A4Q009B3Q00014Q00D53Q00034Q00D53Q00054Q00D53Q00063Q00207B010A0003004400208A000A000A00450006A3000C0001000100042Q00D53Q00074Q009B3Q00084Q00D53Q00034Q009B3Q000A4Q00E1000A000C000100207B010A0003004600208A000A000A00450006A3000C0002000100042Q00D53Q00074Q009B3Q00084Q00D53Q00034Q009B3Q000A4Q006B000A000C000100202Q000A0003004700202Q000A000A00454Q000C00096Q000A000C00014Q000A00036Q000B00056Q000C00066Q000D00086Q000E00073Q0010460002004D00090010460002004C000E0010460002004B000D0010460002004A000C00104600020049000B00104600020048000A0010460002004E0004001275000A004F3Q0020B8000A000A00504Q000B00073Q00202Q000B000B00514Q000C00026Q000A000C00012Q009B000A00073Q00207B010A000A00512Q0077010A000A3Q0026E7000A2Q002Q01000B0004434Q002Q012Q00D5000A00094Q0032000A000100010006A3000A0003000100072Q00D53Q00024Q009B8Q009B3Q00014Q00D53Q00084Q009B3Q00034Q009B3Q000B4Q009B3Q000C3Q0006A3000B0004000100062Q00D53Q00024Q009B8Q00D53Q00084Q009B3Q00044Q009B3Q00014Q009B3Q000D3Q00104600020052000B0006A3000B0005000100092Q00D53Q000A4Q009B8Q009B3Q00044Q009B3Q00014Q009B3Q00054Q009B3Q00034Q009B3Q00084Q009B3Q000A4Q009B3Q00073Q00104600020053000B0006A3000B0006000100072Q00D53Q000A4Q009B8Q009B3Q00054Q009B3Q00014Q009B3Q000D4Q009B3Q00034Q009B3Q000E3Q00104600020054000B0006A3000B0007000100092Q00D53Q000A4Q009B8Q009B3Q00054Q009B3Q00014Q009B3Q00034Q009B3Q000B4Q009B3Q000C4Q009B3Q00084Q009B3Q000A3Q00104600020055000B0006A3000B0008000100062Q00D53Q000A4Q009B3Q00044Q009B3Q00014Q009B8Q009B3Q00054Q009B3Q000D3Q00104600020056000B0006A3000B0009000100072Q00D53Q000A4Q009B8Q009B3Q00044Q009B3Q00014Q009B3Q000D4Q009B3Q00084Q009B3Q000A3Q00104600020057000B0006A3000B000A0001000A2Q00D53Q00084Q009B8Q009B3Q00014Q00D53Q00074Q009B3Q00034Q009B3Q000B4Q009B3Q00044Q009B3Q000D4Q009B3Q00084Q009B3Q000A3Q00104600020058000B0006A3000B000B000100072Q00D53Q000A4Q009B8Q009B3Q00054Q009B3Q00014Q009B3Q000D4Q009B3Q00034Q009B3Q000F3Q00104600020059000B2Q00D6000200024Q0066012Q00013Q000C3Q00193Q0003083Q005F63752Q72656E7403063Q0048692Q64656E03063Q0069706169727303043Q005461627303053Q005F7772617003073Q0056697369626C6501002Q0103083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577027B14AE47E17AA43F028Q0003043Q005F62746E03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03053Q005F69636F6E030A3Q0054657874436F6C6F7233030A3Q005F69636F6E436F6C6F7203073Q005375625465787403053Q005F6E616D6503113Q0053656C656374656449636F6E436F6C6F7203093Q0049636F6E436F6C6F7203063Q00412Q63656E7403043Q005465787400914Q009B7Q00207B014Q00012Q009B000100013Q000684012Q0006000100010004433Q000600012Q0066012Q00014Q009B3Q00023Q00207B014Q000200068A012Q003200013Q0004433Q003200010012753Q00034Q009B00015Q00207B2Q01000100042Q00A6012Q000200020004433Q001400012Q009B000500013Q00068B00040014000100050004433Q0014000100207B0105000400050030A80005000600070006C03Q000F000100020004433Q000F00012Q009B8Q009B000100013Q0010463Q000100012Q009B3Q00033Q0030A83Q000600082Q009B3Q00033Q0012750001000A3Q0020BF00010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q0001000500020010953Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q0020BF00040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q0004000800020010460003000900042Q00E13Q000300012Q0066012Q00013Q0012753Q00034Q009B00015Q00207B2Q01000100042Q00A6012Q000200020004433Q005500012Q009B000500013Q00068B0004003C000100050004433Q003C000100207B0105000400050030A80005000600072Q009B000500043Q00201500060004000E4Q000700066Q00083Q000100302Q0008000F00104Q0005000800014Q000500043Q00202Q0006000400114Q000700066Q00083Q000100202Q00090004001300062Q0009004B000100010004433Q004B00012Q009B000900073Q00207B0109000900140010460008001200092Q00D70005000800014Q000500043Q00202Q0006000400154Q000700066Q00083Q00014Q000900073Q00202Q00090009001400102Q0008001200094Q0005000800010006C03Q0037000100020004433Q003700012Q009B8Q00BA000100013Q00104Q000100016Q00033Q00304Q000600086Q00033Q00122Q0001000A3Q00202Q00010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q00010005000200104Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00202Q00040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q00040008000200102Q0003000900046Q000300016Q00046Q000100086Q000200066Q00033Q000100302Q0003000F000D6Q000300016Q00046Q000100096Q000200066Q00033Q00014Q000400023Q00202Q00040004001600062Q00040086000100010004433Q008600012Q009B000400023Q00207B01040004001700069401040086000100010004433Q008600012Q009B000400073Q00207B0104000400180010460003001200042Q00E13Q000300012Q005C012Q00046Q0001000A6Q000200066Q00033Q00014Q000400073Q00202Q00040004001900102Q0003001200046Q000300016Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q33E33F000B4Q009B7Q00207B014Q0001000694012Q000A000100010004433Q000A00012Q009B3Q00014Q0056000100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q0066012Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F000B4Q009B7Q00207B014Q0001000694012Q000A000100010004433Q000A00012Q009B3Q00014Q0056000100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q0066012Q00017Q000F3Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003C40030B3Q004C61796F75744F72646572030F3Q00426F7264657253697A65506978656C03063Q00506172656E74026Q00144003063Q005374726F6B6501284Q009B00016Q009B00025Q00207B0102000200010020910102000200020010460001000100022Q009B000100013Q001226010200034Q00E000033Q00052Q009B000400023Q00207B0104000400050010B400030004000400122Q000400073Q00202Q00040004000800122Q000500023Q00122Q000600093Q001226010700093Q0006390008001300013Q0004433Q001300010012260108000A4Q00350004000800020010280103000600044Q00045Q00202Q00040004000100102Q0003000B000400302Q0003000C00094Q000400033Q00102Q0003000D00044Q0001000300024Q000200046Q000300013Q00122Q0004000E6Q0002000400014Q000200056Q000300016Q000400023Q00202Q00040004000F4Q000500066Q0002000500014Q000100028Q00017Q00213Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503163Q004261636B67726F756E645472616E73706172656E637903043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003640030B3Q004C61796F75744F7264657203063Q00506172656E7403043Q0049636F6E030D3Q00637562652D7665727465786573026Q002C4003093Q0049636F6E436F6C6F7203063Q00412Q63656E74030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E027Q004003093Q00546578744C6162656C03043Q005465787403043Q004E616D6503073Q0053656374696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q0036C002643Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100024Q009B00026Q009B00035Q00207B0103000300010020910103000300020010460002000100032Q009B000200013Q001226010300034Q00E000043Q00040030A8000400040002001275000500063Q0020BF00050005000700122Q000600023Q00122Q000700083Q00122Q000800083Q00122Q000900096Q0005000900020010670004000500054Q00055Q00202Q00050005000100102Q0004000A00054Q000500023Q00102Q0004000B00054Q0002000400024Q000300033Q00202Q00040001000C00062Q00040020000100010004433Q002000010012260104000D3Q0012260105000E4Q003C00065Q00207B01070001000F00069401070027000100010004433Q002700012Q009B000700043Q00207B0107000700102Q00350003000700020012E6000400123Q00202Q00040004000700122Q000500083Q00122Q000600136Q00040006000200102Q00030011000400122Q000400063Q00202Q00040004000700122Q000500083Q00122Q000600153Q00122Q000700133Q00122Q000800086Q00040008000200102Q00030014000400102Q0003000B00024Q000400013Q00122Q000500166Q00063Q000A00302Q00060004000200202Q00070001001800062Q0007003F000100010004433Q003F0001001226010700193Q0010460006001700072Q0051000700053Q00102Q0006001A00074Q000700043Q00202Q00070007001700102Q0006001B000700302Q0006001C001D00122Q0007001F3Q00202Q00070007001E00202Q00070007002000102Q0006001E0007001296010700123Q00202Q00070007000700122Q000800083Q00122Q000900136Q00070009000200102Q00060011000700122Q000700063Q0020BF00070007000700122Q000800083Q00122Q000900093Q00122Q000A00133Q00122Q000B00086Q0007000B0002001046000600140007001275000700063Q0020BF00070007000700122Q000800023Q00122Q000900213Q00122Q000A00023Q00122Q000B00086Q0007000B00020010A901060005000700102Q0006000B00024Q0004000600014Q000200028Q00017Q003B3Q0003073Q0044656661756C74026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00546578744C6162656C03043Q004E616D6503063Q00546F2Q676C6503083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674025Q0080474003053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q66026Q0020C0030A3Q0066726F6D4F2Q66736574026Q002Q40026Q003040030F3Q00426F7264657253697A65506978656C026Q002CC0027Q0040026Q0018402Q033Q005365742Q033Q0047657403053Q00537461746503043Q00466C616703043Q0053796E6303053Q007461626C6503063Q00696E7365727403083Q005F746F2Q676C657303093Q0041637469766174656403073Q00436F2Q6E65637403083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03083Q00496E7374616E636502F13Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100023Q00207B01020001000100069401020008000100010004433Q000800012Q003C00026Q009B00035Q001226010400024Q00310103000200022Q009B000400013Q001226010500034Q00E000063Q00040030A80006000400050030A8000600060007001275000700093Q0020BF00070007000A00122Q000800073Q00122Q0009000B3Q00122Q000A00073Q00122Q000B000B6Q0007000B000200104500060008000700102Q0006000C00034Q00040006000200122Q0005000D3Q00202Q00060001000E00062Q0006003800013Q0004433Q003800012Q009B000600023Q00202101070001000E00122Q0008000F6Q00095Q00202Q000A0001001000062Q000A0027000100010004433Q002700012Q009B000A00033Q00207B010A000A00112Q00350006000A0002001296010700133Q00202Q00070007000A00122Q0008000B3Q00122Q000900146Q00070009000200102Q00060012000700122Q000700093Q0020BF00070007000A00122Q0008000B3Q00122Q0009000D3Q00122Q000A00143Q00122Q000B000B6Q0007000B00020010460006001500070010460006000C0004001226010500164Q009B000600013Q001247000700176Q00083Q000A00302Q00080006000700202Q00090001001800062Q00090040000100010004433Q00400001001226010900193Q0010460008000400092Q0051000900043Q00102Q0008001A00094Q000900033Q00202Q00090009000400102Q0008001B000900302Q0008001C001D00122Q0009001F3Q00202Q00090009001E00202Q00090009002000102Q0008001E0009001296010900133Q00202Q00090009000A00122Q000A000B3Q00122Q000B00146Q0009000B000200102Q00080012000900122Q000900093Q00207B01090009000A001226010A000B4Q00D5000B00053Q001226010C00143Q001226010D000B4Q00D80009000D000200102Q00080015000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00202Q000B000500214Q000B000B3Q00122Q000C00073Q00122Q000D000B6Q0009000D00020010460008000800090010A40108000C00044Q0006000800014Q000600013Q00122Q000700226Q00083Q000600062Q0002006D00013Q0004433Q006D00012Q009B000900033Q00207B0109000900240006940109006F000100010004433Q006F00012Q009B000900033Q00207B010900090025001046000800230009001230010900133Q00202Q00090009000A00122Q000A00073Q00122Q000B00146Q0009000B000200102Q00080012000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00122Q000B00263Q00122Q000C00143Q00122Q000D000B6Q0009000D000200102Q00080015000900122Q000900093Q00202Q00090009002700122Q000A00283Q00122Q000B00296Q0009000B000200102Q00080008000900302Q0008002A000B00102Q0008000C00044Q0006000800024Q000700056Q000800063Q00122Q0009000D6Q0007000900014Q000700013Q00122Q000800226Q00093Q00064Q000A00033Q00202Q000A000A000400102Q00090023000A00122Q000A00133Q00202Q000A000A000A00122Q000B000B3Q00122Q000C00146Q000A000C000200102Q00090012000A00062Q000200A200013Q0004433Q00A20001001275000A00093Q002002010A000A000A00122Q000B00073Q00122Q000C002B3Q00122Q000D00143Q00122Q000E000B6Q000A000E000200062Q000A00A9000100010004433Q00A90001001275000A00093Q0020BF000A000A000A00122Q000B000B3Q00122Q000C002C3Q00122Q000D00143Q00122Q000E000B6Q000A000E000200104600090015000A00127A000A00093Q00202Q000A000A002700122Q000B001D3Q00122Q000C001D6Q000A000C000200102Q00090008000A00302Q0009002A000B00102Q0009000C00064Q0007000900024Q000800056Q000900073Q00122Q000A002D6Q0008000A00014Q00085Q0006A300093Q000100072Q00D53Q00024Q009B3Q00064Q00D53Q00064Q009B3Q00074Q009B3Q00034Q00D53Q00074Q00D53Q00013Q0010460008002E00090006A300090001000100012Q00D53Q00023Q0010460008002F000900207B010900010030000694010900CC000100010004433Q00CC000100207B01090001003100068A010900CC00013Q0004433Q00CC00010006A300090002000100012Q00D53Q00013Q0006A3000A0003000100082Q00D53Q00094Q00D53Q00024Q009B3Q00064Q00D53Q00064Q009B3Q00074Q009B3Q00034Q00D53Q00074Q00D53Q00013Q00104600080032000A00068A010900DE00013Q0004433Q00DE0001001275000A00333Q0020B8000A000A00344Q000B00083Q00202Q000B000B00354Q000C00086Q000A000C000100207B010A0004003600208A000A000A00370006A3000C0004000100022Q00D53Q00084Q00D53Q00024Q00E1000A000C000100068A010200EE00013Q0004433Q00EE000100207B010A0001003800068A010A00EE00013Q0004433Q00EE0001001275000A00393Q00207B010A000A003A00207B010B000100382Q003C000C00014Q00E1000A000C00010010460008003B00032Q00D6000800024Q0066012Q00013Q00053Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02344Q00A22Q016Q009B000200014Q009B000300024Q009B000400034Q00E000053Q00012Q009B00065Q00068A0106000C00013Q0004433Q000C00012Q009B000600043Q00207B0106000600020006940106000E000100010004433Q000E00012Q009B000600043Q00207B0106000600030010460005000100062Q007C0002000500014Q000200016Q000300056Q000400036Q00053Q00014Q00065Q00062Q0006002000013Q0004433Q00200001001275000600053Q00200201060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A000200062Q00060027000100010004433Q00270001001275000600053Q0020BF00060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A00020010460005000400062Q00E10002000500012Q009B000200063Q00207B01020002000C00068A0102003300013Q0004433Q003300010012750002000D3Q0020F300020002000E4Q000300063Q00202Q00030003000C4Q00048Q0002000400012Q0066012Q00019Q002Q0001034Q009B00016Q00D6000100024Q0066012Q00017Q00033Q0003073Q0067657467656E7603043Q00466C61672Q01000B3Q001268012Q00018Q000100024Q00015Q00202Q0001000100028Q000100264Q0008000100030004433Q000800012Q009F8Q003C3Q00014Q00D63Q00024Q0066012Q00017Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E01464Q009B00015Q0006942Q010004000100010004433Q000400012Q0066012Q00014Q009B00016Q00402Q010001000200068A2Q01000B00013Q0004433Q000B00012Q003C000100013Q0006942Q01000C000100010004433Q000C00012Q003C00016Q009B000200013Q0006842Q010010000100020004433Q001000012Q0066012Q00014Q00A22Q0100014Q009B000200024Q009B000300034Q009B000400044Q00E000053Q00012Q009B000600013Q00068A0106001C00013Q0004433Q001C00012Q009B000600053Q00207B0106000600020006940106001E000100010004433Q001E00012Q009B000600053Q00207B0106000600030010460005000100062Q007C0002000500014Q000200026Q000300066Q000400046Q00053Q00014Q000600013Q00062Q0006003000013Q0004433Q00300001001275000600053Q00200201060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A000200062Q00060037000100010004433Q00370001001275000600053Q0020BF00060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A00020010460005000400062Q00E10002000500010006942Q010045000100010004433Q004500012Q009B000200073Q00207B01020002000C00068A0102004500013Q0004433Q004500010012750002000D3Q00207400020002000E4Q000300073Q00202Q00030003000C4Q00048Q0002000400012Q0066012Q00017Q00013Q002Q033Q0053657400064Q009B7Q00208A5Q00012Q009B000200014Q007B000200024Q00E13Q000200012Q0066012Q00017Q003A3Q002Q033Q004D696E028Q002Q033Q004D6178026Q00594003093Q00496E6372656D656E74026Q00F03F03043Q006D61746803053Q00636C616D7003073Q0044656661756C74026Q00454003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E637903043Q005465787403043Q004E616D6503063Q00536C6964657203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q002040026Q00104003043Q0053697A65026Q004EC0026Q002C4003063Q00506172656E7403083Q00746F737472696E6703063Q00412Q63656E7403053Q005269676874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q0020C0026Q004940030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66026Q00E03F026Q0026C0026Q0030C0026Q002440030F3Q00426F7264657253697A65506978656C026Q00084003053Q004672616D65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578027Q0040026Q0018402Q033Q005365742Q033Q0047657403083Q00496E7374616E636502E53Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100023Q00207B01020001000100069401020008000100010004433Q00080001001226010200023Q00207B0103000100030006940103000C000100010004433Q000C0001001226010300043Q00207B01040001000500069401040010000100010004433Q00100001001226010400063Q001275000500073Q00207B01050005000800207B01060001000900069401060016000100010004433Q001600012Q00D5000600024Q00D5000700024Q0049000800036Q0005000800024Q00065Q00122Q0007000A6Q0006000200024Q000700013Q00122Q0008000B6Q00093Q000900302Q0009000C000600202Q000A0001000E00062Q000A0024000100010004433Q00240001001226010A000F3Q0010460009000D000A2Q0051000A00023Q00102Q00090010000A4Q000A00033Q00202Q000A000A000D00102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A001600102Q00090014000A001275000A00183Q0020BF000A000A001900122Q000B00023Q00122Q000C001A3Q00122Q000D00023Q00122Q000E001B6Q000A000E000200104600090017000A001275000A00183Q0020BF000A000A001900122Q000B00063Q00122Q000C001D3Q00122Q000D00023Q00122Q000E001E6Q000A000E000200100E0009001C000A00102Q0009001F00064Q0007000900014Q000700013Q00122Q0008000B6Q00093Q000A00302Q0009000C000600122Q000A00206Q000B00056Q000A000200020010460009000D000A2Q0051000A00043Q00102Q00090010000A4Q000A00033Q00202Q000A000A002100102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A002200102Q00090014000A001296010A00243Q00202Q000A000A001900122Q000B00063Q00122Q000C00026Q000A000C000200102Q00090023000A00122Q000A00183Q0020BF000A000A001900122Q000B00063Q00122Q000C00253Q00122Q000D00023Q00122Q000E001B6Q000A000E000200104600090017000A001275000A00183Q0020BF000A000A001900122Q000B00023Q00122Q000C00263Q00122Q000D00023Q00122Q000E001E6Q000A000E00020010460009001C000A0010460009001F00062Q00350007000900022Q009B000800013Q001226010900274Q00E0000A3Q00080030A8000A000D00280030A8000A0029002A2Q009B000B00033Q00207B010B000B002C0010B4000A002B000B00122Q000B00243Q00202Q000B000B001900122Q000C00023Q00122Q000D002D4Q0035000B000D0002001046000A0023000B001275000B00183Q0020BF000B000B001900122Q000C00023Q00122Q000D001A3Q00122Q000E00063Q00122Q000F002E6Q000B000F0002001046000A0017000B001275000B00183Q0020BF000B000B001900122Q000C00063Q00122Q000D002F3Q00122Q000E00023Q00122Q000F00306Q000B000F000200107D010A001C000B00302Q000A0031000200102Q000A001F00064Q0008000A00024Q000900056Q000A00083Q00122Q000B00326Q0009000B00014Q000900013Q00122Q000A00334Q00E0000B3Q00042Q009B000C00033Q00207B010C000C0021001046000B002B000C001275000C00183Q00207B010C000C00192Q00AA010D000500022Q00AA010E000300022Q005D000D000D000E001226010E00023Q001226010F00063Q001226011000024Q0004000C0010000200102Q000B001C000C00302Q000B0031000200102Q000B001F00084Q0009000B00024Q000A00056Q000B00093Q00122Q000C00326Q000A000C00014Q000A00013Q001226010B00334Q00C3000C3Q00074Q000D00033Q00202Q000D000D000D00102Q000C002B000D00122Q000D00243Q00202Q000D000D001900122Q000E002D3Q00122Q000F002D6Q000D000F000200102Q000C0023000D001275000D00183Q00207B010D000D00192Q00AA010E000500022Q00AA010F000300022Q005D000E000E000F001226010F00023Q0012260110002D3Q001226011100024Q0035000D001100020010B4000C0017000D00122Q000D00183Q00202Q000D000D003400122Q000E00133Q00122Q000F00134Q0035000D000F0002001046000C001C000D0030A8000C003100020030A8000C00350036001046000C001F00082Q0035000A000C00022Q006C000B00056Q000C000A3Q00122Q000D00376Q000B000D00014Q000B5Q0006A3000C3Q000100082Q00D53Q00024Q00D53Q00034Q00D53Q00054Q00D53Q00044Q00D53Q00094Q00D53Q000A4Q00D53Q00074Q00D53Q00014Q009B000D00064Q00D5000E00083Q0006A3000F0001000100012Q00D53Q000C4Q00E1000D000F00010006A3000D0002000100032Q00D53Q000C4Q00D53Q00024Q00D53Q00033Q001046000B0038000D0006A3000D0003000100012Q00D53Q00053Q001046000B0039000D001046000B003A00062Q00D6000B00024Q0066012Q00013Q00043Q000F3Q0003043Q006D61746803053Q00636C616D7003053Q00666C2Q6F72026Q00E03F028Q0003043Q0053697A6503053Q005544696D322Q033Q006E6577026Q00F03F03083Q00506F736974696F6E03043Q005465787403083Q00746F737472696E6703083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02474Q009B00026Q009B000300014Q009B00046Q00AA0103000300042Q002D010300034Q00B5000200020003001275000300013Q00207B010300030002001275000400013Q00207B0104000400032Q009B000500034Q005D0005000200050020910105000500042Q00310104000200022Q009B000500034Q002D0104000400052Q009B00056Q009B000600014Q00350003000600022Q00A2010300024Q009B000300014Q009B00046Q00AA0103000300040026E70003001C000100050004433Q001C0001001226010300053Q00069401030023000100010004433Q002300012Q009B000300024Q005701048Q0003000300044Q000400016Q00058Q0004000400054Q0003000300042Q009B000400043Q001286010500073Q00202Q0005000500084Q000600033Q00122Q000700053Q00122Q000800093Q00122Q000900056Q00050009000200102Q0004000600054Q000400053Q00122Q000500073Q00207B0105000500082Q00C6000600033Q00122Q000700053Q00122Q000800043Q00122Q000900056Q00050009000200102Q0004000A00054Q000400063Q00122Q0005000C6Q000600026Q0005000200020010460004000B000500068A2Q01004600013Q0004433Q004600012Q009B000400073Q00207B01040004000D00068A0104004600013Q0004433Q004600010012750004000E3Q0020F300040004000F4Q000500073Q00202Q00050005000D4Q000600026Q0004000600012Q0066012Q00019Q002Q0001054Q001A2Q018Q00028Q000300016Q0001000300016Q00017Q00023Q0003043Q006D61746803053Q00636C616D7002104Q005801025Q00122Q000300013Q00202Q0003000300024Q000400016Q000500016Q000600026Q0003000600024Q000400016Q0003000300044Q000400026Q000500016Q0004000400054Q0003000300044Q000400016Q0002000400016Q00019Q002Q0001034Q009B00016Q00D6000100024Q0066012Q00017Q00453Q00026Q003C4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503073Q0054657874626F7803083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65029A5Q99D93F026Q0020C003063Q00506172656E7403053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E64617279026Q0018C0026Q003440030D3Q004175746F6D6174696353697A6503013Q005803103Q00436C69707344657363656E64616E74732Q01030F3Q00426F7264657253697A65506978656C03093Q00554950612Q64696E67030B3Q0050612Q64696E674C65667403043Q005544696D026Q001440030C3Q0050612Q64696E675269676874026Q00104003063Q005374726F6B6503073Q0054657874426F7803073Q0044656661756C74034Q00030F3Q00506C616365686F6C64657254657874030B3Q00506C616365686F6C6465722Q033Q003Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q0053756254657874026Q00264003103Q00436C656172546578744F6E466F637573010003103Q00554953697A65436F6E73747261696E7403073Q004D696E53697A6503083Q004D696E5769647468025Q0080464003073Q004D617853697A6503083Q004D61785769647468025Q0080614003043Q006D61746803043Q006875676503073Q00466F637573656403073Q00436F2Q6E65637403093Q00466F6375734C6F73742Q033Q005365742Q033Q0047657403083Q00496E7374616E636502D23Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100024Q009B00025Q0012A8010300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000A00302Q00050003000400202Q00060001000600062Q0006000F000100010004433Q000F0001001226010600073Q0010460005000500062Q0051000600023Q00102Q0005000800064Q000600033Q00202Q00060006000500102Q00050009000600302Q0005000A000B00122Q0006000D3Q00202Q00060006000C00202Q00060006000E00102Q0005000C0006001296010600103Q00202Q00060006001100122Q000700123Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q0020BF00060006001100122Q000700123Q00122Q000800163Q00122Q000900133Q00122Q000A00126Q0006000A0002001046000500140006001275000600153Q0020BF00060006001100122Q000700183Q00122Q000800193Q00122Q000900043Q00122Q000A00126Q0006000A00020010460005001700060010460005001A00022Q00E10003000500012Q009B000300013Q0012260104001B4Q00C300053Q00084Q000600033Q00202Q00060006001D00102Q0005001C000600122Q000600103Q00202Q00060006001100122Q000700043Q00122Q000800136Q00060008000200102Q0005000F0006001275000600153Q0020BF00060006001100122Q000700043Q00122Q0008001E3Q00122Q000900133Q00122Q000A00126Q0006000A0002001046000500140006001275000600153Q0020BF00060006001100122Q000700123Q00122Q000800123Q00122Q000900123Q00122Q000A001F6Q0006000A000200105700050017000600122Q0006000D3Q00202Q00060006002000202Q00060006002100102Q00050020000600302Q00050022002300302Q00050024001200102Q0005001A00024Q00068Q000700013Q001226010800254Q00E000093Q0002001275000A00273Q00207B010A000A0011001226010B00123Q001226010C00284Q003F000A000C000200102Q00090026000A00122Q000A00273Q00202Q000A000A001100122Q000B00123Q00122Q000C00286Q000A000C000200102Q00090029000A4Q000700096Q00063Q00012Q00350003000600022Q009B000400044Q00D5000500033Q0012260106002A4Q00E10004000600012Q009B000400054Q00D5000500034Q009B000600033Q00207B01060006002B2Q009B000700064Q00350004000700022Q009B000500013Q0012470006002C6Q00073Q000C00302Q00070003000400202Q00080001002D00062Q0008007A000100010004433Q007A00010012260108002E3Q00104600070005000800207B0108000100300006940108007F000100010004433Q007F0001001226010800313Q0010460007002F00082Q009B000800033Q00207B0108000800330010460007003200082Q009B000800023Q0010460007000800082Q009B000800033Q00207B0108000800050010460007000900080030A80007000A00340030A80007003500360012750008000D3Q00201300080008000C00202Q00080008000E00102Q0007000C000800122Q0008000D3Q00202Q00080008002000202Q00080008002100102Q00070020000800122Q000800153Q0020BF00080008001100122Q000900123Q00122Q000A00123Q00122Q000B00043Q00122Q000C00126Q0008000C00020010460007001700080010460007001A00032Q00E000086Q009B000900013Q001226010A00374Q00E0000B3Q0002001275000C00103Q00207B010C000C001100207B010D00010039000694010D00A5000100010004433Q00A50001001226010D003A3Q001226010E00124Q002E010C000E000200102Q000B0038000C00122Q000C00103Q00202Q000C000C001100202Q000D0001003C00062Q000D00AE000100010004433Q00AE0001001226010D003D3Q001275000E003E3Q00207B010E000E003F2Q0035000C000E0002001046000B003B000C2Q00130109000B4Q007F00083Q00012Q003500050008000200207B01060005004000208A0006000600410006A300083Q000100042Q009B3Q00074Q00D53Q00044Q009B3Q00084Q009B3Q00034Q00E100060008000100207B01060005004200208A0006000600410006A300080001000100072Q009B3Q00074Q00D53Q00044Q009B3Q00084Q009B3Q00034Q009B3Q00064Q00D53Q00014Q00D53Q00054Q00E10006000800012Q00E000063Q00030006A300070002000100012Q00D53Q00053Q0010460006004300070006A300070003000100012Q00D53Q00053Q0010460006004400070010460006004500022Q00D6000600024Q0066012Q00013Q00043Q00043Q0003053Q00436F6C6F7203063Q00412Q63656E74030C3Q005472616E73706172656E6379029A5Q99C93F000A4Q003F019Q000100016Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030001000400302Q0003000300046Q000300016Q00017Q00073Q0003053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E637903083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03043Q005465787400164Q00549Q00000100016Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q0003000100044Q000400043Q00102Q0003000300046Q000300016Q00053Q00206Q000400064Q001500013Q0004433Q001500010012753Q00053Q00207B014Q00062Q009B000100053Q00207B2Q01000100042Q009B000200063Q00207B0102000200072Q00E13Q000200012Q0066012Q00017Q00013Q0003043Q005465787402034Q009B00025Q0010460002000100012Q0066012Q00017Q00013Q0003043Q005465787400044Q009B7Q00207B014Q00012Q00D63Q00024Q0066012Q00017Q00253Q00026Q003C40026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D3203063Q00506172656E74026Q003A4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503053Q004C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403043Q0053697A6503053Q0056616C7565034Q0003063Q00412Q63656E7403053Q005269676874026Q0020C02Q033Q005365742Q033Q0047657403083Q00496E7374616E636502853Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100024Q009B00025Q0012C9000300016Q00020002000200122Q000300023Q00202Q00040001000300062Q0004002500013Q0004433Q002500012Q009B000400013Q00202101050001000300122Q000600046Q00075Q00202Q00080001000500062Q00080014000100010004433Q001400012Q009B000800023Q00207B0108000800062Q0035000400080002001296010500083Q00202Q00050005000900122Q0006000A3Q00122Q0007000B6Q00050007000200102Q00040007000500122Q0005000D3Q0020BF00050005000900122Q0006000A3Q00122Q000700023Q00122Q0008000B3Q00122Q0009000A6Q0005000900020010460004000C00050010460004000E00020012260103000F4Q009B000400033Q001247000500106Q00063Q000A00302Q00060011001200202Q00070001001400062Q0007002D000100010004433Q002D0001001226010700153Q0010460006001300072Q0051000700043Q00102Q0006001600074Q000700023Q00202Q00070007001300102Q00060017000700302Q00060018001900122Q0007001B3Q00202Q00070007001A00202Q00070007001C00102Q0006001A0007001296010700083Q00202Q00070007000900122Q0008000A3Q00122Q0009000B6Q00070009000200102Q00060007000700122Q0007000D3Q00207B0107000700090012260108000A4Q00D5000900033Q001226010A000B3Q001226010B000A4Q00350007000B00020010460006000C00070012750007000D3Q00207B0107000700090012260108000B4Q00E3000900033Q001226010A00123Q001226010B000A4Q00350007000B00020010460006001D00070010460006000E00022Q00350004000600022Q009B000500033Q001247000600106Q00073Q000A00302Q00070011001200202Q00080001001E00062Q00080058000100010004433Q005800010012260108001F3Q0010460007001300082Q0051000800053Q00102Q0007001600084Q000800023Q00202Q00080008002000102Q00070017000800302Q00070018001900122Q0008001B3Q00202Q00080008001A00202Q00080008002100102Q0007001A0008001296010800083Q00202Q00080008000900122Q000900123Q00122Q000A000B6Q0008000A000200102Q00070007000800122Q0008000D3Q0020BF00080008000900122Q000900123Q00122Q000A00223Q00122Q000B000B3Q00122Q000C000A6Q0008000C00020010460007000C00080012750008000D3Q0020BF00080008000900122Q0009000B3Q00122Q000A00223Q00122Q000B00123Q00122Q000C000A6Q0008000C00020010460007001D00080010460007000E00022Q00350005000700022Q00E000063Q00030006A300073Q000100012Q00D53Q00053Q0010460006002300070006A300070001000100012Q00D53Q00053Q0010460006002400070010460006002500022Q00D6000600024Q0066012Q00013Q00023Q00023Q0003043Q005465787403083Q00746F737472696E6702064Q002A00025Q00122Q000300026Q000400016Q00030002000200102Q0002000100036Q00017Q00013Q0003043Q005465787400044Q009B7Q00207B014Q00012Q00D63Q00024Q0066012Q00017Q002C3Q00026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00526967687449636F6E030E3Q00526967687449636F6E436F6C6F72026Q0020C003093Q00546578744C6162656C03043Q004E616D6503063Q0042752Q746F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E6365030C3Q00536574526967687449636F6E02A53Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100024Q009B00025Q001223010300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000500302Q00050003000400302Q00050005000600302Q00050007000800122Q0006000A3Q00202Q00060006000B00122Q000700083Q00122Q0008000C3Q00122Q000900083Q00122Q000A000C6Q0006000A000200102Q00050009000600102Q0005000D00024Q00030005000200122Q0004000E3Q00202Q00050001000F00062Q0005003500013Q0004433Q003500012Q009B000500023Q00202101060001000F00122Q000700106Q00085Q00202Q00090001001100062Q00090024000100010004433Q002400012Q009B000900033Q00207B0109000900122Q0035000500090002001296010600143Q00202Q00060006000B00122Q0007000C3Q00122Q000800156Q00060008000200102Q00050013000600122Q0006000A3Q0020BF00060006000B00122Q0007000C3Q00122Q0008000E3Q00122Q000900153Q00122Q000A000C6Q0006000A00020010460005001600060010460005000D0003001226010400174Q00CA000500053Q0012260106000C3Q00207B01070001001800068A0107005500013Q0004433Q005500012Q009B000700023Q00202101080001001800122Q000900106Q000A5Q00202Q000B0001001900062Q000B0043000100010004433Q004300012Q009B000B00033Q00207B010B000B00122Q00350007000B00022Q005F000500073Q00122Q000700143Q00202Q00070007000B00122Q000800083Q00122Q000900156Q00070009000200102Q00050013000700122Q0007000A3Q00202Q00070007000B00122Q000800083Q00122Q0009001A3Q00122Q000A00153Q00122Q000B000C6Q0007000B000200102Q00050016000700102Q0005000D000300122Q000600174Q009B000700013Q0012470008001B6Q00093Q000B00302Q00090007000800202Q000A0001001C00062Q000A005D000100010004433Q005D0001001226010A001D3Q00104600090003000A2Q00DB000A00043Q00102Q0009001E000A4Q000A00033Q00202Q000A000A000300102Q0009001F000A00302Q00090020002100122Q000A00233Q00202Q000A000A002200202Q000A000A002400102Q00090022000A00122Q000A00233Q00202Q000A000A002500202Q000A000A002600102Q00090025000A00122Q000A00143Q00202Q000A000A000B00122Q000B000C3Q00122Q000C00156Q000A000C000200102Q00090013000A00122Q000A000A3Q00202Q000A000A000B00122Q000B000C6Q000C00043Q00122Q000D00153Q00122Q000E000C6Q000A000E000200102Q00090016000A00122Q000A000A3Q00202Q000A000A000B00122Q000B00086Q000C000400064Q000C000C3Q00122Q000D00083Q00122Q000E000C6Q000A000E000200102Q00090009000A00102Q0009000D00034Q00070009000200202Q00080003002700202Q0008000800280006A3000A3Q000100042Q009B3Q00054Q00D53Q00024Q009B3Q00064Q009B3Q00034Q00E10008000A000100207B01080003002900208A0008000800280006A3000A0001000100042Q009B3Q00054Q00D53Q00024Q009B3Q00064Q009B3Q00034Q00E10008000A000100207B01080003002A00208A0008000800280006A3000A0002000100052Q009B3Q00054Q00D53Q00024Q009B3Q00064Q009B3Q00034Q00D53Q00014Q00E10008000A00012Q00E000083Q00020010460008002B00020006A300090003000100012Q00D53Q00053Q0010460008002C00092Q00D6000800024Q0066012Q00013Q00043Q00023Q0003103Q004261636B67726F756E64436F6C6F7233030C3Q00456C656D656E74486F76657200094Q005C019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q005C019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00073Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E7403043Q007461736B03053Q0064656C6179029A5Q99B93F03083Q0043612Q6C6261636B03053Q00737061776E001B4Q002F9Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q0003000100124Q00033Q00206Q000400122Q000100053Q0006A300023Q000100042Q009B8Q009B3Q00014Q009B3Q00024Q009B3Q00034Q00E13Q000200012Q009B3Q00043Q00207B014Q000600068A012Q001A00013Q0004433Q001A00010012753Q00033Q00207B014Q00072Q009B000100043Q00207B2Q01000100062Q00583Q000200012Q0066012Q00013Q00013Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q005C019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00013Q0003043Q005465787402064Q009B00025Q00068A0102000500013Q0004433Q000500012Q009B00025Q0010460002000100012Q0066012Q00017Q00353Q0003043Q0054657874025Q00805240026Q003A4003153Q0046696E6446697273744368696C644F66436C612Q7303093Q00554950612Q64696E67030D3Q0050612Q64696E67426F2Q746F6D03043Q005544696D2Q033Q006E6577028Q00026Q004640030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E6461727903163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00F03F03083Q00506F736974696F6E03053Q005544696D32026Q0028C0026Q0024C003043Q0053697A65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578026Q00244003063Q00506172656E74026Q002A4003063Q005374726F6B65026Q33E33F03043Q0049636F6E03063Q00636972636C6503073Q0053756254657874026Q002640026Q00E03F026Q0020C003093Q00546578744C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q003EC0030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E636502B43Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100023Q00207B01020001000100068A0102000A00013Q0004433Q000A0001001226010300023Q0006940103000B000100010004433Q000B0001001226010300034Q009B00045Q00208A000400040004001226010600054Q003500040006000200068A0104001700013Q0004433Q00170001001275000500073Q00204C00050005000800122Q000600093Q00122Q0007000A6Q00050007000200102Q0004000600052Q009B000500013Q0012260106000B4Q00E000073Q00090030A800070001000C0030A80007000D000E2Q009B000800023Q00207B0108000800100010460007000F00080030A8000700110012001296010800143Q00202Q00080008000800122Q000900153Q00122Q000A00156Q0008000A000200102Q00070013000800122Q000800173Q0020BF00080008000800122Q000900153Q00122Q000A00183Q00122Q000B00153Q00122Q000C00196Q0008000C0002001046000700160008001275000800173Q00207B01080008001B2Q00D5000900033Q001226010A00034Q00350008000A00020010460007001A00080030A80007001C001D2Q009B000800033Q0010460007001E00082Q00350005000700022Q009B000600044Q00D5000700053Q0012260108001F4Q00E10006000800012Q009B000600054Q00D5000700054Q009B000800023Q00207B010800080020001226010900214Q00350006000900022Q009B000700063Q00207B01080001002200069401080047000100010004433Q00470001001226010800233Q0012260109001F4Q003C000A6Q009B000B00023Q00207B010B000B00242Q00350007000B00020030A80007001C00250010460007001E00052Q00CA000800083Q00068A0102008900013Q0004433Q00890001001275000900143Q00209F01090009000800122Q000A00153Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00153Q00122Q000B00273Q00122Q000C00263Q00122Q000D00096Q0009000D000200102Q0007001600094Q000900013Q00122Q000A00286Q000B3Q000B00302Q000B0011001500102Q000B000100024Q000C00073Q00102Q000B0029000C4Q000C00023Q00202Q000C000C002400102Q000B002A000C00302Q000B002B002C00122Q000C002E3Q00202Q000C000C002D00202Q000C000C002F00102Q000B002D000C00122Q000C00143Q00202Q000C000C000800122Q000D00093Q00122Q000E00266Q000C000E000200102Q000B0013000C00122Q000C00173Q00202Q000C000C000800122Q000D00093Q00122Q000E00253Q00122Q000F00263Q00122Q001000096Q000C0010000200102Q000B0016000C00122Q000C00173Q00202Q000C000C000800122Q000D00153Q00122Q000E00303Q00122Q000F00153Q00122Q001000096Q000C0010000200102Q000B001A000C00302Q000B001C002500102Q000B001E00054Q0009000B00024Q000800093Q00044Q00970001001275000900143Q0020CD00090009000800122Q000A00263Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q0020BF00090009000800122Q000A00263Q00122Q000B00093Q00122Q000C00263Q00122Q000D00096Q0009000D000200104600070016000900207B01090005003100208A0009000900320006A3000B3Q000100062Q009B3Q00084Q00D53Q00074Q009B3Q00094Q009B3Q00024Q00D53Q00064Q00D53Q00084Q00E10009000B000100207B01090005003300208A0009000900320006A3000B0001000100062Q009B3Q00084Q00D53Q00074Q009B3Q00094Q009B3Q00024Q00D53Q00064Q00D53Q00084Q00E10009000B000100207B01090005003400208A0009000900320006A3000B0002000100012Q00D53Q00014Q00E10009000B00012Q00E000093Q00010010460009003500052Q00D6000900024Q0066012Q00013Q00033Q00063Q00030A3Q0054657874436F6C6F723303063Q00412Q63656E7403053Q00436F6C6F72030C3Q005472616E73706172656E6379029A5Q99C93F03043Q0054657874001D4Q009B8Q0010000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q002C9Q00000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030003000400302Q0003000400056Q000300016Q00053Q00068A012Q001C00013Q0004433Q001C00012Q009B8Q0010000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000600102Q0003000100046Q000300012Q0066012Q00017Q00063Q00030A3Q0054657874436F6C6F723303073Q005375625465787403053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q33E33F001D4Q009B8Q0010000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q002C9Q00000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000400102Q00030003000400302Q0003000500066Q000300016Q00053Q00068A012Q001C00013Q0004433Q001C00012Q009B8Q0010000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q0066012Q00017Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q009B7Q00207B014Q000100068A012Q000900013Q0004433Q000900010012753Q00023Q00207B014Q00032Q009B00015Q00207B2Q01000100012Q00583Q000200012Q0066012Q00017Q002D3Q00026Q003E4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403013Q002D03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65025Q004060C003063Q00506172656E74034Q0003063Q00412Q63656E7403053Q005269676874026Q004CC0026Q005040030A3Q005465787442752Q746F6E2Q033Q0042757903103Q004261636B67726F756E64436F6C6F7233030F3Q004175746F42752Q746F6E436F6C6F720100026Q0018C0030A3Q0066726F6D4F2Q66736574026Q004640026Q003440026Q00104003093Q0041637469766174656403073Q00436F2Q6E65637403083Q00496E7374616E63652Q033Q0053657402913Q0006942Q010004000100010004433Q000400012Q00E000026Q00D5000100024Q009B00025Q00127E000300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000B00302Q00050003000400302Q0005000500064Q000600023Q00102Q0005000700064Q000600033Q00202Q00060006000500102Q00050008000600302Q00050009000A00122Q0006000C3Q00202Q00060006000B00202Q00060006000D00102Q0005000B000600122Q0006000C3Q00202Q00060006000E00202Q00060006000F00102Q0005000E000600122Q000600113Q00202Q00060006001200122Q000700133Q00122Q000800146Q00060008000200102Q00050010000600122Q000600163Q00202Q00060006001200122Q000700133Q00122Q000800173Q00122Q000900143Q00122Q000A00136Q0006000A000200102Q00050015000600122Q000600163Q00202Q00060006001200122Q000700043Q00122Q000800193Q00122Q000900043Q00122Q000A00136Q0006000A000200102Q00050018000600102Q0005001A00024Q0003000500024Q000400013Q00122Q000500026Q00063Q000A00302Q00060003000400302Q00060005001B4Q000700043Q00102Q0006000700074Q000700033Q00202Q00070007001C00102Q00060008000700302Q00060009000A00122Q0007000C3Q00202Q00070007000B00202Q00070007001D00102Q0006000B000700122Q000700113Q00202Q00070007001200122Q000800043Q00122Q000900146Q00070009000200102Q00060010000700122Q000700163Q00202Q00070007001200122Q000800043Q00122Q0009001E3Q00122Q000A00143Q00122Q000B00136Q0007000B000200102Q00060015000700122Q000700163Q00202Q00070007001200122Q000800133Q00122Q0009001F3Q00122Q000A00043Q00122Q000B00134Q00350007000B000200109501060018000700102Q0006001A00024Q0004000600024Q000500013Q00122Q000600206Q00073Q000A00302Q0007000500214Q000800043Q00102Q0007000700084Q000800033Q00202Q00080008000500102Q00070008000800302Q00070009000A4Q000800033Q00202Q00080008001C00102Q00070022000800302Q00070023002400122Q000800113Q00202Q00080008001200122Q000900043Q00122Q000A00146Q0008000A000200102Q00070010000800122Q000800163Q00202Q00080008001200122Q000900043Q00122Q000A00253Q00122Q000B00143Q00122Q000C00136Q0008000C000200102Q00070015000800122Q000800163Q00202Q00080008002600122Q000900273Q00122Q000A00286Q0008000A000200102Q00070018000800102Q0007001A00024Q0005000700024Q000600056Q000700053Q00122Q000800296Q00060008000100202Q00060005002A00202Q00060006002B0006A300083Q000100012Q00D53Q00014Q00E10006000800012Q00E000063Q00020010460006002C00020006A300070001000100052Q00D53Q00034Q00D53Q00044Q009B3Q00064Q00D53Q00054Q009B3Q00033Q0010460006002D00072Q00D6000600024Q0066012Q00013Q00023Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q009B7Q00207B014Q000100068A012Q000900013Q0004433Q000900010012753Q00023Q00207B014Q00032Q009B00015Q00207B2Q01000100012Q00583Q000200012Q0066012Q00017Q00113Q0003043Q0054657874034Q002Q033Q00737562027Q00C003023Q002D5303103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66030A3Q0054657874436F6C6F723303043Q00536F6C6403053Q005368696E7903063Q00436F6C6F723303073Q0066726F6D524742026Q004440026Q003E40028Q002Q033Q0042757903063Q00412Q63656E74043A4Q009B00045Q0010460004000100012Q009B000400013Q00063900050006000100020004433Q00060001001226010500023Q0010460004000100052Q009B000400024Q00D5000500014Q003101040002000200208A000400040003001226010600044Q003500040006000200261B01040010000100050004433Q001000012Q009F00046Q003C000400013Q0006940103001E000100010004433Q001E00012Q009B000500034Q000C000600043Q00202Q00060006000700102Q0005000600064Q000500036Q000600043Q00202Q00060006000100102Q0005000800064Q000500033Q00302Q0005000100090004433Q0039000100068A0104002F00013Q0004433Q002F00012Q009B000500034Q0063010600043Q00202Q00060006000A00102Q0005000600064Q000500033Q00122Q0006000B3Q00202Q00060006000C00122Q0007000D3Q00122Q0008000E3Q00122Q0009000F6Q00060009000200102Q0005000800064Q000500033Q00302Q00050001001000044Q003900012Q009B000500034Q000C000600043Q00202Q00060006001100102Q0005000600064Q000500036Q000600043Q00202Q00060006000100102Q0005000800064Q000500033Q00302Q0005000100102Q0066012Q00017Q00F73Q00030C3Q0043726561746557696E646F7703053Q005469746C65030A3Q0065796564726F2Q70657203093Q0043726561746554616203043Q004E616D65030D3Q005468616E6B732E204576656E7403043Q0049636F6E03113Q0053656C656374656449636F6E436F6C6F7203053Q0042726F776E030C3Q00437265617465546F2Q676C65030E3Q0057616C6B20746F205475726B6579030E3Q00706572736F6E2D77616C6B696E6703073Q0044656661756C74010003043Q00466C6167030C3Q0057616C6B546F5475726B657903083Q0043612Q6C6261636B030F3Q0053657457616C6B546F5475726B6579030C3Q00437265617465536C6964657203113Q005475726B65792077616C6B2073702Q65642Q033Q004D696E026Q0034402Q033Q004D6178025Q00408E4003093Q00496E6372656D656E74026Q00F03F03123Q005365745475726B657957616C6B53702Q656403113Q004175746F204461696C792052657761726403053Q00636C6F636B030F3Q004175746F4461696C7952657761726403123Q005365744175746F4461696C7952657761726403043Q004D61696E03123Q0033642D637562652D612Q726F772D6C65667403093Q004175746F204C69667403063Q00637572736F7203083Q004175746F4C696674030B3Q005365744175746F4C696674030A3Q004175746F2050756E636803113Q0068616E642D68616C662D652Q6C6970736503093Q004175746F50756E6368030C3Q005365744175746F50756E6368030A3Q004175746F2053746F6D702Q033Q006C656703093Q004175746F53746F6D70030C3Q005365744175746F53746F6D7003103Q004175746F2054652Q7269746F72696573030C3Q006C6F636174696F6E2D70696E030F3Q004175746F54652Q7269746F7269657303123Q005365744175746F54652Q7269746F72696573030C3Q00436F2Q6C65637461626C6573030D3Q0062752Q6C65742D666C79696E67030F3Q0057616C6B20746F2041697264726F70030E3Q0070617065722D616972706C616E65030D3Q0057616C6B546F41697264726F7003103Q0053657457616C6B546F41697264726F7003103Q0057616C6B20746F207461726765742Q20030B3Q004175746F47656D57616C6B030E3Q005365744175746F47656D57616C6B030E3Q0047656D2077616C6B2073702Q6564030F3Q0053657447656D57616C6B53702Q6564030F3Q004175746F2047656D732054772Q656E030F3Q0062752Q746572666C792D77696E6773030C3Q004175746F47656D54772Q656E030F3Q005365744175746F47656D54772Q656E03103Q0047656D732074772Q656E2073702Q6564026Q004940025Q00208C4003103Q0053657447656D54772Q656E53702Q656403043Q00426F2Q7303063Q0073776F72647303103Q004272696E6720412Q6C20626F2Q73657303163Q0074776F2D612Q726F77732D646F776E2D616E642D757003093Q00426F2Q734272696E67030C3Q00536574426F2Q734272696E67030C3Q0057616C6B20746F20426F2Q73030A3Q0057616C6B546F426F2Q73030D3Q0053657457616C6B546F426F2Q73030A3Q00547020746F20626F2Q73030C3Q005470546F426F2Q734B692Q6C030F3Q005365745470546F426F2Q734B692Q6C030D3Q0043726561746553656374696F6E030C3Q00426F2Q7320747261636B657203053Q0073776F726403093Q0049636F6E436F6C6F7203063Q00507572706C6503043Q007461736B03053Q00737061776E03043Q00452Q67732Q033Q00652Q6703173Q004175746F2068617463682073656C656374656420652Q6703143Q004175746F486174636853656C6563746564452Q6703173Q005365744175746F486174636853656C6563746564452Q6703103Q004175746F206861746368207065722031030F3Q004175746F48617463685065724F6E6503123Q005365744175746F48617463685065724F6E6503103Q0053656C65637420452Q672028312D3529026Q00144003133Q0053657453656C6563746564452Q67496E646578030A3Q004375626520776F726C64030D3Q00637562652D7665727465786573030E3Q004175746F20486174636820452Q6703103Q004175746F486174636843756265452Q6703133Q005365744175746F486174636843756265452Q6703043Q0053686F70030D3Q0073686F2Q70696E672D6361727403093Q004175746F2053652Q6C03123Q00636972636C652D646F2Q6C61722D7369676E03083Q004175746F53652Q6C030B3Q005365744175746F53652Q6C03103Q004175746F20427579205765696768747303153Q0073686F2Q70696E672D6261736B65742D636865636B030E3Q004175746F4275795765696768747303113Q005365744175746F42757957656967687473030C3Q004175746F2042757920444E412Q033Q0070696E030A3Q004175746F427579444E41030D3Q005365744175746F427579444E41030F3Q004175746F2042757920426F6469657303103Q00706572736F6E2D63726F756368696E67030D3Q004175746F427579426F6469657303103Q005365744175746F427579426F6469657303063Q004D61726B6574030D3Q006275696C64696E672D62616E6B030C3Q004175746F2042757920412Q6C03193Q004175746F42757953757065726D61726B657457656967687473031C3Q005365744175746F42757953757065726D61726B65745765696768747303133Q004175746F53652Q6C53757065726D61726B657403163Q005365744175746F53652Q6C53757065726D61726B6574031A3Q004175746F53757065726D61726B657454652Q7269746F72696573031D3Q005365744175746F53757065726D61726B657454652Q7269746F7269657303083Q004D65726368616E74030F3Q004D65726368616E74206F2Q6665727303063Q0048692Q64656E2Q0103133Q004D65726368616E742050657420536E69706572030E3Q004D65726368616E74536E6970657203113Q005365744D65726368616E74536E69706572030D3Q0043726561746554657874626F7803093Q004D6178207072696365030B3Q00506C616365686F6C64657203153Q00652E672E20354D2028626C616E6B203D20616E792903133Q005365744D65726368616E744D61785072696365030B3Q004372656174654C6162656C03063Q0054617267657403053Q0056616C756503043Q004E6F6E6503063Q00537461747573034Q00030A3Q005065747320666F756E64030E3Q004F70656E2050657420496E646578025Q00408F40030A3Q0053656172636820706574030B3Q00706574206E616D653Q2E030C3Q0043726561746542752Q746F6E03103Q005265667265736820706574206C69737403183Q00726F746174652D636F756E7465722D636C6F636B7769736503113Q00436C65617220612Q6C207461726765747303013Q007803273Q005069636B20796F75722070657473202874617020746F2073656C6563742F756E73656C6563742903093Q00526967687449636F6E03103Q00612Q726F772D6C617267652D646F776E03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203123Q00437265617465436F726E657242752Q746F6E03113Q00612Q726F772D6C617267652D726967687403043Q005465787403063Q004F2Q6665727303103Q004261636B20746F204D65726368616E7403103Q00612Q726F772D6C617267652D6C65667403063Q0069706169727303053Q00546965723103053Q00546965723203053Q005469657233030C3Q00437265617465427579526F7703043Q004375626503043Q006375626503153Q0055736520617420796F7572206F776E207269736B2103123Q00637562652D7175657374696F6E2D6D61726B030A3Q004175746F20437562657303053Q005374617465030C3Q005365744175746F4375626573030B3Q0054772Q656E2053702Q6564026Q004440030C3Q005365744375626553702Q6564030D3Q0057616C6B20746F204375626573030B3Q0057616C6B546F4375626573030E3Q0053657457616C6B546F437562657303063Q0051756573747303043Q007374617203063Q0059652Q6C6F77030C3Q005072656D69756D204F6E6C7903143Q00612Q726F772D746869636B2D746F2D7269676874030A3Q004175746F20517565737403093Q004175746F5175657374030C3Q005365744175746F517565737403023Q00756903063Q007374617475732Q033Q004F2Q6603073Q0063752Q72656E7403053Q00446F696E6703013Q002D03043Q004D69736303103Q0063726F2Q7368616972732D736C61736803143Q0044697361626C652033442052656E646572696E6703103Q00636F6D70757465722D6D6F6E69746F7203093Q0044697361626C653344030C3Q0053657444697361626C653344030B3Q004175746F2052656A6F696E03133Q00612Q726F772D6375726C2D746F2D7269676874030A3Q004175746F52656A6F696E030D3Q005365744175746F52656A6F696E030E3Q004175746F2052656A6F696E205632030C3Q004175746F52656A6F696E5632030F3Q005365744175746F52656A6F696E563203103Q00437573746F6D2057616C6B53702Q656403063Q00706572736F6E030F3Q0057616C6B53702Q6564546F2Q676C6503123Q0053657457616C6B53702Q6564546F2Q676C65030F3Q0057616C6B53702Q65642056616C7565025Q0070974003113Q0053657457616C6B53702Q656456616C756503103Q00437573746F6D204A756D70506F776572030E3Q00706572736F6E2D66612Q6C696E67030F3Q004A756D70506F776572546F2Q676C6503123Q005365744A756D70506F776572546F2Q676C65030F3Q004A756D70506F7765722056616C7565025Q00407F4003113Q005365744A756D70506F77657256616C7565030D3Q00496E66696E697465204A756D70030C3Q00496E66696E6974654A756D70030F3Q00536574496E66696E6974654A756D7003053Q005374617473030A3Q00636861696E2D6C696E6B2Q033Q00465053031A3Q0063686576726F6E2D6C617267652D636F6E64656E7365642D7570030A3Q004C6967687447722Q656E03013Q0030030C3Q004E6574776F726B2050696E6703103Q00776966692D7369676E616C2D6661697203043Q0030206D73030C3Q00456C61707365642054696D6503083Q002Q303A2Q303A2Q30030A3Q0047656D73202F204D696E03193Q00636C6F636B2D7370696E2D726576657273652D646173686564030B3Q0047656D73204561726E656403073Q006469616D6F6E64030B3Q00526573657420537461747300FB023Q00C27Q00206Q00014Q00023Q00014Q000300013Q00102Q0002000200036Q0002000200122Q000100033Q00202Q00023Q00044Q00043Q000300302Q00040005000600102Q0004000700014Q000500023Q00202Q00050005000900102Q0004000800054Q00020004000200202Q00030002000A4Q00053Q000500302Q00050005000B00302Q00050007000C00302Q0005000D000E00302Q0005000F00104Q000600033Q00202Q00060006001200102Q0005001100064Q00030005000100202Q0003000200134Q00053Q000600302Q00050005001400302Q00050015001600302Q00050017001800302Q0005000D001600302Q00050019001A4Q000600033Q00202Q00060006001B00102Q0005001100064Q00030005000100202Q00030002000A4Q00053Q000500302Q00050005001C00302Q00050007001D00302Q0005000D000E00302Q0005000F001E4Q000600033Q00202Q00060006001F00102Q0005001100064Q00030005000100202Q00033Q00044Q00053Q000200302Q00050005002000302Q0005000700214Q00030005000200202Q00040003000A4Q00063Q000500302Q00060005002200302Q00060007002300302Q0006000D000E00302Q0006000F00244Q000700033Q00202Q00070007002500102Q0006001100074Q00040006000100202Q00040003000A4Q00063Q000500302Q00060005002600302Q00060007002700302Q0006000D000E00302Q0006000F00284Q000700033Q00202Q00070007002900102Q0006001100074Q00040006000100202Q00040003000A4Q00063Q000500302Q00060005002A00302Q00060007002B00302Q0006000D000E00302Q0006000F002C4Q000700033Q00202Q00070007002D00102Q0006001100072Q00E10004000600010020A301040003000A4Q00063Q000500302Q00060005002E00302Q00060007002F00302Q0006000D000E00302Q0006000F00304Q000700033Q00202Q00070007003100102Q0006001100074Q00040006000100202Q00043Q00044Q00063Q000200302Q00060005003200302Q0006000700334Q00040006000200202Q00050004000A4Q00073Q000500302Q00070005003400302Q00070007003500302Q0007000D000E00302Q0007000F00364Q000800033Q00202Q00080008003700102Q0007001100084Q00050007000100202Q00050004000A4Q00073Q000500302Q00070005003800302Q00070007000C00302Q0007000D000E00302Q0007000F00394Q000800033Q00202Q00080008003A00102Q0007001100084Q00050007000100202Q0005000400134Q00073Q000600302Q00070005003B00302Q00070015001600302Q00070017001800302Q0007000D001600302Q00070019001A4Q000800033Q00202Q00080008003C00102Q0007001100084Q00050007000100202Q00050004000A4Q00073Q000500302Q00070005003D00302Q00070007003E00302Q0007000D000E00302Q0007000F003F4Q000800033Q00202Q00080008004000102Q0007001100084Q00050007000100202Q0005000400134Q00073Q000600302Q00070005004100302Q00070015004200302Q00070017004300302Q0007000D004200302Q00070019001A4Q000800033Q00202Q00080008004400102Q0007001100084Q00050007000100202Q00053Q00044Q00073Q000200302Q00070005004500302Q0007000700464Q00050007000200202Q00060005000A4Q00083Q000500302Q00080005004700302Q00080007004800302Q0008000D000E00302Q0008000F00494Q000900033Q00202Q00090009004A0010460008001100092Q009D00060008000100202Q00060005000A4Q00083Q000500302Q00080005004B00302Q00080007004800302Q0008000D000E00302Q0008000F004C4Q000900033Q00202Q00090009004D00102Q0008001100094Q00060008000100202Q00060005000A4Q00083Q000500302Q00080005004E00302Q00080007004800302Q0008000D000E00302Q0008000F004F4Q000900033Q00202Q00090009005000102Q0008001100094Q00060008000100202Q0006000500514Q00083Q000300302Q00080005005200302Q0008000700534Q000900023Q00202Q00090009005500102Q0008005400094Q0006000800014Q00065Q00122Q000700563Q00202Q0007000700570006A300083Q000100032Q009B3Q00044Q00D53Q00064Q00D53Q00054Q006200070002000100202Q00073Q00044Q00093Q000200302Q00090005005800302Q0009000700594Q00070009000200202Q00080007000A4Q000A3Q000500302Q000A0005005A00302Q000A000700590030A8000A000D000E0030D0000A000F005B4Q000B00033Q00202Q000B000B005C00102Q000A0011000B4Q0008000A000100202Q00080007000A4Q000A3Q000500302Q000A0005005D00302Q000A0007005900302Q000A000D000E0030D0000A000F005E4Q000B00033Q00202Q000B000B005F00102Q000A0011000B4Q0008000A000100202Q0008000700134Q000A3Q000600302Q000A0005006000302Q000A0015001A00302Q000A001700610030A8000A000D001A0030A8000A0019001A2Q0055010B00033Q00202Q000B000B006200102Q000A0011000B4Q0008000A000100202Q0008000700514Q000A3Q000200302Q000A0005006300302Q000A000700644Q0008000A000100202Q00080007000A2Q00E0000A3Q00050030A8000A000500650030A8000A000700590030A8000A000D000E0030A8000A000F00662Q009B000B00033Q00207B010B000B00670010F8000A0011000B4Q0008000A000100202Q00083Q00044Q000A3Q000200302Q000A0005006800302Q000A000700692Q00350008000A000200208A00090008000A2Q00E0000B3Q00050030A8000B0005006A0030A8000B0007006B0030A8000B000D000E0030D0000B000F006C4Q000C00033Q00202Q000C000C006D00102Q000B0011000C4Q0009000B000100202Q00090008000A4Q000B3Q000500302Q000B0005006E00302Q000B0007006F00302Q000B000D000E0030A8000B000F00702Q009B000C00033Q00207B010C000C00710010F8000B0011000C4Q0009000B000100202Q00090008000A4Q000B3Q000500302Q000B0005007200302Q000B0007007300305F010B000D000E00302Q000B000F00744Q000C00033Q00202Q000C000C007500102Q000B0011000C4Q0009000B000100202Q00090008000A4Q000B3Q000500302Q000B0005007600302Q000B0007007700302Q000B000D000E00302Q000B000F00784Q000C00033Q00202Q000C000C007900102Q000B0011000C4Q0009000B000100202Q00093Q00044Q000B3Q000200302Q000B0005007A00302Q000B0007007B4Q0009000B000200202Q000A0009000A4Q000C3Q000500302Q000C0005007C00302Q000C0007006F00302Q000C000D000E00302Q000C000F007D4Q000D00033Q00202Q000D000D007E00102Q000C0011000D4Q000A000C000100202Q000A0009000A4Q000C3Q000500302Q000C0005006A00302Q000C0007006B00302Q000C000D000E00302Q000C000F007F4Q000D00033Q00202Q000D000D008000102Q000C0011000D4Q000A000C000100202Q000A0009000A4Q000C3Q000500302Q000C0005002E00302Q000C0007002F00302Q000C000D000E00302Q000C000F00814Q000D00033Q00202Q000D000D008200102Q000C0011000D4Q000A000C000100202Q000A3Q00044Q000C3Q000200302Q000C0005008300302Q000C0007006F4Q000A000C000200202Q000B3Q00044Q000D3Q000200302Q000D0005008400302Q000D008500864Q000B000D000200202Q000C000A000A4Q000E3Q000500302Q000E0005008700302Q000E0007006F00302Q000E000D000E00302Q000E000F00884Q000F00033Q00202Q000F000F008900102Q000E0011000F4Q000C000E000100202Q000C000A008A4Q000E3Q000300302Q000E0005008B00302Q000E008C008D4Q000F00033Q00202Q000F000F008E00102Q000E0011000F4Q000C000E000100202Q000C000A008F2Q00E0000E3Q0002003053000E0005009000302Q000E009100924Q000C000E000200202Q000D000A008F4Q000F3Q000200302Q000F0005009300302Q000F009100944Q000D000F000200202Q000E000A008F4Q00103Q000300302Q00100005009500302Q00100007005900302Q0010009100964Q000E0010000200122Q000F00563Q00202Q000F000F00570006A300100001000100032Q009B3Q00044Q009B3Q00054Q00D53Q000E4Q0030000F000200014Q000F8Q00105Q00122Q001100976Q001200133Q0006A300140002000100052Q00D53Q000C4Q009B3Q00054Q00D53Q000F4Q009B3Q00024Q009B3Q00063Q0006A300150003000100032Q00D53Q00124Q00D53Q000F4Q00D53Q00103Q0006A300160004000100032Q00D53Q00104Q00D53Q00134Q00D53Q00153Q00208A0017000A008A2Q00E000193Q00030030A80019000500980030A80019008C00990006A3001A0005000100022Q00D53Q00164Q00D53Q00153Q00104600190011001A2Q00350017001900022Q00D5001200173Q0006A300170006000100082Q00D53Q000F4Q009B3Q00054Q00D53Q000D4Q00D53Q000A4Q009B3Q00034Q00D53Q00144Q00D53Q00114Q00D53Q00153Q00207D0018000A009A4Q001A3Q000300302Q001A0005009B00302Q001A0007009C00102Q001A001100174Q0018001A000100202Q0018000A009A4Q001A3Q000300302Q001A0005009D00302Q001A0007009E0006A3001B0007000100042Q009B3Q00034Q00D53Q000D4Q009B3Q00054Q00D53Q00143Q00100B011A0011001B4Q0018001A000100202Q0018000A009A4Q001A3Q000500302Q001A0005009F00302Q001A000700594Q001B00023Q00202Q001B001B005500102Q001A0054001B00302Q001A00A000A10006A3001B0008000100022Q00D53Q00164Q00D53Q00103Q0010AA001A0011001B4Q0018001A00024Q001300183Q00202Q0018001300A200202Q00190011001A00102Q001800A300194Q001800166Q00198Q0018000200014Q001800174Q003200180001000100208A0018000A00A42Q00E0001A3Q00030030A8001A000700A50030A8001A00A600A70006A3001B0009000100012Q00D53Q000B3Q0010F8001A0011001B4Q0018001A000100202Q0018000B009A4Q001A3Q000300302Q001A000500A800302Q001A000700A90006A3001B000A000100012Q00D53Q000A3Q0010F8001A0011001B4Q0018001A000100202Q0018000B008F4Q001A3Q000200302Q001A0005009300302Q001A009100942Q00350018001A000200208A0019000B00512Q00E0001B3Q00030030A8001B000500840030A8001B0007006F2Q009B001C00023Q00207B011C001C0055001046001B0054001C2Q00E10019001B00012Q00E000196Q00E0001A5Q001275001B00AA4Q00E9001C00033Q00122Q001D00AB3Q00122Q001E00AC3Q00122Q001F00AD6Q001C000300012Q00A6011B0002001D0004433Q00FB2Q0100208A0020000B00AE2Q00E000223Q00010006A30023000B000100042Q00D53Q001A4Q00D53Q001F4Q009B3Q00054Q00D53Q00183Q0010460022001100232Q00350020002200022Q00440119001F00202Q00D4001E5Q0006C0001B00F02Q0100020004433Q00F02Q01001275001B00563Q00207B011B001B00570006A3001C000C000100042Q009B3Q00044Q009B3Q00054Q00D53Q00194Q00D53Q001A4Q0062001B0002000100202Q001B3Q00044Q001D3Q000200302Q001D000500AF00302Q001D000700B04Q001B001D000200202Q001C001B00514Q001E3Q000300302Q001E000500B100302Q001E000700B22Q009B001F00023Q00207B011F001F00550010F8001E0054001F4Q001C001E000100202Q001C001B000A4Q001E3Q000500302Q001E000500B300302Q001E000700210030A8001E000D000E0006A3001F000D000100012Q009B3Q00073Q001004011E00B4001F4Q001F00033Q00202Q001F001F00B500102Q001E0011001F4Q001C001E000100202Q001C001B00134Q001E3Q000600302Q001E000500B600302Q001E0015001600302Q001E001700B700302Q001E000D00B700302Q001E0019001A4Q001F00033Q00202Q001F001F00B800102Q001E0011001F4Q001C001E000100202Q001C001B000A4Q001E3Q000500302Q001E000500B900302Q001E0007000C00302Q001E000D000E00302Q001E000F00BA4Q001F00033Q00202Q001F001F00BB00102Q001E0011001F4Q001C001E000100202Q001C3Q00044Q001E3Q000300302Q001E000500BC00302Q001E000700BD4Q001F00023Q00202Q001F001F00BE00102Q001E0008001F4Q001C001E000200202Q001D001C00514Q001F3Q000300302Q001F000500BF00302Q001F000700C04Q002000023Q00202Q0020002000BE00102Q001F005400204Q001D001F000100202Q001D001C000A4Q001F3Q000600302Q001F000500C100302Q001F000700BD4Q002000023Q00202Q0020002000BE00102Q001F0054002000302Q001F000D000E00302Q001F000F00C24Q002000033Q00202Q0020002000C300102Q001F001100204Q001D001F00014Q001D00083Q00202Q001D001D00C400202Q001E001C008F4Q00203Q000200302Q00200005009300302Q0020009100C64Q001E0020000200102Q001D00C5001E4Q001D00083Q00202Q001D001D00C400202Q001E001C008F4Q00203Q000200302Q0020000500C800302Q0020009100C94Q001E0020000200102Q001D00C7001E00202Q001D3Q00044Q001F3Q000200302Q001F000500CA00302Q001F000700CB4Q001D001F000200202Q001E001D000A4Q00203Q000500302Q0020000500CC00302Q0020000700CD0030A80020000D000E0030980120000F00CE4Q002100033Q00202Q0021002100CF00102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500D000302Q0020000700D100302Q0020000D000E00302Q0020000F00D24Q002100033Q00202Q0021002100D300102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500D400302Q0020000700D100302Q0020000D000E00302Q0020000F00D54Q002100033Q00202Q0021002100D600102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500D700302Q0020000700D800302Q0020000D000E00302Q0020000F00D94Q002100033Q00202Q0021002100DA00102Q0020001100214Q001E0020000100202Q001E001D00134Q00203Q000600302Q0020000500DB00302Q00200015001600302Q0020001700DC00302Q0020000D001600302Q00200019001A4Q002100033Q00202Q0021002100DD00102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500DE00302Q0020000700DF00302Q0020000D000E00302Q0020000F00E04Q002100033Q00202Q0021002100E100102Q0020001100214Q001E0020000100202Q001E001D00134Q00203Q000600302Q0020000500E200302Q00200015004200302Q0020001700E300302Q0020000D004200302Q00200019001A4Q002100033Q00202Q0021002100E400102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000400302Q0020000500E500302Q0020000D000E00302Q0020000F00E64Q002100033Q00202Q0021002100E700102Q0020001100214Q001E0020000100202Q001E3Q00044Q00203Q000200302Q0020000500E800302Q0020000700E92Q0035001E0020000200208A001F001E008F2Q00E000213Q00040030A80021000500EA0030A80021000700EB2Q009B002200023Q00207B0122002200EC0010460021005400220030A80021009100ED2Q0035001F0021000200202E0020001E008F4Q00223Q000300302Q0022000500EE00302Q0022000700EF00302Q0022009100F04Q00200022000200202Q0021001E008F4Q00233Q000300302Q0023000500F100302Q00230007001D0030A80023009100F22Q003500210023000200202E0022001E008F4Q00243Q000300302Q0024000500F300302Q0024000700F400302Q0024009100ED4Q00220024000200202Q0023001E008F4Q00253Q000300302Q0025000500F500302Q0025000700F60030A80025009100ED2Q003500230025000200208A0024001E009A2Q00E000263Q00030030A80026000500F70030A800260007009C0006A30027000E000100062Q009B3Q00094Q009B3Q000A4Q009B3Q000B4Q00D53Q00214Q00D53Q00234Q00D53Q00223Q0010460026001100272Q00E1002400260001001275002400563Q00207B0124002400570006A30025000F0001000D2Q009B3Q00044Q00D53Q001F4Q009B3Q000C4Q009B3Q000D4Q00D53Q00204Q009B3Q00094Q00D53Q00214Q009B3Q000E4Q009B3Q000B4Q009B3Q000A4Q00D53Q00234Q009B3Q000F4Q00D53Q00224Q00580024000200012Q0066012Q00013Q00103Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03083Q00486974734C65667403073Q004D61784869747303043Q004E616D652Q0103053Q0056616C7565030B3Q004372656174654C6162656C03083Q00746F737472696E672Q033Q00202F202Q033Q0053657403053Q00706169727303083Q00496E7374616E636503073Q0044657374726F792Q00614Q009B7Q00207B014Q000100068A012Q006000013Q0004433Q006000010012753Q00023Q002037014Q000300122Q000100048Q0002000100124Q00053Q00206Q000600122Q000200078Q000200024Q00015Q00064Q004E00013Q0004433Q004E0001001275000200083Q00208A00033Q00092Q001C010300044Q001900023Q00040004433Q004C000100208A00070006000A0012260109000B4Q003500070009000200068A0107004C00013Q0004433Q004C000100208A0007000600060012AB0109000C6Q00070009000200202Q00080006000600122Q000A000D6Q0008000A000200062Q0007004C00013Q0004433Q004C000100068A0108004C00013Q0004433Q004C000100207B01090006000E00209100010009000F00202Q00090007001000202Q000A000800104Q000B00013Q00202Q000C0006000E4Q000B000B000C00062Q000B003F000100010004433Q003F00012Q009B000B00013Q002076000C0006000E4Q000D00023Q00202Q000D000D00114Q000F3Q000200202Q00100006000E00102Q000F000E001000122Q001000126Q001100096Q00100002000200122Q001100133Q001275001200124Q00EF0013000A6Q0012000200024Q00100010001200102Q000F001000104Q000D000F00024Q000B000C000D00044Q004C00012Q009B000B00013Q0020A7000C0006000E4Q000B000B000C00202Q000B000B001400122Q000D00126Q000E00096Q000D0002000200122Q000E00133Q00122Q000F00126Q0010000A6Q000F000200024Q000D000D000F4Q000B000D00010006C000020014000100020004433Q00140001001275000200154Q009B000300014Q00A60102000200040004433Q005D00012Q001C0007000100050006940107005D000100010004433Q005D000100207B01070006001600068A0107005B00013Q0004433Q005B000100207B01070006001600208A0007000700172Q00580007000200012Q009B000700013Q00207E0107000500180006C000020052000100020004433Q005200010004435Q00012Q0066012Q00017Q00083Q0003053Q00416C697665030D3Q00676574496E646578436F756E742Q033Q005365742Q033Q00202F2003143Q004F70656E2050657420496E64657820666972737403043Q007461736B03043Q0077616974027Q004000194Q009B7Q00207B014Q000100068A012Q001800013Q0004433Q001800012Q009B3Q00013Q00207B014Q00022Q004A012Q000100012Q009B000200023Q00208A00020002000300068A012Q001100013Q0004433Q001100012Q00D500045Q001226010500044Q00D5000600014Q00F200040004000600069401040012000100010004433Q00120001001226010400054Q00E1000200040001001265000200063Q00202Q00020002000700122Q000300086Q00020002000100046Q00012Q0066012Q00017Q00123Q002Q033Q00536574030A3Q007461726765745465787403063Q00697061697273030A3Q006973546172676574656403043Q006E616D652Q033Q00726F7703153Q0046696E6446697273744368696C644F66436C612Q7303083Q0055495374726F6B6503053Q00436F6C6F7203063Q00412Q63656E7403063Q005374726F6B65030C3Q005472616E73706172656E6379029A5Q99B93F03163Q0046696E6446697273744368696C64576869636849734103093Q00546578744C6162656C030A3Q0054657874436F6C6F7233030A3Q004C6967687447722Q656E03043Q005465787400374Q004A7Q00206Q00014Q000200013Q00202Q0002000200024Q000200019Q00000100124Q00036Q000100028Q0002000200044Q003400012Q009B000500013Q00205301050005000400202Q0006000400054Q00050002000200202Q00060004000600202Q00060006000700122Q000800086Q00060008000200062Q0006002400013Q0004433Q0024000100068A0105001A00013Q0004433Q001A00012Q009B000700033Q00207B01070007000A0006940107001C000100010004433Q001C00012Q009B000700033Q00207B01070007000B00104600060009000700068A0105002200013Q0004433Q002200010012260107000D3Q00069401070023000100010004433Q002300012Q009B000700043Q0010460006000C000700207B01070004000600208A00070007000E0012260109000F4Q003C000A00014Q00350007000A000200068A0107003400013Q0004433Q0034000100068A0105003100013Q0004433Q003100012Q009B000800033Q00207B01080008001100069401080033000100010004433Q003300012Q009B000800033Q00207B0108000800120010460007001000080006C03Q000A000100020004433Q000A00012Q0066012Q00017Q000B3Q0003063Q00737472696E6703053Q006C6F7765722Q033Q00476574034Q0003063Q006970616972732Q033Q00726F7703073Q0056697369626C6503043Q0066696E6403043Q006E616D65026Q00F03F2Q00283Q0012753Q00013Q00207B014Q00022Q009B00015Q00068A2Q01000A00013Q0004433Q000A00012Q009B00015Q00207B2Q01000100032Q00402Q01000100020006942Q01000B000100010004433Q000B00010012262Q0100044Q0031012Q00020002001275000100054Q009B000200014Q00A62Q01000200030004433Q0025000100207B0106000500062Q009B000700023Q00068A0107002400013Q0004433Q0024000100261B012Q0023000100040004433Q00230001001275000700013Q00203800070007000800122Q000800013Q00202Q00080008000200202Q0009000500094Q0008000200024Q00095Q00122Q000A000A6Q000B00016Q0007000B000200262Q000700230001000B0004433Q002300012Q009F00076Q003C000700013Q0010460006000700070006C000010010000100020004433Q001000012Q0066012Q00017Q00033Q00030C3Q00536574526967687449636F6E030E3Q00612Q726F772D6C617267652D757003103Q00612Q726F772D6C617267652D646F776E01104Q00A2017Q009B000100013Q00068A2Q01000D00013Q0004433Q000D00012Q009B000100013Q00208A00010001000100068A012Q000B00013Q0004433Q000B0001001226010300023Q0006940103000C000100010004433Q000C0001001226010300034Q00E10001000300012Q009B000100024Q00320001000100012Q0066012Q00017Q00013Q00034Q0001093Q00261B012Q0006000100010004433Q000600012Q009B00016Q003C000200014Q00580001000200010004433Q000800012Q009B000100014Q00320001000100012Q0066012Q00017Q00133Q0003063Q006970616972732Q033Q00726F7703073Q0044657374726F79030B3Q006765745065744E616D65732Q033Q0053657403153Q004F70656E2050657420496E6465782066697273742103073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564030C3Q0043726561746542752Q746F6E03043Q004E616D6503083Q0043612Q6C6261636B03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203053Q007461626C6503063Q00696E7365727403043Q006E616D6500483Q0012753Q00014Q009B00016Q00A6012Q000200020004433Q000A000100207B01050004000200068A0105000A00013Q0004433Q000A000100207B01050004000200208A0005000500032Q00580005000200010006C03Q0004000100020004433Q000400012Q00E08Q00A2017Q009B3Q00013Q00207B014Q00042Q0040012Q00010002000694012Q0018000100010004433Q001800012Q009B000100023Q00208A000100010005001226010300064Q00E10001000300012Q0066012Q00014Q009B000100023Q0020FE00010001000500122Q000300076Q00030001000200202Q0003000300084Q000300033Q00122Q000400096Q000500013Q00202Q00050005000A00122Q0006000B6Q0003000300064Q00010003000100122Q000100016Q00028Q00010002000300044Q004100012Q009B000600033Q00208A00060006000C2Q00E000083Q00020010460008000D00050006A300093Q000100052Q009B3Q00044Q00D53Q00054Q009B3Q00024Q009B3Q00014Q009B3Q00053Q00104C0108000E00094Q00060008000200202Q00070006000F4Q000800066Q00080008000400102Q00070010000800122Q000700113Q00202Q0007000700124Q00088Q00093Q000200102Q00090013000500202Q000A0006000F00102Q00090002000A4Q0007000900014Q00045Q0006C000010028000100020004433Q002800012Q009B000100074Q00320001000100012Q009B000100054Q00320001000100012Q0066012Q00013Q00013Q00073Q0003143Q00546F2Q676C654D65726368616E745461726765742Q033Q0053657403073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564001A4Q00A57Q00206Q00014Q000100018Q0002000100064Q001300013Q0004433Q001300012Q009B000200023Q00204101020002000200122Q000400036Q00040001000200202Q0004000400044Q000400043Q00122Q000500056Q000600033Q00202Q00060006000600122Q000700076Q0004000400074Q00020004000100044Q001700012Q009B000200023Q00208A0002000200022Q00D5000400014Q00E10002000400012Q009B000200044Q00320002000100012Q0066012Q00017Q00053Q0003143Q00436C6561724D65726368616E74546172676574732Q033Q0053657403023Q00302F2Q033Q006D617803093Q00207461726765746564000E4Q00F57Q00206Q00016Q000100016Q00013Q00206Q000200122Q000200036Q000300023Q00202Q00030003000400122Q000400056Q0002000200042Q00E13Q000200012Q009B3Q00034Q00323Q000100012Q0066012Q00019Q003Q00054Q0015019Q000100016Q000100018Q000200016Q00017Q00013Q0003073Q005F73656C65637400044Q009B7Q00207B014Q00012Q00323Q000100012Q0066012Q00017Q00013Q0003073Q005F73656C65637400044Q009B7Q00207B014Q00012Q00323Q000100012Q0066012Q00017Q00093Q0003093Q0067657452656D6F746503093Q00617661696C61626C652Q033Q00536574030D3Q004E6F7420617661696C61626C6503103Q0052656D6F7465206E6F7420666F756E6403053Q007063612Q6C03073Q00426F756768742003043Q006E616D65030A3Q00427579206661696C656400284Q00949Q00000100019Q0000014Q000100023Q00202Q0001000100014Q00010001000200064Q000B00013Q0004433Q000B000100207B01023Q000200069401020010000100010004433Q001000012Q009B000200033Q00208A000200020003001226010400044Q00E10002000400010004433Q002700010006942Q010017000100010004433Q001700012Q009B000200033Q00208A000200020003001226010400054Q00E10002000400010004433Q00270001001275000200063Q0006A300033Q000100022Q00D53Q00014Q009B3Q00014Q00310102000200022Q009B000300033Q00208A00030003000300068A0102002500013Q0004433Q00250001001226010500073Q00207B01063Q00082Q00F200050005000600069401050026000100010004433Q00260001001226010500094Q00E10003000500012Q0066012Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q00BE7Q00206Q00014Q000200018Q000200016Q00017Q000C3Q0003053Q00416C69766503093Q006765744F2Q6665727303053Q0070616972732Q033Q0053657403043Q006E616D6503053Q00707269636503093Q00617661696C61626C6503203Q004F70656E20746865204D65726368616E7420746F206C6F6164206F2Q66657273034Q0003043Q007461736B03043Q0077616974026Q00E03F00254Q009B7Q00207B014Q000100068A012Q002400013Q0004433Q002400012Q009B3Q00013Q00207B014Q00022Q0040012Q00010002001275000100034Q009B000200024Q00A62Q01000200030004433Q001D00010006550006000E00013Q0004433Q000E00012Q001C00063Q00042Q009B000700034Q004401070004000600068A0106001800013Q0004433Q0018000100208A00070005000400207B01090006000500207B010A0006000600207B010B000600072Q00E10007000B00010004433Q001D000100208A000700050004001226010900083Q001226010A00094Q003C000B6Q00E10007000B00010006C00001000B000100020004433Q000B00010012750001000A3Q00207B2Q010001000B0012260102000C4Q00580001000200010004435Q00012Q0066012Q00017Q00013Q0003073Q00456E61626C656400044Q009B7Q00207B014Q00012Q00D63Q00024Q0066012Q00017Q00063Q0003023Q006F7303043Q0074696D65028Q002Q033Q0053657403083Q002Q303A2Q303A2Q3003013Q003000153Q00124Q00013Q00206Q00026Q000100029Q0000124Q00038Q00019Q008Q00028Q00033Q00206Q000400122Q000200058Q000200016Q00043Q00206Q000400122Q000200068Q000200016Q00053Q00206Q000400122Q000200068Q000200016Q00017Q00153Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00F03F2Q033Q0053657403083Q00746F737472696E67028Q0003053Q007063612Q6C2Q033Q00206D7303023Q006F7303043Q0074696D6503043Q006D61746803053Q00666C2Q6F72025Q0020AC40026Q004E4003063Q00737472696E6703063Q00666F726D6174030E3Q00253032643A253032643A2530326403053Q0056616C75650003013Q0030005E4Q009B7Q00207B014Q000100068A012Q005D00013Q0004433Q005D00010012753Q00023Q00201E014Q000300122Q000100048Q000200016Q00013Q00206Q000500122Q000200066Q000300026Q000200039Q00000100124Q00073Q00122Q000100083Q0006A300023Q000100022Q00D58Q009B3Q00034Q00580001000200012Q009B000100043Q00208A000100010005001275000300064Q00D500046Q0031010300020002001226010400094Q00F20003000300042Q00E10001000300010012750001000A3Q00207B2Q010001000B2Q00402Q01000100022Q009B000200054Q00AA2Q01000100020012750002000C3Q00207B01020002000D00202201030001000E2Q00310102000200020012750003000C3Q00207B01030003000D00203A01040001000E00202201040004000F2Q003101030002000200203A01040001000F2Q009B000500063Q00208A000500050005001275000700103Q00207B010700070011001226010800124Q00D5000900024Q00D5000A00034Q00D5000B00044Q00130107000B4Q006A01053Q00012Q009B000500074Q004001050001000200068A0105005B00013Q0004433Q005B000100207B0106000500132Q009B000700083Q00261B01070044000100140004433Q004400012Q009B000700084Q00AA010700060007000E1701070044000100070004433Q004400012Q009B000800094Q00B50008000800072Q00A2010800094Q00A2010600084Q00C10007000A3Q00202Q0007000700054Q0009000B6Q000A00096Q0009000A6Q00073Q0001000E2Q00070057000100010004433Q005700012Q009B000700094Q005D0007000700010020BB00070007000F2Q009B0008000C3Q00208A0008000800052Q009B000A000B4Q00D5000B00074Q001C010A000B4Q006A01083Q00010004433Q005B00012Q009B0007000C3Q00208A000700070005001226010900154Q00E10007000900012Q00D47Q0004435Q00012Q0066012Q00013Q00013Q00043Q0003043Q006D61746803053Q00666C2Q6F72030E3Q004765744E6574776F726B50696E67025Q00408F4000093Q0012163Q00013Q00206Q00024Q000100013Q00202Q0001000100034Q00010002000200202Q0001000100046Q000200029Q006Q00017Q00", GetFEnv(), ...);
