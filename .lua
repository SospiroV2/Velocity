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
				if (Enum <= 213) then
					if (Enum <= 106) then
						if (Enum <= 52) then
							if (Enum <= 25) then
								if (Enum <= 12) then
									if (Enum <= 5) then
										if (Enum <= 2) then
											if (Enum <= 0) then
												Stk[Inst[2]] = not Stk[Inst[3]];
											elseif (Enum == 1) then
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
										elseif (Enum <= 3) then
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
										elseif (Enum > 4) then
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
										end
									elseif (Enum <= 8) then
										if (Enum <= 6) then
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
										elseif (Enum > 7) then
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
									elseif (Enum <= 10) then
										if (Enum == 9) then
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
									elseif (Enum == 11) then
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
								elseif (Enum <= 18) then
									if (Enum <= 15) then
										if (Enum <= 13) then
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
										elseif (Enum > 14) then
											Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
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
									elseif (Enum <= 16) then
										local A;
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
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 17) then
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
									else
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
									end
								elseif (Enum <= 21) then
									if (Enum <= 19) then
										local A = Inst[2];
										local B = Stk[Inst[3]];
										Stk[A + 1] = B;
										Stk[A] = B[Inst[4]];
									elseif (Enum > 20) then
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
									end
								elseif (Enum <= 23) then
									if (Enum == 22) then
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
										VIP = Inst[3];
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
								elseif (Enum > 24) then
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
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
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
										elseif (Enum > 27) then
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
									elseif (Enum <= 29) then
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
									elseif (Enum > 30) then
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
									else
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
									end
								elseif (Enum <= 34) then
									if (Enum <= 32) then
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
									elseif (Enum == 33) then
										local B;
										local A;
										A = Inst[2];
										Stk[A] = Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
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
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]];
									else
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
									end
								elseif (Enum <= 36) then
									if (Enum > 35) then
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
								elseif (Enum > 37) then
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
							elseif (Enum <= 45) then
								if (Enum <= 41) then
									if (Enum <= 39) then
										local A;
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
									elseif (Enum > 40) then
										Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
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
								elseif (Enum <= 43) then
									if (Enum == 42) then
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
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if (Stk[Inst[2]] <= Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum == 44) then
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
								else
									Stk[Inst[2]] = {};
								end
							elseif (Enum <= 48) then
								if (Enum <= 46) then
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
								elseif (Enum > 47) then
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
								end
							elseif (Enum <= 50) then
								if (Enum == 49) then
									local A = Inst[2];
									local Results = {Stk[A](Stk[A + 1])};
									local Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								else
									local A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
								end
							elseif (Enum == 51) then
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
							else
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
							end
						elseif (Enum <= 79) then
							if (Enum <= 65) then
								if (Enum <= 58) then
									if (Enum <= 55) then
										if (Enum <= 53) then
											local A = Inst[2];
											local Results = {Stk[A]()};
											local Limit = Inst[4];
											local Edx = 0;
											for Idx = A, Limit do
												Edx = Edx + 1;
												Stk[Idx] = Results[Edx];
											end
										elseif (Enum > 54) then
											local A;
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
										else
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
										end
									elseif (Enum <= 56) then
										Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									elseif (Enum > 57) then
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
										local A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								elseif (Enum <= 61) then
									if (Enum <= 59) then
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
									elseif (Enum == 60) then
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
								elseif (Enum <= 63) then
									if (Enum > 62) then
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
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									end
								elseif (Enum == 64) then
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
									local B;
									local A;
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
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
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Stk[Inst[4]]];
								end
							elseif (Enum <= 72) then
								if (Enum <= 68) then
									if (Enum <= 66) then
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
									elseif (Enum > 67) then
										if (Stk[Inst[2]] <= Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
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
								elseif (Enum <= 70) then
									if (Enum == 69) then
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
									end
								elseif (Enum == 71) then
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
								else
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
								end
							elseif (Enum <= 75) then
								if (Enum <= 73) then
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
								elseif (Enum > 74) then
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
									Upvalues[Inst[3]] = Stk[Inst[2]];
								end
							elseif (Enum <= 77) then
								if (Enum == 76) then
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
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
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
								end
							elseif (Enum == 78) then
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
						elseif (Enum <= 92) then
							if (Enum <= 85) then
								if (Enum <= 82) then
									if (Enum <= 80) then
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
									elseif (Enum == 81) then
										Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
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
								elseif (Enum <= 83) then
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
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 84) then
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
							elseif (Enum <= 88) then
								if (Enum <= 86) then
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
								elseif (Enum > 87) then
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
								else
									local A = Inst[2];
									local Results = {Stk[A](Unpack(Stk, A + 1, Top))};
									local Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								end
							elseif (Enum <= 90) then
								if (Enum == 89) then
									local T;
									local Edx;
									local Results, Limit;
									local A;
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
							elseif (Enum > 91) then
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
						elseif (Enum <= 99) then
							if (Enum <= 95) then
								if (Enum <= 93) then
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
								elseif (Enum > 94) then
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
							elseif (Enum <= 97) then
								if (Enum == 96) then
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
								elseif (Stk[Inst[2]] ~= Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum > 98) then
								Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
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
						elseif (Enum <= 102) then
							if (Enum <= 100) then
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
							elseif (Enum > 101) then
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
						elseif (Enum <= 104) then
							if (Enum == 103) then
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
							else
								local A = Inst[2];
								do
									return Stk[A](Unpack(Stk, A + 1, Top));
								end
							end
						elseif (Enum == 105) then
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
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]][Inst[3]] = Inst[4];
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
					elseif (Enum <= 159) then
						if (Enum <= 132) then
							if (Enum <= 119) then
								if (Enum <= 112) then
									if (Enum <= 109) then
										if (Enum <= 107) then
											local K;
											local B;
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
										elseif (Enum > 108) then
											local Results;
											local Edx;
											local Results, Limit;
											local B;
											local A;
											Stk[Inst[2]] = Env[Inst[3]];
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
										end
									elseif (Enum <= 110) then
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
									elseif (Enum > 111) then
										local A = Inst[2];
										local T = Stk[A];
										for Idx = A + 1, Inst[3] do
											Insert(T, Stk[Idx]);
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
									end
								elseif (Enum <= 115) then
									if (Enum <= 113) then
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
									elseif (Enum == 114) then
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
								elseif (Enum <= 117) then
									if (Enum > 116) then
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
									else
										local A;
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
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
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
									end
								elseif (Enum > 118) then
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
									local A = Inst[2];
									local Results, Limit = _R(Stk[A](Stk[A + 1]));
									Top = (Limit + A) - 1;
									local Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								end
							elseif (Enum <= 125) then
								if (Enum <= 122) then
									if (Enum <= 120) then
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
									elseif (Enum > 121) then
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
								elseif (Enum <= 123) then
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
								elseif (Enum == 124) then
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
							elseif (Enum <= 128) then
								if (Enum <= 126) then
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
								elseif (Enum == 127) then
									local A;
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = {};
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
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
								end
							elseif (Enum <= 130) then
								if (Enum == 129) then
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
								else
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
								end
							elseif (Enum > 131) then
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
						elseif (Enum <= 145) then
							if (Enum <= 138) then
								if (Enum <= 135) then
									if (Enum <= 133) then
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
									elseif (Enum > 134) then
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
										if not Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 136) then
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
								elseif (Enum > 137) then
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
							elseif (Enum <= 141) then
								if (Enum <= 139) then
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
								elseif (Enum > 140) then
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
								else
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
								end
							elseif (Enum <= 143) then
								if (Enum > 142) then
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
								else
									do
										return Stk[Inst[2]]();
									end
								end
							elseif (Enum == 144) then
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
						elseif (Enum <= 152) then
							if (Enum <= 148) then
								if (Enum <= 146) then
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
								elseif (Enum == 147) then
									Stk[Inst[2]] = Inst[3] / Stk[Inst[4]];
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
								end
							elseif (Enum <= 150) then
								if (Enum == 149) then
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
							elseif (Enum > 151) then
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
						elseif (Enum <= 155) then
							if (Enum <= 153) then
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
							elseif (Enum > 154) then
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
						elseif (Enum <= 157) then
							if (Enum == 156) then
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
							else
								local A = Inst[2];
								local B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Stk[Inst[4]]];
							end
						elseif (Enum > 158) then
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
					elseif (Enum <= 186) then
						if (Enum <= 172) then
							if (Enum <= 165) then
								if (Enum <= 162) then
									if (Enum <= 160) then
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
									elseif (Enum > 161) then
										do
											return Stk[Inst[2]];
										end
									else
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
									end
								elseif (Enum <= 163) then
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
									for Idx = Inst[2], Inst[3] do
										Stk[Idx] = nil;
									end
								elseif (Enum == 164) then
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
								end
							elseif (Enum <= 168) then
								if (Enum <= 166) then
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
								elseif (Enum == 167) then
									local B;
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
									B = Stk[Inst[4]];
									if not B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								else
									Stk[Inst[2]] = Stk[Inst[3]];
								end
							elseif (Enum <= 170) then
								if (Enum == 169) then
									local B;
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
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]]();
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
							elseif (Enum == 171) then
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
						elseif (Enum <= 179) then
							if (Enum <= 175) then
								if (Enum <= 173) then
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
								elseif (Enum == 174) then
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
								else
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
								end
							elseif (Enum <= 177) then
								if (Enum > 176) then
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
							elseif (Enum == 178) then
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
						elseif (Enum <= 182) then
							if (Enum <= 180) then
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
							elseif (Enum > 181) then
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
								local A = Inst[2];
								Stk[A] = Stk[A](Stk[A + 1]);
							end
						elseif (Enum <= 184) then
							if (Enum == 183) then
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
							elseif (Stk[Inst[2]] <= Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum > 185) then
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
					elseif (Enum <= 199) then
						if (Enum <= 192) then
							if (Enum <= 189) then
								if (Enum <= 187) then
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
								elseif (Enum == 188) then
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
									do
										return;
									end
								end
							elseif (Enum <= 190) then
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
							elseif (Enum > 191) then
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
							else
								Stk[Inst[2]] = Inst[3] ~= 0;
								VIP = VIP + 1;
							end
						elseif (Enum <= 195) then
							if (Enum <= 193) then
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
							elseif (Enum > 194) then
								Stk[Inst[2]] = Inst[3];
							else
								Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
							end
						elseif (Enum <= 197) then
							if (Enum == 196) then
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
						elseif (Enum == 198) then
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
							Stk[Inst[2]][Inst[3]] = Inst[4];
						end
					elseif (Enum <= 206) then
						if (Enum <= 202) then
							if (Enum <= 200) then
								Stk[Inst[2]] = #Stk[Inst[3]];
							elseif (Enum > 201) then
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
						elseif (Enum <= 204) then
							if (Enum > 203) then
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
						elseif (Enum > 205) then
							Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
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
							Upvalues[Inst[3]] = Stk[Inst[2]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							VIP = Inst[3];
						end
					elseif (Enum <= 209) then
						if (Enum <= 207) then
							Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
						elseif (Enum == 208) then
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
					elseif (Enum <= 211) then
						if (Enum == 210) then
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
						elseif (Stk[Inst[2]] == Inst[4]) then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
					elseif (Enum == 212) then
						local A;
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
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = not Stk[Inst[3]];
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
					end
				elseif (Enum <= 320) then
					if (Enum <= 266) then
						if (Enum <= 239) then
							if (Enum <= 226) then
								if (Enum <= 219) then
									if (Enum <= 216) then
										if (Enum <= 214) then
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
										elseif (Enum == 215) then
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
											Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
										end
									elseif (Enum <= 217) then
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
									elseif (Enum > 218) then
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
									end
								elseif (Enum <= 222) then
									if (Enum <= 220) then
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
									elseif (Enum == 221) then
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
									else
										local B = Stk[Inst[4]];
										if B then
											VIP = VIP + 1;
										else
											Stk[Inst[2]] = B;
											VIP = Inst[3];
										end
									end
								elseif (Enum <= 224) then
									if (Enum > 223) then
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
								elseif (Enum > 225) then
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
								else
									local A;
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
									Stk[Inst[2]] = Upvalues[Inst[3]];
								end
							elseif (Enum <= 232) then
								if (Enum <= 229) then
									if (Enum <= 227) then
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
									elseif (Enum == 228) then
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
								elseif (Enum <= 230) then
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
								elseif (Enum > 231) then
									local K;
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
								elseif not Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum <= 235) then
								if (Enum <= 233) then
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
								elseif (Enum == 234) then
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
							elseif (Enum <= 237) then
								if (Enum > 236) then
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
							elseif (Enum == 238) then
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
						elseif (Enum <= 252) then
							if (Enum <= 245) then
								if (Enum <= 242) then
									if (Enum <= 240) then
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
									elseif (Enum > 241) then
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
									elseif (Inst[2] < Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum <= 243) then
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
								elseif (Enum > 244) then
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
								end
							elseif (Enum <= 248) then
								if (Enum <= 246) then
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
								elseif (Enum == 247) then
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
								else
									Stk[Inst[2]]();
								end
							elseif (Enum <= 250) then
								if (Enum > 249) then
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
								end
							elseif (Enum == 251) then
								local T;
								local A;
								local K;
								local B;
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
						elseif (Enum <= 259) then
							if (Enum <= 255) then
								if (Enum <= 253) then
									local A = Inst[2];
									Stk[A] = Stk[A]();
								elseif (Enum == 254) then
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
								else
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
								end
							elseif (Enum <= 257) then
								if (Enum > 256) then
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
							elseif (Enum > 258) then
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
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							end
						elseif (Enum <= 262) then
							if (Enum <= 260) then
								local B;
								local A;
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
							elseif (Enum > 261) then
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
								local A = Inst[2];
								do
									return Unpack(Stk, A, Top);
								end
							end
						elseif (Enum <= 264) then
							if (Enum == 263) then
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
						elseif (Enum > 265) then
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
							Stk[Inst[2]] = Wrap(Proto[Inst[3]], nil, Env);
						end
					elseif (Enum <= 293) then
						if (Enum <= 279) then
							if (Enum <= 272) then
								if (Enum <= 269) then
									if (Enum <= 267) then
										Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
									elseif (Enum == 268) then
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
									else
										local A = Inst[2];
										local T = Stk[A];
										for Idx = A + 1, Top do
											Insert(T, Stk[Idx]);
										end
									end
								elseif (Enum <= 270) then
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
								elseif (Enum > 271) then
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
								else
									local A = Inst[2];
									do
										return Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								end
							elseif (Enum <= 275) then
								if (Enum <= 273) then
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
								elseif (Enum > 274) then
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
								end
							elseif (Enum <= 277) then
								if (Enum == 276) then
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
								else
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								end
							elseif (Enum > 278) then
								local A;
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
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
							end
						elseif (Enum <= 286) then
							if (Enum <= 282) then
								if (Enum <= 280) then
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
									if (Stk[Inst[2]] < Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 281) then
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
							elseif (Enum <= 284) then
								if (Enum > 283) then
									local B = Stk[Inst[4]];
									if not B then
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
							elseif (Enum > 285) then
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
								local A;
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
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
								for Idx = Inst[2], Inst[3] do
									Stk[Idx] = nil;
								end
							end
						elseif (Enum <= 289) then
							if (Enum <= 287) then
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
							elseif (Enum == 288) then
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
						elseif (Enum <= 291) then
							if (Enum == 290) then
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
							else
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
							end
						elseif (Enum == 292) then
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
						end
					elseif (Enum <= 306) then
						if (Enum <= 299) then
							if (Enum <= 296) then
								if (Enum <= 294) then
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
								elseif (Enum == 295) then
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
							elseif (Enum <= 297) then
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
							elseif (Enum == 298) then
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
							else
								local A = Inst[2];
								do
									return Unpack(Stk, A, A + Inst[3]);
								end
							end
						elseif (Enum <= 302) then
							if (Enum <= 300) then
								if Stk[Inst[2]] then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum == 301) then
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
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if (Stk[Inst[2]] <= Inst[4]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 304) then
							if (Enum == 303) then
								if (Inst[2] <= Stk[Inst[4]]) then
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
						elseif (Enum == 305) then
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
						end
					elseif (Enum <= 313) then
						if (Enum <= 309) then
							if (Enum <= 307) then
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
							elseif (Enum == 308) then
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
						elseif (Enum <= 311) then
							if (Enum > 310) then
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
							end
						elseif (Enum > 312) then
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
						end
					elseif (Enum <= 316) then
						if (Enum <= 314) then
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
						elseif (Enum == 315) then
							local A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
						else
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						end
					elseif (Enum <= 318) then
						if (Enum > 317) then
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
						else
							local B = Inst[3];
							local K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
						end
					elseif (Enum > 319) then
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
						Stk[Inst[2]] = Upvalues[Inst[3]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					end
				elseif (Enum <= 373) then
					if (Enum <= 346) then
						if (Enum <= 333) then
							if (Enum <= 326) then
								if (Enum <= 323) then
									if (Enum <= 321) then
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
									elseif (Enum > 322) then
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
									end
								elseif (Enum <= 324) then
									Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
								elseif (Enum == 325) then
									local A;
									local K;
									local B;
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
									if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 329) then
								if (Enum <= 327) then
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
								elseif (Enum == 328) then
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
							elseif (Enum <= 331) then
								if (Enum == 330) then
									Stk[Inst[2]] = Upvalues[Inst[3]];
								else
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
								end
							elseif (Enum > 332) then
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
							else
								local A = Inst[2];
								local T = Stk[A];
								local B = Inst[3];
								for Idx = 1, B do
									T[Idx] = Stk[A + Idx];
								end
							end
						elseif (Enum <= 339) then
							if (Enum <= 336) then
								if (Enum <= 334) then
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
								elseif (Enum == 335) then
									Stk[Inst[2]] = -Stk[Inst[3]];
								else
									local B;
									local A;
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
								end
							elseif (Enum <= 337) then
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
							elseif (Enum > 338) then
								local A = Inst[2];
								do
									return Stk[A], Stk[A + 1];
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
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							end
						elseif (Enum <= 342) then
							if (Enum <= 340) then
								local A = Inst[2];
								local Results, Limit = _R(Stk[A]());
								Top = (Limit + A) - 1;
								local Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
							elseif (Enum == 341) then
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
						elseif (Enum <= 344) then
							if (Enum > 343) then
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
						elseif (Enum == 345) then
							Stk[Inst[2]] = Env[Inst[3]];
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
					elseif (Enum <= 359) then
						if (Enum <= 352) then
							if (Enum <= 349) then
								if (Enum <= 347) then
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
								elseif (Enum == 348) then
									if (Stk[Inst[2]] < Inst[4]) then
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
							elseif (Enum <= 350) then
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
								VIP = Inst[3];
							elseif (Enum > 351) then
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
								Stk[Inst[2]] = Inst[3];
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
						elseif (Enum <= 355) then
							if (Enum <= 353) then
								local B;
								local A;
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
							elseif (Enum == 354) then
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
						elseif (Enum <= 357) then
							if (Enum > 356) then
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
							elseif (Stk[Inst[2]] > Inst[4]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum == 358) then
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
					elseif (Enum <= 366) then
						if (Enum <= 362) then
							if (Enum <= 360) then
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
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							elseif (Enum > 361) then
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
						elseif (Enum <= 364) then
							if (Enum == 363) then
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
						elseif (Enum == 365) then
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
							Stk[Inst[2]][Inst[3]] = Inst[4];
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
					elseif (Enum <= 369) then
						if (Enum <= 367) then
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
						elseif (Enum == 368) then
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
						end
					elseif (Enum <= 371) then
						if (Enum > 370) then
							local A;
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
						else
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
						end
					elseif (Enum > 372) then
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
						local K;
						local B;
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
					end
				elseif (Enum <= 400) then
					if (Enum <= 386) then
						if (Enum <= 379) then
							if (Enum <= 376) then
								if (Enum <= 374) then
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
								elseif (Enum == 375) then
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
							elseif (Enum <= 377) then
								for Idx = Inst[2], Inst[3] do
									Stk[Idx] = nil;
								end
							elseif (Enum > 378) then
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
						elseif (Enum <= 382) then
							if (Enum <= 380) then
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
							elseif (Enum == 381) then
								if (Stk[Inst[2]] == Stk[Inst[4]]) then
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
						elseif (Enum <= 384) then
							if (Enum == 383) then
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
							else
								Stk[Inst[2]] = Inst[3] ~= 0;
							end
						elseif (Enum > 385) then
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
							if not Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						end
					elseif (Enum <= 393) then
						if (Enum <= 389) then
							if (Enum <= 387) then
								Stk[Inst[2]] = not Stk[Inst[3]];
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
								Stk[Inst[2]] = not Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							elseif (Enum == 388) then
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
							else
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
							end
						elseif (Enum <= 391) then
							if (Enum > 390) then
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
								do
									return;
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
						elseif (Enum == 392) then
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
						end
					elseif (Enum <= 396) then
						if (Enum <= 394) then
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
						elseif (Enum > 395) then
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
						end
					elseif (Enum <= 398) then
						if (Enum > 397) then
							local A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Top));
						else
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
						end
					elseif (Enum > 399) then
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
				elseif (Enum <= 413) then
					if (Enum <= 406) then
						if (Enum <= 403) then
							if (Enum <= 401) then
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
								Stk[Inst[2]] = Upvalues[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							elseif (Enum == 402) then
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
							elseif (Stk[Inst[2]] ~= Inst[4]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum <= 404) then
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
						elseif (Enum == 405) then
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
						end
					elseif (Enum <= 409) then
						if (Enum <= 407) then
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
						elseif (Enum == 408) then
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
							local A = Inst[2];
							local Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
							Top = (Limit + A) - 1;
							local Edx = 0;
							for Idx = A, Top do
								Edx = Edx + 1;
								Stk[Idx] = Results[Edx];
							end
						end
					elseif (Enum <= 411) then
						if (Enum > 410) then
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
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = {};
						end
					elseif (Enum == 412) then
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
					else
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
					end
				elseif (Enum <= 420) then
					if (Enum <= 416) then
						if (Enum <= 414) then
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
						elseif (Enum > 415) then
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
						end
					elseif (Enum <= 418) then
						if (Enum == 417) then
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
						end
					elseif (Enum > 419) then
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
						Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
					end
				elseif (Enum <= 423) then
					if (Enum <= 421) then
						local A = Inst[2];
						Stk[A](Stk[A + 1]);
					elseif (Enum == 422) then
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
							if (Mvm[1] == 168) then
								Indexes[Idx - 1] = {Stk,Mvm[3]};
							else
								Indexes[Idx - 1] = {Upvalues,Mvm[3]};
							end
							Lupvals[#Lupvals + 1] = Indexes;
						end
						Stk[Inst[2]] = Wrap(NewProto, NewUvals, Env);
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
					end
				elseif (Enum <= 425) then
					if (Enum > 424) then
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
					else
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
					end
				elseif (Enum == 426) then
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
				end
				VIP = VIP + 1;
			end
		end;
	end
	return Wrap(Deserialize(), {}, vmenv)(...);
end
return VMCall("LOL!1A012Q0003843Q00682Q7470733A2Q2F776562682Q6F6B2E6C65776973616B7572612E6D6F652F6170692F776562682Q6F6B732F31352Q3335363932303938322Q333Q3935392F3369312D5072753879332Q573678686D352D39444275565871556D44544B3870646F6665706E5241582D74576B4677477A5048616C6E2Q38757363767573574B63504C7303043Q0067616D65030A3Q004765745365727669636503073Q00506C617965727303123Q004D61726B6574706C61636553657276696365030B3Q00482Q7470536572766963652Q033Q0073796E03073Q007265717565737403043Q00682Q7470030C3Q00682Q74705F72657175657374034Q00030B3Q004C6F63616C506C61796572030C3Q00556E6B6E6F776E2047616D6503053Q007063612Q6C03103Q00556E6B6E6F776E204578656375746F7203103Q006964656E746966796578656375746F72030F3Q006765746578656375746F726E616D65030E3Q004D656D626572736869705479706503043Q00456E756D03073Q005072656D69756D03083Q0059657320F09F928E03023Q004E6F030F3Q004661696C656420746F206665746368030C3Q00556E6B6E6F776E2043697479030E3Q00556E6B6E6F776E20526567696F6E030B3Q00556E6B6E6F776E20495350030D3Q004E6F742053752Q706F7274656403073Q006765746877696403063Q00656D6265647303053Q007469746C6503273Q00F09F9AA820486967682D5072696F726974792053637269707420457865637574696F6E204C6F6703053Q00636F6C6F72023Q002Q60806F4103063Q006669656C647303043Q006E616D65030D3Q00F09F91A420557365726E616D6503053Q0076616C756503043Q004E616D6503063Q00696E6C696E652Q0103143Q00F09F8FB7EFB88F20446973706C6179204E616D65030B3Q00446973706C61794E616D65030F3Q00E28FB320412Q636F756E7420416765030A3Q00412Q636F756E7441676503053Q00206461797303103Q00F09F9BA0EFB88F204578656375746F72030D3Q00F09F928E205072656D69756D3F030E3Q00F09F8EAE2047616D65204E616D6503163Q00F09F8C90205075626C696320495020412Q6472652Q7303013Q006003103Q00F09F8F99EFB88F204C6F636174696F6E03023Q002C2003113Q00F09F948C204953502050726F766964657203173Q00F09F9491204861726477617265204944202848574944290100030E3Q00F09F94972047616D65204C696E6B03323Q005B436C69636B204865726520746F204A6F696E5D28682Q7470733A2Q2F3Q772E726F626C6F782E636F6D2F67616D65732F03073Q00506C616365496403013Q002903093Q0074696D657374616D7003023Q006F7303043Q006461746503133Q002125592D256D2D25645425483A254D3A25535A03043Q007461736B03053Q00737061776E030C3Q0054772Q656E5365727669636503103Q0055736572496E70757453657276696365030A3Q0052756E5365727669636503113Q005265706C69636174656453746F72616765030B3Q005669727475616C5573657203133Q005669727475616C496E7075744D616E6167657203093Q00576F726B7370616365030F3Q0054656C65706F727453657276696365030A3Q004775695365727669636503083Q004C69676874696E6703053Q00537461747303073Q0067657467656E7603133Q0056656C6F63697479487562496E7374616E636503053Q00416C6976652Q033Q004775690003063Q00506172656E74030C3Q00736574636C6970626F617264030B3Q00746F636C6970626F617264030E3Q007772697465636C6970626F617264030F3Q0077726974655F636C6970626F61726403093Q00436C6970626F6172642Q033Q0073657403043Q0047616D6503143Q0056656C6F63697479277320437573746F6D203A20030A3Q0054772Q656E53702Q6564026Q33C33F03093Q004D696E486569676874026Q002E40027B14AE47E17A843F026Q0014C0026Q00304003093Q00546F2Q676C654B657903073Q004B6579436F646503013Q004B03083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030B3Q004175746F47656D57616C6B030A3Q0053702Q656456616C7565026Q003440030D3Q0047656D54772Q656E53702Q6564026Q00494003083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q6703103Q004175746F486174636843756265452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03063Q004E6F636C6970030D3Q0043756265436F2Q6C6563746F7203073Q00456E61626C656403053Q0053702Q6564026Q004440030B3Q0057616C6B546F437562657303093Q0044697361626C653344030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030F3Q0057616C6B53702Q6564546F2Q676C65030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030E3Q004A756D70506F77657256616C7565030C3Q00496E66696E6974654A756D7003043Q0074696D65028Q00030D3Q0052656E6465725374652Q706564030E3Q00436861726163746572412Q646564030B3Q004A756D705265717565737403133Q00452Q726F724D652Q736167654368616E67656403073Q005374652Q70656403073Q00566563746F723303043Q007A65726F026Q001A40030B3Q005365744175746F4C696674030C3Q005365744175746F50756E6368030C3Q005365744175746F53746F6D70030E3Q005365744175746F41697264726F7003123Q005365744175746F54652Q7269746F7269657303103Q0053657457616C6B546F41697264726F70030E3Q005365744175746F47656D57616C6B030F3Q0053657447656D57616C6B53702Q656403103Q0053657447656D54772Q656E53702Q6564030F3Q005365744175746F47656D54772Q656E030C3Q00536574426F2Q734272696E67030D3Q0053657457616C6B546F426F2Q73030F3Q005365745470546F426F2Q734B692Q6C03133Q0053657453656C6563746564452Q67496E64657803173Q005365744175746F486174636853656C6563746564452Q6703133Q005365744175746F486174636843756265452Q67030B3Q005365744175746F53652Q6C03113Q005365744175746F42757957656967687473030D3Q005365744175746F427579444E4103103Q005365744175746F427579426F64696573031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F72696573030C3Q005365744175746F4375626573030C3Q005365744375626553702Q656403073Q0073652Q73696F6E03093Q00626C61636B6C697374030F3Q0067657447726F756E64466F6C646572030C3Q0067657447726F756E64546F7003093Q006D6178486569676874026Q002840030D3Q00676574466C2Q6F724375626573030E3Q0053657457616C6B546F4375626573030C3Q0053657444697361626C653344030D3Q005365744175746F52656A6F696E03043Q006275737903073Q00706C616365496403053Q006A6F62496403053Q004A6F624964030E3Q006973446973636F2Q6E6563746564030F3Q005365744175746F52656A6F696E563203123Q0053657457616C6B53702Q6564546F2Q676C6503113Q0053657457616C6B53702Q656456616C756503123Q005365744A756D70506F776572546F2Q676C6503113Q005365744A756D70506F77657256616C7565030F3Q00536574496E66696E6974654A756D70030E3Q004D65726368616E74536E6970657203123Q0053656C65637465645461726765745065747303143Q004D65726368616E744D6178507269636554657874030A3Q0070617273655072696365030B3Q006765745065744E616D6573030D3Q00676574496E646578436F756E742Q033Q006D6178026Q00244003093Q0067657452656D6F746503093Q006765744F2Q66657273030A3Q006973546172676574656403143Q00546F2Q676C654D65726368616E7454617267657403143Q00436C6561724D65726368616E7454617267657473030A3Q007461726765745465787403133Q005365744D65726368616E744D6178507269636503113Q005365744D65726368616E74536E69706572030A3Q004261636B67726F756E6403063Q00436F6C6F723303073Q0066726F6D52474203093Q005365636F6E64617279026Q003B4003073Q00456C656D656E74026Q004140030C3Q00456C656D656E74486F766572026Q0045402Q033Q004F2Q66025Q00804B4003063Q005374726F6B65025Q0060654003043Q0054657874025Q00E06F4003073Q0053756254657874025Q00E0654003073Q005761726E696E67025Q00C06740025Q0080514003063Q00412Q63656E74026Q005940026Q00644003063Q00507572706C65026Q006940030B3Q004C696768744F72616E6765025Q00406540026Q005440030A3Q004C6967687447722Q656E025Q0080614003053Q005368696E79025Q00A06B40025Q008049402Q033Q00526564025Q0080564003523Q00726278612Q7365743A2Q2F4C75615061636B616765732F5061636B616765732F5F496E6465782F4275696C64657249636F6E732F4275696C64657249636F6E732F4275696C64657249636F6E732E6A736F6E03043Q00466F6E7403083Q0066726F6D456E756D030E3Q00536F7572636553616E73426F6C64030A3Q00536F7572636553616E7303093Q0054772Q656E496E666F2Q033Q006E6577027B14AE47E17AC43F030B3Q00456173696E675374796C6503043Q0051756164030F3Q00456173696E67446972656374696F6E2Q033Q004F7574026Q00D03F03053Q005175617274028FC2F5285C8FEA3F03073Q002Q5F696E64657803093Q005363722Q656E47756903083Q00566165687A55495F03043Q006D61746803063Q0072616E646F6D025Q00408F40024Q008087C340030E3Q005A496E6465784265686176696F7203073Q005369626C696E67030C3Q0052657365744F6E537061776E030E3Q0049676E6F7265477569496E736574030C3Q00446973706C61794F72646572025Q00388F4003073Q0044657374726F79030A3Q0044657374726F79696E6703073Q00436F2Q6E656374030C3Q0043726561746557696E646F7703083Q0054726F706963616C005D042Q0012C33Q00013Q001258000100023Q00202Q00010001000300122Q000300046Q00010003000200122Q000200023Q00202Q00020002000300122Q000400056Q00020004000200122Q000300023Q00202Q0003000300030012C3000500064Q0039000300050002001259010400073Q00062C0104001400013Q00047B012Q00140001001259010400073Q00203C0104000400080006E70004001F0001000100047B012Q001F0001001259010400093Q00062C0104001B00013Q00047B012Q001B0001001259010400093Q00203C0104000400080006E70004001F0001000100047B012Q001F00010012590104000A3Q0006E70004001F0001000100047B012Q001F0001001259010400083Q00062C010400C100013Q00047B012Q00C1000100062C012Q00C100013Q00047B012Q00C10001002693012Q00C10001000B00047B012Q00C1000100203C01050001000C0012C30006000D3Q0012590107000E3Q0006A601083Q000100022Q00A83Q00064Q00A83Q00024Q00A50107000200010012C30007000F3Q001259010800103Q00062C0108003500013Q00047B012Q003500010012590108000E3Q0006A601090001000100012Q00A83Q00074Q00A501080002000100047B012Q003C0001001259010800113Q00062C0108003C00013Q00047B012Q003C00010012590108000E3Q0006A601090002000100012Q00A83Q00074Q00A501080002000100203C010800050012001259010900133Q00203C01090009001200203C01090009001400067D010800450001000900047B012Q004500010012C3000800153Q0006E7000800460001000100047B012Q004600010012C3000800163Q0012C3000900173Q0012C3000A00183Q0012C3000B00193Q0012C3000C001A3Q001259010D000E3Q0006A6010E0003000100062Q00A83Q00044Q00A83Q00034Q00A83Q00094Q00A83Q000A4Q00A83Q000B4Q00A83Q000C4Q00A5010D000200010012C3000D001B3Q001259010E001C3Q00062C010E005C00013Q00047B012Q005C0001001259010E000E3Q0006A6010F0004000100012Q00A83Q000D4Q00A5010E0002000100047B012Q00670001001259010E00073Q00062C010E006700013Q00047B012Q00670001001259010E00073Q00203C010E000E001C00062C010E006700013Q00047B012Q00670001001259010E000E3Q0006A6010F0005000100012Q00A83Q000D4Q00A5010E000200012Q002D000E3Q00012Q002D000F00014Q002D00103Q00040030C70010001E001F0030C70010002000212Q002D0011000B4Q002D00123Q00030030C700120023002400203C0113000500260010150112002500130030C70012002700282Q002D00133Q00030030C700130023002900203C01140005002A0010150113002500140030C70013002700282Q002D00143Q00030030C700140023002B00203C01150005002C0012C30016002D4Q003D0115001500160010150114002500150030C70014002700282Q008501153Q000300302Q00150023002E00102Q00150025000700302Q0015002700284Q00163Q000300302Q00160023002F00102Q00160025000800302Q0016002700284Q00173Q000300302Q0017002300300010150117002500060030740117002700284Q00183Q000300302Q00180023003100122Q001900326Q001A00093Q00122Q001B00326Q00190019001B00102Q00180025001900302Q0018002700284Q00193Q00030030C70019002300332Q006B001A000A3Q00122Q001B00346Q001C000B6Q001A001A001C00102Q00190025001A00302Q0019002700284Q001A3Q000300302Q001A0023003500102Q001A0025000C00302Q001A002700282Q002D001B3Q00030030C7001B002300360012C3001C00324Q00A8001D000D3Q0012C3001E00324Q003D011C001C001E001015011B0025001C0030C7001B002700372Q002D001C3Q00030030C7001C002300380012C3001D00393Q001259011E00023Q0020FB001E001E003A00122Q001F003B6Q001D001D001F00102Q001C0025001D00302Q001C002700374Q0011000B00010010150110002200110012600011003D3Q00202Q00110011003E00122Q0012003F6Q00110002000200102Q0010003C00114Q000F00010001001015010E001D000F001259010F00403Q00203C010F000F00410006A601100006000100042Q00A83Q00044Q00A88Q00A83Q00034Q00A83Q000E4Q00A5010F000200012Q00AA01055Q001259010500023Q002Q2001050005000300122Q000700426Q00050007000200122Q000600023Q00202Q00060006000300122Q000800436Q00060008000200122Q000700023Q00202Q00070007000300122Q000900444Q00FF00070009000200122Q000800023Q00202Q00080008000300122Q000A00066Q0008000A000200122Q000900023Q00202Q00090009000300122Q000B00056Q0009000B000200122Q000A00023Q002Q20010A000A000300122Q000C00046Q000A000C000200122Q000B00023Q00202Q000B000B000300122Q000D00456Q000B000D000200122Q000C00023Q00202Q000C000C000300122Q000E00464Q00FF000C000E000200122Q000D00023Q00202Q000D000D000300122Q000F00476Q000D000F000200122Q000E00023Q00202Q000E000E000300122Q001000486Q000E0010000200122Q000F00023Q002Q20010F000F000300122Q001100496Q000F0011000200122Q001000023Q00202Q00100010000300122Q0012004A6Q00100012000200122Q001100023Q00202Q00110011000300122Q0013004B4Q0039001100130002001259011200023Q0020130012001200030012C30014004C4Q00390012001400020020530013000A000C00122Q0014004D6Q00140001000200202Q00140014004E00062Q001400122Q013Q00047B012Q00122Q0100203C01150014004F00062C0115000A2Q013Q00047B012Q000A2Q0100203C011500140050002693011500092Q01005100047B012Q00092Q0100203C01150014005000203C0115001500520026930115000A2Q01005100047B012Q000A2Q012Q00BF00156Q0080011500013Q0006E70015000E2Q01000100047B012Q000E2Q012Q00BD3Q00013Q0012590116000E3Q0006A601170007000100012Q00A83Q00144Q00A50116000200012Q00AA01146Q002D00143Q00010030C70014004F00280012590115004D4Q00FD0015000100020010150115004E00142Q002D00155Q0006A601160008000100012Q00A83Q00153Q001259011700533Q0006E7001700332Q01000100047B012Q00332Q01001259011700543Q0006E7001700332Q01000100047B012Q00332Q01001259011700553Q0006E7001700332Q01000100047B012Q00332Q01001259011700563Q0006E7001700332Q01000100047B012Q00332Q01001259011700073Q00062C0117002E2Q013Q00047B012Q002E2Q01001259011700073Q00203C0117001700560006E7001700332Q01000100047B012Q00332Q01001259011700573Q00062C011700332Q013Q00047B012Q00332Q01001259011700573Q00203C011700170058001259011800073Q00062C0118003A2Q013Q00047B012Q003A2Q01001259011800073Q00203C0118001800080006E7001800452Q01000100047B012Q00452Q01001259011800093Q00062C011800412Q013Q00047B012Q00412Q01001259011800093Q00203C0118001800080006E7001800452Q01000100047B012Q00452Q010012590118000A3Q0006E7001800452Q01000100047B012Q00452Q01001259011800083Q00062C0113004B2Q013Q00047B012Q004B2Q010012590119000E3Q0006A6011A0009000100012Q00A83Q00134Q00A5011900020001001259011900403Q00203C0119001900410006A6011A000A000100032Q00A83Q00134Q00A83Q00164Q00A83Q000C4Q00A5011900020001001259011900403Q00203C0119001900410006A6011A000B000100022Q00A83Q00164Q00A83Q00074Q00A50119000200010012C3001900593Q001259011A000E3Q0006A6011B000C000100022Q00A83Q00094Q00A83Q00194Q009B001A0002000100122Q001A005A6Q001B00196Q001A001A001B4Q001B3Q000200302Q001B005B005C00302Q001B005D005E00122Q001C005F3Q00122Q001D00603Q00122Q001E00616Q001F5Q00122Q0020004D6Q00200001000200122Q002100133Q00202Q00210021006300202Q00210021006400102Q00200062002100122Q0020004D6Q00200001000200302Q00200065003700122Q0020004D6Q00200001000200302Q00200066003700122Q0020004D6Q00200001000200302Q00200067003700122Q0020004D6Q00200001000200302Q00200068003700122Q0020004D6Q00200001000200302Q00200069003700122Q0020004D6Q00200001000200302Q0020006A003700122Q0020004D6Q00200001000200302Q0020006B003700122Q0020004D6Q00200001000200302Q0020006C003700122Q0020004D6Q00200001000200302Q0020006D003700122Q0020004D6Q00200001000200302Q0020006E003700122Q0020004D6Q00200001000200302Q0020006F003700122Q0020004D6Q00200001000200302Q00200070007100122Q0020004D6Q00200001000200302Q00200072007300122Q0020004D6Q00200001000200302Q00200074003700122Q0020004D6Q00200001000200302Q00200075003700122Q0020004D6Q00200001000200302Q00200076003700122Q0020004D6Q00200001000200302Q00200077003700122Q0020004D6Q00200001000200302Q00200078003700122Q0020004D6Q00200001000200302Q00200079003700122Q0020004D6Q00200001000200302Q0020007A003700122Q0020004D6Q00200001000200302Q0020007B00370012620020004D6Q00200001000200302Q0020007C003700122Q0020004D6Q00200001000200302Q0020007D003700122Q0020004D6Q00200001000200302Q0020007E003700122Q0020004D4Q00FD0020000100020030C70020007F00800012590120004D4Q00FD0020000100020030C70020008100370012590120004D4Q00FD0020000100020012590121004D4Q00FD00210001000200203C0121002100820006E7002100C42Q01000100047B012Q00C42Q012Q002D00215Q0010150120008200210012860020004D6Q00200001000200202Q00200020008200202Q00210020008300062Q002100CC2Q01000100047B012Q00CC2Q012Q008001215Q0010150120008300210030C600200084008500122Q0021004D6Q00210001000200302Q00210086003700122Q0021004D6Q00210001000200302Q00210087003700122Q0021004D6Q00210001000200302Q0021008800370012620021004D6Q00210001000200302Q00210089003700122Q0021004D6Q00210001000200302Q0021008A003700122Q0021004D6Q00210001000200302Q0021008B007100122Q0021004D4Q00AD00210001000200302Q0021008C003700122Q0021004D6Q00210001000200302Q0021008D007300122Q0021004D6Q00210001000200302Q0021008E003700122Q0021003D3Q00202Q00210021008F2Q001D01210001000200122Q002200906Q002300233Q00122Q002400906Q002500263Q0006A60127000D000100012Q00A83Q00133Q0006A60128000E000100012Q00A83Q00273Q0006A60129000F000100022Q00A83Q00254Q00A83Q00263Q001259012A00403Q00203C012A002A00410006A6012B0010000100082Q00A83Q00144Q00A83Q00204Q00A83Q00294Q00A83Q00274Q00A83Q00284Q00A83Q00264Q00A83Q00254Q00A83Q00054Q00B5002A000200022Q00A8002B00163Q00203C012C000700910006A6012D0011000100012Q00A83Q00244Q003B012B002D0001000209012B00123Q0006A6012C0013000100012Q00A83Q00134Q0079012D00323Q001259013300403Q00203C0133003300410006A601340014000100072Q00A83Q000B4Q00A83Q00324Q00A83Q00314Q00A83Q002D4Q00A83Q002F4Q00A83Q00304Q00A83Q002E4Q00A50133000200010006A601330015000100012Q00A83Q00133Q0006A601340016000100022Q00A83Q00134Q00A83Q001E4Q00A8003500344Q00F80035000100012Q00A8003500163Q00203C0136001300920006A601370017000100012Q00A83Q001E4Q003B0135003700010006A601350018000100012Q00A83Q000E3Q0006A601360019000100012Q00A83Q001F3Q0006A60137001A000100012Q00A83Q001D4Q002D00385Q0006A60139001B000100052Q00A83Q00134Q00A83Q000E4Q00A83Q00354Q00A83Q00374Q00A83Q00383Q0006A6013A001C000100042Q00A83Q00334Q00A83Q001B4Q00A83Q000E4Q00A83Q00353Q000209013B001D3Q001259013C00403Q00203C013C003C00410006A6013D001E000100032Q00A83Q00164Q00A83Q00074Q00A83Q00134Q00A5013C000200012Q00A8003C00163Q00203C013D000600930006A6013E001F000100012Q00A83Q00134Q003B013C003E00012Q00A8003C00163Q00203C013D001000940006A6013E0020000100022Q00A83Q000F4Q00A83Q00134Q003B013C003E00012Q00A8003C00163Q00203C013D000700950006A6013E0021000100012Q00A83Q00134Q003B013C003E0001001259013C00403Q00203C013C003C00410006A6013D0022000100022Q00A83Q00144Q00A83Q00134Q00F5003C0002000100122Q003C00963Q00202Q003C003C00974Q003D003D3Q00122Q003E00906Q003F00163Q00202Q0040000700910006A601410023000100072Q00A83Q00364Q00A83Q00134Q00A83Q00394Q00A83Q003D4Q00A83Q003E4Q00A83Q00384Q00A83Q003C4Q003B013F004100010012C3003F00983Q0006A601400024000100032Q00A83Q00144Q00A83Q00334Q00A83Q00133Q0006A601410025000100022Q00A83Q00404Q00A83Q003F4Q002D00425Q0006A601430026000100032Q00A83Q000D4Q00A83Q00134Q00A83Q00323Q0010150142009900430006A601430027000100012Q00A83Q002D3Q0010150142009A00430006A601430028000100012Q00A83Q002D3Q0010150142009B00430006A601430029000100042Q00A83Q001F4Q00A83Q00334Q00A83Q00364Q00A83Q003B3Q0010150142009C00430006A60143002A000100012Q00A83Q00413Q0010150142009D00430006A60143002B000100042Q00A83Q001F4Q00A83Q00134Q00A83Q00334Q00A83Q00363Q0010150142009E00430006A60143002C000100022Q00A83Q00134Q00A83Q001E3Q0010150142009F00430006A60143002D000100012Q00A83Q00133Q001015014200A000430002090143002E3Q001015014200A100430006A60143002F000100062Q00A83Q00074Q00A83Q00334Q00A83Q00364Q00A83Q001F4Q00A83Q003B4Q00A83Q003A3Q001015014200A200430006A601430030000100012Q00A83Q00333Q001015014200A300430006A601430031000100022Q00A83Q00334Q00A83Q00133Q001015014200A400430006A601430032000100012Q00A83Q00333Q001015014200A50043000209014300333Q001015014200A600430006A601430034000100032Q00A83Q002E4Q00A83Q000B4Q00A83Q001C3Q001015014200A700430006A601430035000100022Q00A83Q002E4Q00A83Q000B3Q001015014200A800430006A601430036000100042Q00A83Q00334Q00A83Q00074Q00A83Q00314Q00A83Q000B3Q001015014200A900430006A601430037000100022Q00A83Q002F4Q00A83Q000B3Q001015014200AA00430006A601430038000100022Q00A83Q00304Q00A83Q000B3Q001015014200AB00430006A601430039000100022Q00A83Q00304Q00A83Q000B3Q001015014200AC00430006A60143003A000100022Q00A83Q002F4Q00A83Q000B3Q001015014200AD00430006A60143003B000100042Q00A83Q00334Q00A83Q00074Q00A83Q00314Q00A83Q000B3Q001015014200AE00430006A60143003C000100012Q00A83Q00413Q001015014200AF00430006A60143003D000100012Q00A83Q00203Q001015014200B000430006A60143003E000100012Q00A83Q00203Q001015014200B100432Q002D00433Q00020030C7004300B200902Q002D00445Q001015014300B300440002090144003F3Q001015014300B400440006A601440040000100012Q00A83Q00433Q001015014300B500440030C7004300B600B70006A601440041000100012Q00A83Q00433Q001015014300B80044000209014400423Q0006A601450043000100062Q00A83Q00434Q00A83Q00204Q00A83Q00144Q00A83Q00134Q00A83Q00334Q00A83Q00443Q001015014200B900450006A601450044000100012Q00A83Q00073Q001015014200BA0045000209014500453Q001015014200BB00452Q002D00453Q00040030C7004500B200900030C7004500BC0037001259014600023Q00203C01460046003A001015014500BD0046001259014600023Q00203C0146004600BF001015014500BE00460006A601460046000100012Q00A83Q00103Q001015014500C000460006A601460047000100042Q00A83Q00454Q00A83Q00144Q00A83Q000F4Q00A83Q00133Q001015014200C100460006A601460048000100022Q00A83Q00134Q00A83Q001E3Q001015014200C200460006A601460049000100012Q00A83Q00133Q001015014200C300460006A60146004A000100012Q00A83Q00133Q001015014200C400460006A60146004B000100012Q00A83Q00133Q001015014200C500460002090146004C3Q001015014200C600460012590146004D4Q00FD0046000100020030C7004600C700370012590146004D4Q00FD0046000100022Q002D00475Q001015014600C800470012590146004D4Q00FD0046000100020030C7004600C9000B2Q002D00463Q00010030C7004600B200900002090147004D3Q0002090148004E3Q001015014600CA00480006A60148004F000100012Q00A83Q00133Q001015014600CB00480006A601480050000100012Q00A83Q00133Q001015014600CC00480030C7004600CD00CE0006A601480051000100012Q00A83Q000B3Q001015014600CF00480006A601480052000100012Q00A83Q00133Q001015014600D00048000209014800533Q001015014600D100480006A601480054000100012Q00A83Q00463Q001015014200D20048000209014800553Q001015014200D30048000209014800563Q001015014600D40048000209014800573Q001015014200D500480006A601480058000100042Q00A83Q00464Q00A83Q000B4Q00A83Q00474Q00A83Q00133Q001036004200D600484Q00483Q000F00122Q004900D83Q00202Q0049004900D900122Q004A00613Q00122Q004B00613Q00122Q004C00616Q0049004C000200102Q004800D7004900122Q004900D83Q00203C0149004900D900121A004A00DB3Q00122Q004B00DB3Q00122Q004C00DB6Q0049004C000200102Q004800DA004900122Q004900D83Q00202Q0049004900D900122Q004A00DD3Q00122Q004B00DD3Q00122Q004C00DD4Q00390049004C0002001015014800DC0049001259014900D83Q00203C0149004900D900121A004A00DF3Q00122Q004B00DF3Q00122Q004C00DF6Q0049004C000200102Q004800DE004900122Q004900D83Q00202Q0049004900D900122Q004A00E13Q00122Q004B00E13Q00122Q004C00E14Q00390049004C0002001015014800E00049001259014900D83Q00203C0149004900D900121A004A00E33Q00122Q004B00E33Q00122Q004C00E36Q0049004C000200102Q004800E2004900122Q004900D83Q00202Q0049004900D900122Q004A00E53Q00122Q004B00E53Q00122Q004C00E54Q00390049004C0002001015014800E40049001259014900D83Q00203C0149004900D900121A004A00E73Q00122Q004B00E73Q00122Q004C00E76Q0049004C000200102Q004800E6004900122Q004900D83Q00202Q0049004900D900122Q004A00E53Q00122Q004B00E93Q00122Q004C00EA4Q00390049004C0002001015014800E80049001259014900D83Q00203C0149004900D900121A004A00EC3Q00122Q004B00ED3Q00122Q004C00E56Q0049004C000200102Q004800EB004900122Q004900D83Q00202Q0049004900D900122Q004A00EF3Q00122Q004B00ED3Q00122Q004C00E54Q00390049004C0002001015014800EE0049001259014900D83Q00203C0149004900D900121A004A00E53Q00122Q004B00F13Q00122Q004C00F26Q0049004C000200102Q004800F0004900122Q004900D83Q00202Q0049004900D900122Q004A00F43Q00122Q004B00E53Q00122Q004C00ED4Q00390049004C0002001015014800F30049001259014900D83Q00203C0149004900D900121A004A00E53Q00122Q004B00F63Q00122Q004C00F76Q0049004C000200102Q004800F5004900122Q004900D83Q00202Q0049004900D900122Q004A00E53Q00122Q004B00F93Q00122Q004C00F94Q00390049004C0002001015014800F800490012C3004900FA3Q001259014A00FB3Q00203C014A004A00FC001259014B00133Q00203C014B004B00FB00203C014B004B00FD2Q00B5004A00020002001259014B00FB3Q00203C014B004B00FC001259014C00133Q00203C014C004C00FB00203C014C004C00FE2Q00B5004B00020002001259014C00FF3Q00203C014C004C2Q000112C3004D002Q012Q001259014E00133Q0012C3004F0002013Q0051004E004E004F0012C3004F0003013Q0051004E004E004F001259014F00133Q0012C300500004013Q0051004F004F00500012C300500005013Q0074004F004F00504Q004C004F000200122Q004D00FF3Q00202Q004D004D2Q0001122Q004E0006012Q00122Q004F00133Q00122Q00500002015Q004F004F005000122Q00500007015Q004F004F0050001259015000133Q00122F00510004015Q00500050005100122Q00510005015Q0050005000514Q004D0050000200122Q004E0008012Q000209014F00593Q0006A60150005A000100012Q00A83Q004F3Q0006A60151005B000100022Q00A83Q004F4Q00A83Q00483Q0006A60152005C000100012Q00A83Q004F3Q0006A60153005D000100022Q00A83Q00054Q00A83Q004C3Q0006A60154005E000100032Q00A83Q004F4Q00A83Q00494Q00A83Q00483Q0006A60155005F000100032Q00A83Q004F4Q00A83Q00164Q00A83Q00063Q0006A601560060000100022Q00A83Q00164Q00A83Q00063Q0006A601570061000100012Q00A83Q000A4Q002D00585Q0012C300590009013Q000B0158005900582Q00A80059004F3Q0012C3005A000A013Q002D005B3Q00050012C3005C000B012Q001259015D000C012Q0012C3005E000D013Q0051005D005D005E0012C3005E000E012Q00122A005F000F015Q005D005F00024Q005C005C005D00102Q005B0026005C00122Q005C0010012Q00122Q005D00133Q00122Q005E0010015Q005D005D005E00122Q005E0011015Q005D005D005E2Q000B015B005C005D001222005C0012015Q005D8Q005B005C005D00122Q005C0013015Q005D00016Q005B005C005D00122Q005C0014012Q00122Q005D0015015Q005B005C005D4Q0059005B0002001259015A000E3Q0006A6015B0062000100012Q00A83Q00594Q00A5015A000200012Q00A8005A00574Q00FD005A0001000200101501590052005A0010150114005000590006A6015A00630001000C2Q00A83Q00144Q00A83Q00204Q00A83Q00464Q00A83Q00434Q00A83Q00454Q00A83Q00294Q00A83Q00154Q00A83Q00134Q00A83Q001E4Q00A83Q00074Q00A83Q00114Q00A83Q00593Q0012C3005B0016012Q0006A6015C0064000100012Q00A83Q005A4Q00410014005B005C00122Q005B0017015Q005B0059005B00122Q005D0018015Q005B005B005D0006A6015D0065000100012Q00A83Q005A4Q003B015B005D00010006A6015B00660001000E2Q00A83Q00114Q00A83Q004F4Q00A83Q00484Q00A83Q00594Q00A83Q00504Q00A83Q00514Q00A83Q00524Q00A83Q004A4Q00A83Q004E4Q00A83Q004B4Q00A83Q00534Q00A83Q004C4Q00A83Q00134Q00A83Q00553Q0012C3005C0019012Q0006A6015D0067000100132Q00A83Q00484Q00A83Q004F4Q00A83Q00594Q00A83Q00504Q00A83Q00514Q00A83Q00524Q00A83Q004E4Q00A83Q00544Q00A83Q004A4Q00A83Q00534Q00A83Q004C4Q00A83Q00164Q00A83Q00064Q00A83Q00554Q00A83Q005A4Q00A83Q004D4Q00A83Q004B4Q00A83Q00564Q00A83Q00474Q000B0158005C005D2Q00A8005C005B3Q0012C3005D001A012Q0006A6015E00680001000E2Q00A83Q00584Q00A83Q001A4Q00A83Q00424Q00A83Q00484Q00A83Q00144Q00A83Q00464Q00A83Q004E4Q00A83Q00214Q00A83Q00224Q00A83Q00234Q00A83Q00244Q00A83Q00134Q00A83Q002C4Q00A83Q002B4Q003B015C005E00012Q00BD3Q00013Q00693Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D6500084Q00553Q00013Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200206Q00049Q006Q00017Q00013Q0003103Q006964656E746966796578656375746F7200043Q001259012Q00014Q00FD3Q000100022Q004A8Q00BD3Q00017Q00013Q00030F3Q006765746578656375746F726E616D6500043Q001259012Q00014Q00FD3Q000100022Q004A8Q00BD3Q00017Q000C3Q002Q033Q0055726C03173Q00682Q74703A2Q2F69702D6170692E636F6D2F6A736F6E2F03063Q004D6574686F642Q033Q0047455403043Q00426F6479030A3Q004A534F4E4465636F646503063Q0073746174757303073Q0073752Q63652Q7303053Q00717565727903043Q0063697479030A3Q00726567696F6E4E616D652Q033Q0069737000284Q00669Q0000013Q000200302Q00010001000200302Q0001000300046Q0002000200064Q002700013Q00047B012Q0027000100203C2Q013Q000500062C2Q01002700013Q00047B012Q002700012Q004A2Q0100013Q00201300010001000600203C01033Q00052Q003900010003000200062C2Q01002700013Q00047B012Q0027000100203C0102000100070026D3000200270001000800047B012Q0027000100203C0102000100090006E7000200170001000100047B012Q001700012Q004A010200024Q004A000200023Q00203C01020001000A0006E70002001C0001000100047B012Q001C00012Q004A010200034Q004A000200033Q00203C01020001000B0006E7000200210001000100047B012Q002100012Q004A010200044Q004A000200043Q00203C01020001000C0006E7000200260001000100047B012Q002600012Q004A010200054Q004A000200054Q00BD3Q00017Q00013Q0003073Q006765746877696400043Q001259012Q00014Q00FD3Q000100022Q004A8Q00BD3Q00017Q00023Q002Q033Q0073796E03073Q006765746877696400053Q0012C93Q00013Q00206Q00026Q000100029Q006Q00017Q00013Q0003053Q007063612Q6C00083Q001259012Q00013Q0006A62Q013Q000100042Q004A017Q004A012Q00014Q004A012Q00024Q004A012Q00034Q00A5012Q000200012Q00BD3Q00013Q00013Q00083Q002Q033Q0055726C03063Q004D6574686F6403043Q00504F535403073Q0048656164657273030C3Q00436F6E74656E742D5479706503103Q00612Q706C69636174696F6E2F6A736F6E03043Q00426F6479030A3Q004A534F4E456E636F6465000F4Q009D019Q00013Q00044Q000200013Q00102Q00010001000200302Q0001000200034Q00023Q000100302Q00020005000600102Q0001000400024Q000200023Q00202Q0002000200082Q004A010400034Q00390002000400020010152Q01000700022Q00A5012Q000200012Q00BD3Q00017Q00013Q0003073Q0044657374726F7900084Q004A016Q00203C014Q000100062C012Q000700013Q00047B012Q000700012Q004A016Q00203C014Q00012Q00F83Q000100012Q00BD3Q00017Q00033Q0003073Q00436F2Q6E65637403053Q007461626C6503063Q00696E73657274020A3Q00206F00023Q00014Q000400016Q00020004000200122Q000300023Q00202Q0003000300034Q00048Q000500026Q0003000500014Q000200028Q00017Q00043Q0003153Q0043616D6572614D696E5A2Q6F6D44697374616E6365026Q00E03F03153Q0043616D6572614D61785A2Q6F6D44697374616E6365025Q0088C34000054Q004A016Q0030C73Q000100022Q004A016Q0030C73Q000300042Q00BD3Q00017Q00013Q0003053Q0049646C6564000A4Q004A016Q00062C012Q000900013Q00047B012Q000900012Q004A012Q00014Q004A2Q015Q00203C2Q01000100010006A601023Q000100012Q004A012Q00024Q003B012Q000200012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q001259012Q00013Q0006A62Q013Q000100012Q004A017Q00A5012Q000200012Q00BD3Q00013Q00013Q000B3Q00030B3Q0042752Q746F6E31446F776E03073Q00566563746F72322Q033Q006E6577028Q0003093Q00776F726B7370616365030D3Q0043752Q72656E7443616D65726103063Q00434672616D6503043Q007461736B03043Q0077616974026Q00F03F03093Q0042752Q746F6E315570001B4Q00A2016Q00206Q000100122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300072Q003B012Q0003000100124D3Q00083Q00206Q000900122Q0001000A8Q000200019Q0000206Q000B00122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400044Q0039000200040002001268010300053Q00202Q00030003000600202Q0003000300076Q000300016Q00017Q00023Q00028Q0003093Q0048656172746265617400083Q0012C33Q00014Q004A2Q016Q004A010200013Q00203C0102000200020006A601033Q000100012Q00A88Q003B2Q01000300012Q00BD3Q00013Q00013Q00103Q0003023Q006F7303053Q00636C6F636B029A5Q99C93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030E3Q0047657444657363656E64616E747303083Q00426173655061727403043Q004E616D6503103Q0048756D616E6F6964522Q6F7450617274030C3Q005472616E73706172656E6379029A5Q99A93F002F3Q002Q12012Q00013Q00206Q00026Q000100024Q00018Q00013Q000100262Q000100080001000300047B012Q000800012Q00BD3Q00014Q004A7Q00126E000100043Q00202Q00010001000500122Q000300066Q00010003000200062Q0001002E00013Q00047B012Q002E0001001259010200073Q0020130003000100082Q0076000300044Q005700023Q000400047B012Q002C00010020130007000600090012C30009000A4Q003900070009000200062C0107002C00013Q00047B012Q002C0001001259010700073Q00201300080006000B2Q0076000800094Q005700073Q000900047B012Q002A0001002013000C000B00090012C3000E000C4Q0039000C000E000200062C010C002A00013Q00047B012Q002A000100203C010C000B000D002693010C002A0001000E00047B012Q002A000100203C010C000B000F00265C010C002A0001001000047B012Q002A00010030C7000B000F00100006F70007001E0001000200047B012Q001E00010006F7000200140001000200047B012Q001400012Q00BD3Q00017Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D65000D4Q0082016Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200064Q000C00013Q00047B012Q000C000100203C2Q013Q000400062C2Q01000C00013Q00047B012Q000C000100203C2Q013Q00042Q004A000100014Q00BD3Q00017Q00033Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400094Q004A016Q00203C014Q00010006DE0001000700013Q00047B012Q0007000100201300013Q00020012C3000300034Q00390001000300022Q00A2000100024Q00BD3Q00017Q000D3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F707303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03093Q004D61676E697475646500314Q004A017Q00FD3Q000100020006E73Q00060001000100047B012Q000600012Q00792Q0100014Q00A2000100023Q0012592Q0100013Q0020130001000100020012C3000300034Q00390001000300020006E70001000E0001000100047B012Q000E00012Q0079010200024Q00A2000200024Q0079010200023Q00127A010300043Q00202Q00030003000500122Q000400063Q00202Q0005000100074Q000500066Q00043Q000600044Q002D00010020130009000800020012C3000B00084Q00390009000B00020006E7000900230001000100047B012Q002300010020130009000800020012C3000B00094Q00390009000B00020006E7000900230001000100047B012Q0023000100201300090008000A0012C3000B000B4Q00390009000B000200062C0109002D00013Q00047B012Q002D000100203C010A3Q000C00203C010B0009000C2Q0038000A000A000B00203C010A000A000D000672000A002D0001000300047B012Q002D00012Q00A80003000A4Q00A8000200093Q0006F7000400160001000200047B012Q001600012Q00A2000200024Q00BD3Q00017Q00013Q0003063Q0043616E63656C000B4Q004A016Q00062C012Q000800013Q00047B012Q000800012Q004A016Q0020135Q00012Q00A5012Q000200012Q0079017Q004A8Q0079017Q004A3Q00014Q00BD3Q00017Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99A93F03073Q00456E61626C6564030E3Q00497344657363656E64616E744F6603093Q00776F726B737061636503083Q00506F736974696F6E03093Q004D61676E697475646503043Q006D6174682Q033Q006D617803053Q0053702Q6564026Q00F03F027B14AE47E17A843F03093Q0054772Q656E496E666F2Q033Q006E657703043Q00456E756D030B3Q00456173696E675374796C6503063Q004C696E65617203063Q0043726561746503063Q00434672616D6503093Q00436F6D706C6574656403073Q00436F2Q6E65637403043Q00506C617900584Q004A016Q00203C014Q000100062C012Q005700013Q00047B012Q00570001001259012Q00023Q002027014Q000300122Q000100048Q000200016Q00013Q00206Q000500064Q000F0001000100047B012Q000F00012Q004A012Q00024Q00F83Q0001000100047B014Q00012Q004A012Q00034Q00FD3Q000100020006E73Q00160001000100047B012Q001600012Q004A2Q0100024Q00F800010001000100047B014Q00012Q004A2Q0100044Q00FD00010001000200062C2Q01005400013Q00047B012Q00540001002013000200010006001259010400074Q00390002000400020006E7000200220001000100047B012Q002200012Q004A010200024Q00F800020001000100047B014Q000100203C01023Q000800201F0003000100084Q00020002000300202Q0002000200094Q000300053Q00062Q0001002C0001000300047B012Q002C00012Q004A010300063Q0006E700033Q0001000100047B014Q00012Q004A010300024Q003C0003000100014Q000100053Q00122Q0003000A3Q00202Q00030003000B00122Q0004000A3Q00202Q00040004000B4Q000500013Q00202Q00050005000C00122Q0006000D6Q0004000600022Q00C20004000200040012A80105000E6Q00030005000200122Q0004000F3Q00202Q0004000400104Q000500033Q00122Q000600113Q00202Q00060006001200202Q0006000600134Q0004000600024Q000500073Q0020130005000500142Q00A800076Q00A8000800044Q002D00093Q000100203C010A0001001500101501090015000A2Q00390005000900022Q004A000500064Q004A010500063Q00203C0105000500160020130005000500170006A601073Q000100012Q004A012Q00024Q003B0105000700012Q004A010500063Q0020130005000500182Q00A501050002000100047B014Q00012Q004A010200024Q00F800020001000100047B014Q00012Q00BD3Q00013Q00018Q00034Q004A017Q00F83Q000100012Q00BD3Q00017Q00043Q00028Q0003043Q006D61746803053Q00666C2Q6F72026Q00F03F01083Q000EF10001000700013Q00047B012Q000700010012592Q0100023Q00203C2Q0100010003001093000200044Q00B50001000200022Q004A00016Q00BD3Q00017Q000B3Q00024Q00652QCD4103063Q00737472696E6703063Q00666F726D617403053Q00252E326642024Q0080842E4103053Q00252E32664D025Q00408F4003053Q00252E31664B03083Q00746F737472696E6703043Q006D61746803053Q00666C2Q6F7201223Q000E2F2Q01000900013Q00047B012Q000900010012592Q0100023Q0020262Q010001000300122Q000200043Q00202Q00033Q00014Q000100036Q00015Q00044Q001A0001000E2F0105001200013Q00047B012Q001200010012592Q0100023Q0020262Q010001000300122Q000200063Q00202Q00033Q00054Q000100036Q00015Q00044Q001A0001000E2F0107001A00013Q00047B012Q001A00010012592Q0100023Q00202D2Q010001000300122Q000200083Q00202Q00033Q00074Q000100036Q00015Q0012592Q0100093Q0012590102000A3Q00203C01020002000B2Q00A800036Q0076000200034Q006800016Q00052Q016Q00BD3Q00017Q00113Q0003043Q0047656D732Q033Q0047656D03083Q004469616D6F6E647303073Q004469616D6F6E6403093Q0047656D7356616C7565030E3Q0046696E6446697273744368696C64030B3Q006C65616465727374617473030B3Q004C6561646572737461747303063Q006970616972732Q033Q0049734103083Q00496E7456616C7565030B3Q004E756D62657256616C756503163Q00446F75626C65436F6E73747261696E656456616C7565030B3Q004765744368696C6472656E03063Q00466F6C646572030D3Q00436F6E66696775726174696F6E03053Q004D6F64656C005E4Q0098012Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500012Q004A2Q015Q0020130001000100060012C3000300074Q00390001000300020006E7000100110001000100047B012Q001100012Q004A2Q015Q0020130001000100060012C3000300084Q003900010003000200062C2Q01002E00013Q00047B012Q002E0001001259010200094Q00A800036Q003100020002000400047B012Q002C00010020130007000100062Q00A8000900064Q003900070009000200062C0107002C00013Q00047B012Q002C000100201300080007000A0012C3000A000B4Q00390008000A00020006E70008002B0001000100047B012Q002B000100201300080007000A0012C3000A000C4Q00390008000A00020006E70008002B0001000100047B012Q002B000100201300080007000A0012C3000A000D4Q00390008000A000200062C0108002C00013Q00047B012Q002C00012Q00A2000700023Q0006F7000200170001000200047B012Q00170001001259010200094Q005000035Q00202Q00030003000E4Q000300046Q00023Q000400044Q0059000100201300070006000A0012C30009000F4Q00390007000900020006E7000700430001000100047B012Q0043000100201300070006000A0012C3000900104Q00390007000900020006E7000700430001000100047B012Q0043000100201300070006000A0012C3000900114Q003900070009000200062C0107005900013Q00047B012Q00590001001259010700094Q00A800086Q003100070002000900047B012Q00570001002013000C000600062Q00A8000E000B4Q0039000C000E000200062C010C005700013Q00047B012Q00570001002013000D000C000A0012C3000F000B4Q0039000D000F00020006E7000D00560001000100047B012Q00560001002013000D000C000A0012C3000F000C4Q0039000D000F000200062C010D005700013Q00047B012Q005700012Q00A2000C00023Q0006F7000700470001000200047B012Q004700010006F7000200340001000200047B012Q003400012Q0079010200024Q00A2000200024Q00BD3Q00017Q000C3Q00030C3Q0057616974466F724368696C6403073Q0052656D6F746573026Q001440030A3Q004C69667457656967687403133Q0053652Q6C537472656E677468526571756573742Q033Q00505650030D3Q00412Q7461636B412Q74656D707403043Q0053686F70030D3Q0052657175657374427579412Q6C030F3Q0052657175657374507572636861736503043Q0050657473030B3Q005075726368617365452Q6700384Q004A016Q0020AB5Q000100122Q000200023Q00122Q000300038Q0003000200064Q003700013Q00047B012Q0037000100201300013Q0001001257010300043Q00122Q000400036Q0001000400024Q000100013Q00202Q00013Q000100122Q000300053Q00122Q000400036Q0001000400024Q000100023Q00206F2Q013Q000100122Q000300063Q00122Q000400036Q00010004000200062Q0002001B0001000100047B012Q001B00010020130002000100010012C3000400073Q0012C3000500034Q00390002000500022Q004A000200033Q0020AB00023Q000100122Q000400083Q00122Q000500036Q00020005000200062Q0002002C00013Q00047B012Q002C0001002013000300020001001257010500093Q00122Q000600036Q0003000600024Q000300043Q00202Q00030002000100122Q0005000A3Q00122Q000600036Q0003000600024Q000300053Q00201300033Q00010012C30005000B3Q0012C3000600034Q00390003000600020006DE000400360001000300047B012Q003600010020130004000300010012C30006000C3Q0012C3000700034Q00390004000700022Q004A000400064Q00BD3Q00017Q00073Q0003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004865616C7468028Q00030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400154Q004A016Q00203C014Q00010006E73Q00060001000100047B012Q000600012Q00792Q0100014Q00A2000100023Q00201300013Q00020012C3000300034Q003900010003000200062C2Q01001000013Q00047B012Q0010000100203C010200010004002644000200100001000500047B012Q001000012Q0079010200024Q00A2000200023Q00201300023Q00060012C3000400074Q000F010200044Q000501026Q00BD3Q00017Q00083Q0003093Q00436861726163746572030E3Q00436861726163746572412Q64656403043Q0057616974030C3Q0057616974466F724368696C6403083Q0048756D616E6F6964026Q00144003093Q0057616C6B53702Q6564029Q00144Q004A016Q00203C014Q00010006E73Q00080001000100047B012Q000800012Q004A016Q00203C014Q00020020135Q00032Q00B53Q0002000200201300013Q00040012C3000300053Q0012C3000400064Q003900010004000200062C2Q01001300013Q00047B012Q0013000100203C010200010007000EF1000800130001000200047B012Q0013000100203C0102000100072Q004A000200014Q00BD3Q00017Q00093Q0003043Q007461736B03043Q0077616974029A5Q99C93F03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q0067657467656E76030B3Q004175746F47656D57616C6B030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q656401163Q00128D2Q0100013Q00202Q00010001000200122Q000200036Q00010002000100202Q00013Q000400122Q000300056Q00010003000200062Q0001001500013Q00047B012Q00150001001259010200064Q00FD00020001000200203C0102000200070006E7000200150001000100047B012Q00150001001259010200064Q00FD00020001000200203C0102000200080006E7000200150001000100047B012Q0015000100203C0102000100092Q004A00026Q00BD3Q00017Q00083Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103063Q00466F6C64657203063Q00737472696E6703053Q006D6174636803043Q004E616D6503053Q005E25642B2400183Q00126D3Q00016Q00015Q00202Q0001000100024Q000100029Q00000200044Q001300010020130005000400030012C3000700044Q003900050007000200062C0105001300013Q00047B012Q00130001001259010500053Q0020AC00050005000600202Q00060004000700122Q000700086Q00050007000200062Q0005001300013Q00047B012Q001300012Q00A2000400023Q0006F73Q00060001000200047B012Q000600012Q0079017Q00A23Q00024Q00BD3Q00017Q000A3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403083Q0041697264726F707303063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D6503073Q0041697264726F7003103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727400263Q001281012Q00013Q00206Q000200122Q000200038Q0002000200064Q00080001000100047B012Q000800012Q00792Q0100014Q00A2000100023Q0012592Q0100043Q00201300023Q00052Q0076000200034Q005700013Q000300047B012Q0021000100203C0106000500060026D3000600210001000700047B012Q002100012Q004A01066Q00510006000600050006E7000600210001000100047B012Q002100010020130006000500020012C3000800084Q00390006000800020006E70006001C0001000100047B012Q001C00010020130006000500090012C30008000A4Q003900060008000200062C0106002100013Q00047B012Q002100012Q00A8000700054Q00A8000800064Q0053010700033Q0006F70001000D0001000200047B012Q000D00012Q00792Q0100014Q00A2000100024Q00BD3Q00017Q00123Q0003043Q004E616D6503083Q0047656D4D6F64656C030B3Q0042696747656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903083Q004D6573685061727403083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C6173746963030C3Q005472616E73706172656E6379028Q0003043Q004E656F6E01503Q0006E73Q00040001000100047B012Q000400012Q00802Q016Q00A2000100023Q00203C2Q013Q00010026932Q0100110001000200047B012Q0011000100203C2Q013Q00010026932Q0100110001000300047B012Q001100010012592Q0100043Q0020F200010001000500202Q00023Q000100122Q000300066Q00010003000200044Q001200012Q00BF00016Q00802Q0100013Q0006E7000100160001000100047B012Q001600012Q008001026Q00A2000200023Q00201300023Q00070012C3000400084Q003900020004000200062C0102001D00013Q00047B012Q001D000100061C0102002000013Q00047B012Q0020000100201300023Q00090012C3000400084Q003900020004000200062C0102004D00013Q00047B012Q004D000100203C01030002000A00203C01030003000B2Q004A01045Q000672000300290001000400047B012Q002900012Q008001036Q00A2000300023Q0020130003000200070012C30005000C4Q00390003000500020006E7000300310001000100047B012Q003100010020130003000200070012C3000500084Q003900030005000200203C01040002000D0012590105000E3Q00203C01050005000D00203C01050005000F00067D0104003A0001000500047B012Q003A000100203C0104000200100026930104003B0001001100047B012Q003B00012Q00BF00046Q0080010400013Q00203C01050002000D0012590106000E3Q00203C01060006000D00203C01060006001200067D010500450001000600047B012Q0045000100203C010500020010002693010500460001001100047B012Q004600012Q00BF00056Q0080010500013Q0006DE0006004C0001000300047B012Q004C000100061C0106004C0001000400047B012Q004C00012Q00A8000600054Q00A2000600024Q008001036Q00A2000300024Q00BD3Q00017Q000F3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403043Q006D61746803043Q006875676503103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03093Q004D61676E697475646500504Q004A016Q00203C014Q000100062C012Q000900013Q00047B012Q0009000100201300013Q00020012C3000300034Q00390001000300020006E70001000B0001000100047B012Q000B00012Q00792Q0100014Q00A2000100023Q00203C2Q013Q00032Q0079010200023Q001259010300043Q00203C0103000300052Q002D00046Q0096000500013Q00202Q00050005000200122Q000700066Q00050007000200062Q0005001B00013Q00047B012Q001B0001001259010600073Q00203C0106000600082Q00A8000700044Q00A8000800054Q003B0106000800012Q004A010600024Q00FD00060001000200062C0106002400013Q00047B012Q00240001001259010700073Q00203C0107000700082Q00A8000800044Q00A8000900064Q003B010700090001001259010700094Q00A8000800044Q003100070002000900047B012Q004C0001001259010C00093Q002013000D000B000A2Q0076000D000E4Q0057000C3Q000E00047B012Q004A00012Q004A011100034Q00A8001200104Q00B500110002000200062C0111004A00013Q00047B012Q004A000100201300110010000B0012C30013000C4Q003900110013000200062C0111003900013Q00047B012Q0039000100061C0111003C0001001000047B012Q003C000100201300110010000D0012C30013000C4Q003900110013000200062C0111004A00013Q00047B012Q004A00012Q004A011200044Q00510012001200110006E70012004A0001000100047B012Q004A000100203C01120001000E00203C01130011000E2Q003800120012001300203C01120012000F0006720012004A0001000300047B012Q004A00012Q00A8000300124Q00A8000200113Q0006F7000C002D0001000200047B012Q002D00010006F7000700280001000200047B012Q002800012Q00A2000200024Q00BD3Q00017Q001B3Q0003043Q006D61746803043Q006875676503093Q004D696E486569676874030E3Q0046696E6446697273744368696C6403103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q004D6573685061727403043Q004E616D6503083Q0047656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D03083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C617374696303043Q004E656F6E030E3Q0052656E646572466964656C69747903073Q0050726563697365030C3Q005472616E73706172656E6379028Q0003083Q00506F736974696F6E03013Q005903093Q004D61676E697475646500674Q004A017Q00FD3Q000100020006E73Q00060001000100047B012Q000600012Q00792Q0100014Q00A2000100024Q00792Q0100013Q001207010200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q00048Q000500023Q00202Q00050005000400122Q000700056Q00050007000200062Q0005001700013Q00047B012Q00170001001259010600063Q00203C0106000600072Q00A8000700044Q00A8000800054Q003B0106000800012Q004A010600034Q00FD00060001000200062C0106002000013Q00047B012Q00200001001259010700063Q00203C0107000700072Q00A8000800044Q00A8000900064Q003B010700090001001259010700084Q00A8000800044Q003100070002000900047B012Q00630001001259010C00083Q002013000D000B00092Q0076000D000E4Q0057000C3Q000E00047B012Q0061000100201300110010000A0012C30013000B4Q003900110013000200062C0111006100013Q00047B012Q0061000100203C01110010000C002693011100380001000D00047B012Q003800010012590111000E3Q0020AC00110011000F00202Q00120010000C00122Q001300106Q00110013000200062Q0011006100013Q00047B012Q0061000100203C011100100011001259011200123Q00203C01120012001100203C0112001200130006610011003F0001001200047B012Q003F00012Q00BF00116Q0080011100013Q00203C011200100011001259011300123Q00203C01130013001100203C01130013001400067D0112004F0001001300047B012Q004F000100203C011200100015001259011300123Q00203C01130013001500203C01130013001600067D0112004F0001001300047B012Q004F000100203C011200100017002693011200500001001800047B012Q005000012Q00BF00126Q0080011200013Q0006E7001100550001000100047B012Q0055000100062C0112006100013Q00047B012Q0061000100203C01130010001900203C01130013001A000672000300610001001300047B012Q0061000100203C01130010001900203C01143Q00192Q003800130013001400203C01130013001B000672001300610001000200047B012Q006100012Q00A8000200134Q00A8000100103Q0006F7000C00290001000200047B012Q002900010006F7000700240001000200047B012Q002400012Q00A2000100024Q00BD3Q00017Q000C3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203083Q004B4F54484172656103043Q0052696E672Q033Q0049734103083Q00426173655061727403063Q00434672616D6503053Q004D6F64656C03083Q004765745069766F74003F3Q00126E3Q00013Q00206Q000200122Q000200038Q0002000200064Q000B00013Q00047B012Q000B0001001259012Q00013Q00203C014Q00030020135Q00020012C3000200044Q00393Q000200020006DE0001001000013Q00047B012Q0010000100201300013Q00020012C3000300054Q00390001000300020006DE000200150001000100047B012Q001500010020130002000100020012C3000400064Q003900020004000200062C0102003C00013Q00047B012Q003C00010020130003000200020012C3000500074Q003900030005000200062C0103002C00013Q00047B012Q002C00010020130004000300080012C3000600094Q003900040006000200062C0104002400013Q00047B012Q0024000100203C01040003000A2Q00A2000400023Q00047B012Q002C00010020130004000300080012C30006000B4Q003900040006000200062C0104002C00013Q00047B012Q002C000100201300040003000C2Q000F010400054Q000501045Q0020130004000200080012C3000600094Q003900040006000200062C0104003400013Q00047B012Q0034000100203C01040002000A2Q00A2000400023Q00047B012Q003C00010020130004000200080012C30006000B4Q003900040006000200062C0104003C00013Q00047B012Q003C000100201300040002000C2Q000F010400054Q000501046Q0079010300034Q00A2000300024Q00BD3Q00017Q00013Q00030D3Q0050726553696D756C6174696F6E00074Q004A017Q004A2Q0100013Q00203C2Q01000100010006A601023Q000100012Q004A012Q00024Q003B012Q000200012Q00BD3Q00013Q00013Q00133Q0003093Q0043686172616374657203073Q0067657467656E7603063Q004E6F636C697003063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C696465010003153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C756500334Q004A016Q00203C014Q00010006E73Q00050001000100047B012Q000500012Q00BD3Q00013Q0012592Q0100024Q00FD00010001000200203C2Q010001000300062C2Q01001A00013Q00047B012Q001A00010012592Q0100043Q00201300023Q00052Q0076000200034Q005700013Q000300047B012Q001800010020130006000500060012C3000800074Q003900060008000200062C0106001800013Q00047B012Q0018000100203C01060005000800062C0106001800013Q00047B012Q001800010030C70005000800090006F70001000F0001000200047B012Q000F000100201300013Q000A0012C30003000B4Q003900010003000200062C2Q01003200013Q00047B012Q00320001001259010200024Q00FD00020001000200203C01020002000C00062C0102002800013Q00047B012Q00280001001259010200024Q00FD00020001000200203C01020002000E0010152Q01000D0002001259010200024Q00FD00020001000200203C01020002000F00062C0102003200013Q00047B012Q003200010030C7000100100011001259010200024Q00FD00020001000200203C0102000200130010152Q01001200022Q00BD3Q00017Q00093Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030B3Q004368616E6765537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503073Q004A756D70696E6700143Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001300013Q00047B012Q001300012Q004A016Q00203C014Q00030006DE0001000C00013Q00047B012Q000C000100201300013Q00040012C3000300054Q003900010003000200062C2Q01001300013Q00047B012Q00130001002013000200010006001259010400073Q00203C01040004000800203C0104000400092Q003B0102000400012Q00BD3Q00017Q00093Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E563203043Q007461736B03043Q0077616974027Q004003083Q0054656C65706F727403043Q0067616D6503073Q00506C616365496400153Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001400013Q00047B012Q00140001001259012Q00014Q00FD3Q0001000200203C014Q00030006E73Q00140001000100047B012Q00140001001259012Q00043Q00203C014Q00050012C3000100064Q00A5012Q000200012Q004A016Q0020135Q0007001259010200083Q00203C0102000200092Q004A010300014Q003B012Q000300012Q00BD3Q00017Q001B3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C6964650100030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00476574537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503083Q0046722Q6566612Q6C03163Q00412Q73656D626C794C696E65617256656C6F6369747903013Q0059026Q00344003073Q00566563746F72332Q033Q006E657703013Q0058026Q0049C003013Q005A026Q004EC0026Q0034C000453Q001259012Q00014Q00FD3Q0001000200203C014Q00020006E73Q00060001000100047B012Q000600012Q00BD3Q00014Q004A016Q00203C014Q00030006E73Q000B0001000100047B012Q000B00012Q00BD3Q00013Q0012592Q0100043Q00201300023Q00052Q0076000200034Q005700013Q000300047B012Q001600010020130006000500060012C3000800074Q003900060008000200062C0106001600013Q00047B012Q001600010030C70005000800090006F7000100100001000200047B012Q0010000100201300013Q000A0012E60003000B6Q00010003000200202Q00023Q000C00122Q0004000D6Q00020004000200062Q0001004400013Q00047B012Q0044000100062C0102004400013Q00047B012Q0044000100201300030002000E2Q001000030002000200122Q0004000F3Q00202Q00040004001000202Q00040004001100062Q0003002D0001000400047B012Q002D000100203C01030001001200203C010300030013000EF1001400370001000300047B012Q00370001001259010300153Q00202401030003001600202Q00040001001200202Q00040004001700122Q000500183Q00202Q00060001001200202Q0006000600194Q00030006000200102Q00010012000300044Q0044000100203C01030001001200203C01030003001300265C010300440001001A00047B012Q00440001001259010300153Q00203C01030003001600203C01040001001200203C0104000400170012C30005001B3Q00203C01060001001200203C0106000600192Q00390003000600020010152Q01001200032Q00BD3Q00017Q000B3Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030A3Q0053702Q656456616C756500224Q004A016Q00203C014Q000100062C012Q002100013Q00047B012Q00210001001259012Q00023Q0020CC5Q000300122Q000100048Q0002000100124Q00058Q0001000200206Q000600066Q00013Q00047B014Q00012Q004A012Q00013Q00203C014Q00070006DE0001001400013Q00047B012Q0014000100201300013Q00080012C3000300094Q003900010003000200062C2Q013Q00013Q00047B014Q000100203C01020001000A001259010300054Q00FD00030001000200203C01030003000B00066100023Q0001000300047B014Q0001001259010200054Q00FD00020001000200203C01020002000B0010152Q01000A000200047B014Q00012Q00BD3Q00017Q00293Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F7003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00506F736974696F6E03073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00184003023Q006F7303053Q00636C6F636B026Q0008402Q0103043Q007461736B03053Q0064656C6179026Q004E40026Q00E03F03043Q00556E697403043Q004C65727003043Q006D61746803053Q00636C616D70026Q002440026Q00F03F03043Q004D6F7665026Q000C4003063Q00434672616D6503063Q006C2Q6F6B417403013Q0059026Q002040026Q00104003113Q0066697265746F756368696E74657265737403043Q007A65726F01A93Q0012592Q0100014Q00FD00010001000200203C2Q01000100020006E7000100060001000100047B012Q000600012Q00BD3Q00013Q0012592Q0100014Q00FD00010001000200203C2Q01000100030006E7000100150001000100047B012Q001500010012592Q0100014Q00FD00010001000200203C2Q01000100040006E7000100150001000100047B012Q001500010012592Q0100014Q00FD00010001000200203C2Q010001000500062C2Q01001C00013Q00047B012Q001C00012Q004A2Q016Q003500010001000200062C2Q01001C00013Q00047B012Q001C000100062C0102001C00013Q00047B012Q001C00012Q00BD3Q00014Q004A2Q0100013Q00203C2Q01000100060006E7000100210001000100047B012Q002100012Q00BD3Q00013Q0020130002000100070012E6000400086Q00020004000200202Q00030001000900122Q0005000A6Q00030005000200062Q000200A800013Q00047B012Q00A8000100062C010300A800013Q00047B012Q00A800012Q004A010400024Q00FD00040001000200062C0104009700013Q00047B012Q0097000100203C01050004000B00202E01060002000B4Q00050005000600122Q0006000C3Q00202Q00060006000D00202Q00070005000E00122Q0008000F3Q00202Q0009000500104Q00060009000200202Q00070006001100262Q000700590001001200047B012Q005900012Q004A010800033Q000661000800450001000400047B012Q004500012Q00A8000800043Q0012CD000900133Q00202Q0009000900144Q0009000100024Q000900046Q000800033Q00044Q005B0001001259010800133Q00203C0108000800142Q00FD0008000100022Q004A010900044Q0038000800080009000EF10015005B0001000800047B012Q005B00012Q004A010800053Q002044010800040016001259010800173Q00203C0108000800180012C3000900193Q0006A6010A3Q000100022Q004A012Q00054Q00A83Q00044Q00370108000A00014Q000800086Q000800038Q00013Q00044Q005B00012Q0079010800084Q004A000800033Q000EF1001A00870001000700047B012Q0087000100203C01080006001B2Q004A010900063Q00201300090009001C2Q00A8000B00083Q001259010C001D3Q00203C010C000C001E0020A3010D3Q001F0012C3000E000F3Q0012C3000F00204Q0099010C000F4Q003200093Q00022Q004A000900063Q0020300109000300214Q000B00066Q000C8Q0009000C0001000E2Q002200870001000700047B012Q00870001001259010900233Q00207C00090009002400202Q000A0002000B00122Q000B000C3Q00202Q000B000B000D00202Q000C0004000B00202Q000C000C000E00202Q000D0002000B00202Q000D000D002500202Q000E0004000B00202Q000E000E00104Q000B000E6Q00093Q000200202Q000A0002002300202Q000A000A001C4Q000C00093Q00122Q000D001D3Q00202Q000D000D001E00202Q000E3Q002600122Q000F000F3Q00122Q001000206Q000D00106Q000A3Q000200102Q00020023000A002644000700A70001002700047B012Q00A70001001259010800283Q00062C010800A700013Q00047B012Q00A70001001259010800284Q0077010900026Q000A00043Q00122Q000B000F6Q0008000B000100122Q000800286Q000900026Q000A00043Q00122Q000B00206Q0008000B000100044Q00A700012Q004A010500063Q00201300050005001C0012590107000C3Q00203C0107000700290012590108001D3Q00203C01080008001E0020A301093Q00260012C3000A000F3Q0012C3000B00204Q00990108000B4Q003200053Q00022Q004A000500063Q0020130005000300212Q004A010700064Q008001086Q003B0105000800012Q00AA01046Q00BD3Q00013Q00013Q00015Q00044Q004A017Q004A2Q0100013Q002044012Q000100012Q00BD3Q00017Q00153Q0003083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03053Q00416C697665026Q00444003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00104003063Q004D6F7665546F03043Q007461736B03043Q0077616974029A5Q99B93F03043Q004D6F766503043Q007A65726F024D3Q00203C01023Q0001001259010300023Q00203C0103000300032Q00FD0003000100022Q00A8000400014Q00FD00040001000200062C0104003A00013Q00047B012Q003A00012Q004A01045Q00203C01040004000400062C0104003A00013Q00047B012Q003A0001001259010400023Q00203C0104000400032Q00FD0004000100022Q003800040004000300265C0104003A0001000500047B012Q003A00012Q004A010400014Q00FD0004000100022Q004A010500023Q00203C01050005000600062C0105001D00013Q00047B012Q001D00012Q004A010500023Q00203C0105000500060020130005000500070012C3000700084Q003900050007000200062C0104003A00013Q00047B012Q003A00010006E7000500220001000100047B012Q0022000100047B012Q003A0001001259010600093Q00202B00060006000A00202Q00070002000B00202Q00080004000100202Q00080008000B4Q00070007000800122Q0008000C3Q00202Q00090002000D00202Q000A0004000100202Q000A000A000D4Q00090009000A4Q00060009000200202Q00060006000E00262Q000600320001000F00047B012Q0032000100047B012Q003A00010020130007000500102Q00A8000900024Q003B010700090001001207000700113Q00202Q00070007001200122Q000800136Q00070002000100044Q000400012Q004A010400023Q00203C01040004000600062C0104004300013Q00047B012Q004300012Q004A010400023Q00203C0104000400060020130004000400070012C3000600084Q003900040006000200062C0104004900013Q00047B012Q00490001002013000500040014001259010700093Q00203C0107000700152Q003B0105000700012Q00A8000500014Q008E000500014Q000501056Q00BD3Q00017Q000C3Q0003063Q0069706169727303073Q0067657467656E76030E3Q0046696E6446697273744368696C642Q033Q0049734103083Q00426173655061727403063Q00434672616D6503083Q004765745069766F74028Q0003043Q007461736B03043Q0077616974029A5Q99B93F010003363Q001259010300014Q00A8000400014Q003100030002000500047B012Q00300001001259010800024Q00FD0008000100022Q00510008000800020006E70008000A0001000100047B012Q000A000100047B012Q0032000100201300083Q00032Q00A8000A00074Q00390008000A000200062C0108003000013Q00047B012Q003000010020130009000800040012C3000B00054Q00390009000B000200062C0109001700013Q00047B012Q0017000100203C0109000800060006E7000900190001000100047B012Q001900010020130009000800072Q00B50009000200022Q004A010A6Q00A8000B00093Q0006A6010C3Q000100012Q00A83Q00024Q0039000A000C00020006E7000A00210001000100047B012Q0021000100047B012Q003200010012C3000A00084Q004A010B00013Q000672000A00300001000B00047B012Q00300001001259010B00024Q00FD000B000100022Q0051000B000B000200062C010B003000013Q00047B012Q00300001001259010B00093Q0020C0000B000B000A00122Q000C000B6Q000B0002000100202Q000A000A000B00044Q002200010006F7000300040001000200047B012Q00040001001259010300024Q00FD00030001000200204401030002000C2Q00BD3Q00013Q00013Q00013Q0003073Q0067657467656E7600063Q00126E012Q00018Q000100024Q00019Q003Q00016Q00028Q00017Q00043Q0003073Q0067657467656E7603083Q004175746F4C69667403043Q007461736B03053Q00737061776E010D3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000C00013Q00047B012Q000C00010012592Q0100033Q00203C2Q01000100040006A601023Q000100032Q004A017Q004A012Q00014Q004A012Q00024Q00A52Q01000200012Q00BD3Q00013Q00013Q000F3Q0003053Q007063612Q6C03073Q0067657467656E7603083Q004175746F4C69667403093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03163Q0046696E6446697273744368696C64576869636849734103083Q0048756D616E6F696403093Q004571756970542Q6F6C030A3Q004669726553657276657203043Q007461736B03043Q0077616974029A5Q99B93F00333Q001259012Q00013Q0006A62Q013Q000100012Q004A017Q00A5012Q00020001001259012Q00024Q00FD3Q0001000200203C014Q000300062C012Q003200013Q00047B012Q003200012Q004A012Q00013Q00203C014Q00042Q0096000100013Q00202Q00010001000500122Q000300066Q00010003000200064Q002100013Q00047B012Q0021000100062C2Q01002100013Q00047B012Q0021000100201300023Q00070012C3000400084Q00390002000400020006E7000200210001000100047B012Q002100010020130003000100090012C3000500084Q003900030005000200062C0103002100013Q00047B012Q0021000100203C01043Q000A00201300040004000B2Q00A8000600034Q003B0104000600012Q004A010200023Q00062C0102002800013Q00047B012Q002800012Q004A010200023Q00201300020002000C2Q00A501020002000100047B012Q002C0001001259010200013Q0006A601030001000100012Q00A88Q00A50102000200010012590102000D3Q00201101020002000E00122Q0003000F6Q0002000200019Q0000044Q000400012Q00BD3Q00013Q00023Q00083Q00030C3Q0053656E644B65794576656E7403043Q00456E756D03073Q004B6579436F64652Q033Q004F6E6503043Q0067616D6503043Q007461736B03043Q0077616974029A5Q99A93F00174Q0087016Q00206Q00014Q000200013Q00122Q000300023Q00202Q00030003000300202Q0003000300044Q00045Q00122Q000500058Q0005000100124Q00063Q00206Q000700122Q000100088Q000200019Q0000206Q00014Q00025Q00122Q000300023Q00202Q00030003000300202Q0003000300044Q00045Q00122Q000500058Q000500016Q00017Q00033Q0003153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03083Q004163746976617465000C4Q004A016Q00062C012Q000700013Q00047B012Q000700012Q004A016Q0020135Q00010012C3000200024Q00393Q0002000200062C012Q000B00013Q00047B012Q000B000100201300013Q00032Q00A52Q01000200012Q00BD3Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F50756E636803043Q007461736B03053Q00737061776E010B3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000A00013Q00047B012Q000A00010012592Q0100033Q00203C2Q01000100040006A601023Q000100012Q004A017Q00A52Q01000200012Q00BD3Q00013Q00013Q00083Q0003073Q0067657467656E7603093Q004175746F50756E6368030A3Q004669726553657276657203053Q0050756E6368026Q00F03F03043Q007461736B03043Q0077616974029A5Q99A93F00133Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001200013Q00047B012Q001200012Q004A016Q00062C012Q000D00013Q00047B012Q000D00012Q004A016Q0020135Q00030012C3000200043Q0012C3000300054Q003B012Q00030001001259012Q00063Q00203C014Q00070012C3000100084Q00A5012Q0002000100047B014Q00012Q00BD3Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F53746F6D7003043Q007461736B03053Q00737061776E010B3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000A00013Q00047B012Q000A00010012592Q0100033Q00203C2Q01000100040006A601023Q000100012Q004A017Q00A52Q01000200012Q00BD3Q00013Q00013Q00073Q0003073Q0067657467656E7603093Q004175746F53746F6D70030A3Q004669726553657276657203053Q0053746F6D7003043Q007461736B03043Q0077616974029A5Q99A93F00123Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001100013Q00047B012Q001100012Q004A016Q00062C012Q000C00013Q00047B012Q000C00012Q004A016Q0020135Q00030012C3000200044Q003B012Q00020001001259012Q00053Q00203C014Q00060012C3000100074Q00A5012Q0002000100047B014Q00012Q00BD3Q00017Q00083Q0003073Q0067657467656E76030B3Q004175746F41697264726F70030F3Q004175746F54652Q7269746F72696573010003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01153Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q001400013Q00047B012Q001400010012592Q0100014Q005200010001000200302Q00010003000400122Q000100053Q00202Q0001000100064Q00028Q00010002000100122Q000100073Q00202Q0001000100080006A601023Q000100042Q004A012Q00014Q004A012Q00024Q004A017Q004A012Q00034Q00A52Q01000200012Q00BD3Q00013Q00013Q000D3Q0003073Q0067657467656E76030B3Q004175746F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F030C3Q004175746F47656D54772Q656E03063Q00434672616D652Q033Q006E6577028Q00026Q000840026Q002E402Q01029A5Q99C93F003D3Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q003C00013Q00047B012Q003C0001001259012Q00033Q00203C014Q00040012C3000100054Q00A5012Q000200012Q004A017Q00FD3Q000100022Q004A2Q0100014Q003500010001000200062C2Q013Q00013Q00047B014Q000100062C01023Q00013Q00047B014Q000100062C014Q00013Q00047B014Q0001001259010300014Q00FD00030001000200203C0103000300060006E700033Q0001000100047B014Q000100203C010300020007001294000400073Q00202Q00040004000800122Q000500093Q00122Q0006000A3Q00122Q000700096Q0004000700024Q00030003000400104Q0007000300122Q000300033Q00202Q00030003000400122Q0004000B6Q0003000200014Q000300023Q00202Q00030001000C4Q000300036Q0003000100024Q00048Q00040001000200062Q00033Q00013Q00047B014Q000100062C01043Q00013Q00047B014Q0001001259010500073Q00205E01050005000800122Q000600093Q00122Q0007000A3Q00122Q000800096Q0005000800024Q00050003000500102Q00040007000500122Q000500033Q00202Q00050005000400122Q0006000D6Q00050002000100046Q00012Q00BD3Q00017Q00083Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F72696573030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E67030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01143Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q001300013Q00047B012Q001300010012592Q0100014Q00AD00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q0001000100080006A601023Q000100012Q004A017Q00A52Q01000200012Q00BD3Q00013Q00013Q000A3Q0003023Q00543103023Q00543203023Q00543303023Q00543403023Q00543503093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0054652Q7269746F72696573030F3Q004175746F54652Q7269746F72696573001A4Q0098012Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500010012592Q0100063Q0020130001000100070012C3000300084Q003900010003000200062C2Q01001200013Q00047B012Q001200010012592Q0100063Q00203C2Q01000100080020130001000100070012C3000300094Q003900010003000200062C2Q01001900013Q00047B012Q001900012Q004A01026Q00A8000300014Q00A800045Q0012C30005000A4Q003B0102000500012Q00BD3Q00017Q00063Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01123Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q001100013Q00047B012Q001100010012592Q0100033Q0020890001000100044Q00028Q00010002000100122Q000100053Q00202Q0001000100060006A601023Q000100042Q004A012Q00014Q004A012Q00024Q004A012Q00034Q004A017Q00A52Q01000200012Q00BD3Q00013Q00013Q00133Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00506172656E7403083Q00506F736974696F6E03093Q004D61676E6974756465026Q00144003063Q004D6F7665546F029A5Q99C93F03043Q004D6F766503073Q00566563746F723303043Q007A65726F026Q002E402Q01004C3Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q004B00013Q00047B012Q004B0001001259012Q00033Q0020845Q000400122Q000100058Q000200019Q0000206Q00064Q000100016Q00010001000200062Q0002001200013Q00047B012Q0012000100201300023Q00070012C3000400084Q00390002000400022Q004A010300024Q003500030001000400062C01033Q00013Q00047B014Q000100062C01043Q00013Q00047B014Q000100062C2Q013Q00013Q00047B014Q000100062C01023Q00013Q00047B014Q0001001259010500014Q00FD00050001000200203C01050005000200062C0105003200013Q00047B012Q0032000100203C01050003000900062C0105003200013Q00047B012Q0032000100203C01050001000A00203C01060004000A2Q003800050005000600203C01050005000B000EF1000C00320001000500047B012Q0032000100201300050002000D00203C01070004000A2Q003B010500070001001207000500033Q00202Q00050005000400122Q0006000E6Q00050002000100044Q001C0001001259010500014Q00FD00050001000200203C01050005000200062C01053Q00013Q00047B014Q000100203C01050003000900062C01053Q00013Q00047B014Q000100203C01050001000A00203C01060004000A2Q003800050005000600203C01050005000B00264400053Q0001000C00047B014Q000100201300050002000F001259010700103Q00203C0107000700112Q003B010500070001001259010500033Q00203C0105000500040012C3000600124Q00A50105000200012Q004A010500033Q00204401050003001300047B014Q00012Q00BD3Q00017Q000D3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E6703093Q0057616C6B53702Q6564030A3Q0053702Q656456616C756503043Q004D6F766503073Q00566563746F723303043Q007A65726F01223Q0012922Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A0001000100047B012Q000A00010020130002000100040012C3000400054Q003900020004000200062C012Q001900013Q00047B012Q00190001001259010300014Q008700030001000200302Q00030006000700122Q000300016Q00030001000200302Q00030008000700062Q0002002100013Q00047B012Q00210001001259010300014Q00FD00030001000200203C01030003000A00101501020009000300047B012Q0021000100062C0102002100013Q00047B012Q0021000100201300030002000B0012020105000C3Q00202Q00050005000D4Q0003000500014Q000300013Q00102Q0002000900032Q00BD3Q00017Q00073Q0003073Q0067657467656E76030A3Q0053702Q656456616C7565030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q0012342Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q00047B012Q001200012Q004A2Q015Q00203C2Q01000100040006DE0002000F0001000100047B012Q000F00010020130002000100050012C3000400064Q003900020004000200062C0102001200013Q00047B012Q00120001001015010200074Q00BD3Q00017Q00023Q0003073Q0067657467656E76030D3Q0047656D54772Q656E53702Q656401043Q0012592Q0100014Q00FD0001000100020010152Q0100024Q00BD3Q00017Q00093Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03063Q004E6F636C6970030C3Q004175746F47656D4272696E670100030B3Q004175746F47656D57616C6B030F3Q004175746F54652Q7269746F7269657303043Q007461736B03053Q00737061776E011C3Q001218000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200102Q000100033Q00064Q001B00013Q00047B012Q001B00010012592Q0100014Q00AD00010001000200302Q00010004000500122Q000100016Q00010001000200302Q00010006000500122Q000100016Q00010001000200302Q00010007000500122Q000100083Q00202Q0001000100090006A601023Q000100062Q004A017Q004A012Q00014Q004A012Q00024Q004A012Q00034Q004A012Q00044Q004A012Q00054Q00A52Q01000200012Q00BD3Q00013Q00013Q00153Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03093Q0048656172746265617403043Q0057616974030B3Q004175746F41697264726F7003063Q00434672616D652Q033Q006E6577028Q00026Q00084003163Q00412Q73656D626C794C696E65617256656C6F6369747903073Q00566563746F723303173Q00412Q73656D626C79416E67756C617256656C6F6369747903043Q007461736B03043Q0077616974026Q002E402Q01029A5Q99C93F03083Q00506F736974696F6E030D3Q0047656D54772Q656E53702Q656403093Q004D61676E697475646503043Q00556E697400703Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q006F00013Q00047B012Q006F00012Q004A016Q00203C014Q00030020135Q00042Q00B53Q000200022Q004A2Q0100014Q00FD00010001000200062C2Q013Q00013Q00047B014Q00012Q004A010200024Q00EB00020001000300122Q000400016Q00040001000200202Q00040004000500062Q0004004A00013Q00047B012Q004A000100062C0102004A00013Q00047B012Q004A000100062C0103004A00013Q00047B012Q004A000100203C010400030006001224000500063Q00202Q00050005000700122Q000600083Q00122Q000700093Q00122Q000800086Q0005000800024Q00040004000500102Q00010006000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000A000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000C000400122Q0004000D3Q00202Q00040004000E00122Q0005000F6Q0004000200014Q000400033Q00202Q0004000200104Q000400046Q0004000100024Q000500016Q00050001000200062Q00043Q00013Q00047B014Q000100062C01053Q00013Q00047B014Q0001001259010600063Q00205E01060006000700122Q000700083Q00122Q000800093Q00122Q000900086Q0006000900024Q00060004000600102Q00050006000600122Q0006000D3Q00202Q00060006000E00122Q000700116Q00060002000100046Q00012Q004A010400054Q00FD00040001000200062C01043Q00013Q00047B014Q000100203C0105000400120020B70006000100124Q00050005000600122Q000600016Q00060001000200202Q0006000600134Q000600063Q00202Q00070005001400062Q0007005B0001000600047B012Q005B000100203C0107000400060010152Q010006000700047B012Q0060000100203C01070001000600203C0108000500152Q00CF0008000800062Q00630007000700080010152Q01000600070012590107000B3Q00203C01070007000700121A000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000A000700122Q0007000B3Q00202Q00070007000700122Q000800083Q00122Q000900083Q00122Q000A00084Q00390007000A00020010152Q01000C000700047B014Q00012Q00BD3Q00017Q000E3Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03053Q00737061776E03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F726564010001273Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100012Q004A017Q00A52Q010002000100047B012Q002600010012592Q0100053Q0020130001000100060012C3000300074Q003900010003000200062C2Q01002600013Q00047B012Q002600010012592Q0100083Q001235010200053Q00202Q00020002000700202Q0002000200094Q000200036Q00013Q000300044Q0024000100201300060005000A0012C30008000B4Q003900060008000200062C0106002400013Q00047B012Q002400010020130006000500060012C30008000C4Q003900060008000200062C0106002400013Q00047B012Q0024000100203C01060005000C0030C70006000D000E0006F7000100180001000200047B012Q001800012Q00BD3Q00013Q00013Q00143Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403063Q00434672616D652Q033Q006E6577028Q00026Q001AC0026Q001EC003083Q00416E63686F7265642Q0100343Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q003300013Q00047B012Q00330001001259012Q00033Q00203C014Q00040012C3000100054Q00A5012Q000200012Q004A017Q00FD3Q0001000200062C014Q00013Q00047B014Q00010012592Q0100063Q0020130001000100070012C3000300084Q003900010003000200062C2Q013Q00013Q00047B014Q00010012592Q0100093Q001235010200063Q00202Q00020002000800202Q00020002000A4Q000200036Q00013Q000300044Q0030000100201300060005000B0012C30008000C4Q003900060008000200062C0106003000013Q00047B012Q003000010020130006000500070012C30008000D4Q003900060008000200062C0106003000013Q00047B012Q0030000100203C01060005000D00206601073Q000E00122Q0008000E3Q00202Q00080008000F00122Q000900103Q00122Q000A00113Q00122Q000B00126Q0008000B00024Q00070007000800102Q0006000E000700202Q00060005000D0030C70006001300140006F70001001A0001000200047B012Q001A000100047B014Q00012Q00BD3Q00017Q00043Q0003073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303043Q007461736B03053Q00737061776E010C3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100022Q004A017Q004A012Q00014Q00A52Q01000200012Q00BD3Q00013Q00013Q001B3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q0052696768744C6F7765724C656703103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E657703083Q00506F736974696F6E03073Q00566563746F7233026Q002E40027Q0040028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004D6F7665546F00723Q001281012Q00013Q00206Q000200122Q000200038Q0002000200064Q00070001000100047B012Q000700012Q00BD3Q00014Q00792Q0100013Q00125E000200043Q00202Q00033Q00054Q000300046Q00023Q000400044Q002300010020130007000600060012C3000900074Q003900070009000200062C0107002300013Q00047B012Q002300010020130007000600020012C3000900084Q003900070009000200061C2Q0100200001000700047B012Q002000010020130007000600020012C3000900094Q003900070009000200061C2Q0100200001000700047B012Q0020000100201300070006000A0012C30009000B4Q00390007000900022Q00A8000100073Q00062C2Q01002300013Q00047B012Q0023000100047B012Q002500010006F70002000D0001000200047B012Q000D00012Q004A01026Q00FD00020001000200062C0102003B00013Q00047B012Q003B000100062C2Q01003B00013Q00047B012Q003B00010012590103000C3Q00206301030003000D00202Q00040001000E00122Q0005000F3Q00202Q00050005000D00122Q000600103Q00122Q000700113Q00122Q000800126Q0005000800024Q0004000400054Q0003000200020010150102000C0003001259010300133Q00203C0103000300140012C3000400154Q00A5010300020001001259010300164Q00FD00030001000200203C01030003001700062C0103007100013Q00047B012Q00710001001259010300133Q0020E900030003001400122Q000400156Q0003000200014Q000300013Q00202Q00030003001800062Q0004004B0001000300047B012Q004B00010020130004000300190012C30006001A4Q00390004000600022Q0079010500053Q00125E000600043Q00202Q00073Q00054Q000700086Q00063Q000800044Q00670001002013000B000A00060012C3000D00074Q0039000B000D000200062C010B006700013Q00047B012Q00670001002013000B000A00020012C3000D00084Q0039000B000D000200061C010500640001000B00047B012Q00640001002013000B000A00020012C3000D00094Q0039000B000D000200061C010500640001000B00047B012Q00640001002013000B000A000A0012C3000D000B4Q0039000B000D00022Q00A80005000B3Q00062C0105006700013Q00047B012Q0067000100047B012Q006900010006F7000600510001000200047B012Q0051000100062C0104003B00013Q00047B012Q003B000100062C0105003B00013Q00047B012Q003B000100201300060004001B00203C01080005000E2Q003B01060008000100047B012Q003B00012Q00BD3Q00017Q00063Q0003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C030A3Q0057616C6B546F426F2Q73010003043Q007461736B03053Q00737061776E010E3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000D00013Q00047B012Q000D00010012592Q0100014Q00FD0001000100020030C70001000300040012592Q0100053Q00203C2Q01000100060006A601023Q000100012Q004A017Q00A52Q01000200012Q00BD3Q00013Q00013Q00153Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03043Q007461736B03043Q0077616974027B14AE47E17A843F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E6577028Q00026Q000C4003083Q0056656C6F6369747903073Q00566563746F723300413Q001281012Q00013Q00206Q000200122Q000200038Q0002000200064Q00070001000100047B012Q000700012Q00BD3Q00013Q0012592Q0100044Q00FD00010001000200203C2Q010001000500062C2Q01004000013Q00047B012Q004000010012592Q0100063Q00209E2Q010001000700122Q000200086Q0001000200014Q00018Q0001000100024Q000200023Q00125E000300093Q00202Q00043Q000A4Q000400056Q00033Q000500044Q0029000100201300080007000B0012C3000A000C4Q00390008000A000200062C0108002900013Q00047B012Q002900010020130008000700020012C3000A000D4Q00390008000A000200061C010200260001000800047B012Q0026000100201300080007000E0012C3000A000F4Q00390008000A00022Q00A8000200083Q00062C0102002900013Q00047B012Q0029000100047B012Q002B00010006F7000300180001000200047B012Q0018000100062C2Q01000700013Q00047B012Q0007000100062C0102000700013Q00047B012Q0007000100203C0103000200100012A0000400103Q00202Q00040004001100122Q000500123Q00122Q000600123Q00122Q000700136Q0004000700024Q00030003000400102Q00010010000300122Q000300153Q00202Q00030003001100122Q000400123Q00122Q000500123Q00122Q000600126Q00030006000200102Q00010014000300044Q000700012Q00BD3Q00017Q00023Q0003073Q0067657467656E7603103Q0053656C6563746564452Q67496E64657801043Q0012592Q0100014Q00FD0001000100020010152Q0100024Q00BD3Q00017Q00043Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q6703043Q007461736B03053Q00737061776E010D3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000C00013Q00047B012Q000C00010012592Q0100033Q00203C2Q01000100040006A601023Q000100032Q004A017Q004A012Q00014Q004A012Q00024Q00A52Q01000200012Q00BD3Q00013Q00013Q000B3Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q003000013Q00047B012Q003000012Q004A016Q0006E73Q001B0001000100047B012Q001B00012Q004A012Q00013Q0020135Q00030012C3000200044Q00393Q0002000200062C012Q001B00013Q00047B012Q001B00012Q004A012Q00013Q002021014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q00047B012Q001B00012Q004A012Q00013Q00203D5Q000400206Q000500206Q000300122Q000200068Q0002000200062C012Q002A00013Q00047B012Q002A00010012592Q0100014Q00FD00010001000200203C2Q01000100070006E7000100230001000100047B012Q002300010012C3000100083Q001259010200093Q00203C01020002000A0006A601033Q000100022Q00A88Q00A83Q00014Q00A50102000200012Q00AA2Q015Q0012592Q0100093Q00203C2Q010001000B2Q004A010200024Q00A52Q01000200012Q00AA016Q00047B014Q00012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q001259012Q00013Q0006A62Q013Q000100022Q004A017Q004A012Q00014Q00A5012Q000200012Q00BD3Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00084003073Q0049736C616E647300074Q0076016Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q6703043Q007461736B03053Q00737061776E010C3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100022Q004A017Q004A012Q00014Q00A52Q01000200012Q00BD3Q00013Q00013Q00093Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703043Q007461736B03053Q00737061776E03043Q007761697400283Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q002700013Q00047B012Q002700012Q004A016Q0006E73Q001B0001000100047B012Q001B00012Q004A012Q00013Q0020135Q00030012C3000200044Q00393Q0002000200062C012Q001B00013Q00047B012Q001B00012Q004A012Q00013Q002021014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q00047B012Q001B00012Q004A012Q00013Q00203D5Q000400206Q000500206Q000300122Q000200068Q0002000200062C012Q002200013Q00047B012Q002200010012592Q0100073Q00203C2Q01000100080006A601023Q000100012Q00A88Q00A52Q01000200010012592Q0100073Q00203C2Q01000100092Q00F80001000100012Q00AA016Q00047B014Q00012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q001259012Q00013Q0006A62Q013Q000100012Q004A017Q00A5012Q000200012Q00BD3Q00013Q00013Q00043Q00030C3Q00496E766F6B65536572766572026Q00F03F026Q00084003093Q0043756265576F726C6400074Q0049016Q00206Q000100122Q000200023Q00122Q000300033Q00122Q000400048Q000400016Q00017Q00163Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001503Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q004A00013Q00047B012Q004A00010012592Q0100033Q0020130001000100040012C3000300054Q003900010003000200062C2Q01002100013Q00047B012Q002100010012592Q0100033Q0020212Q010001000500202Q00010001000400122Q000300066Q00010003000200062Q0001002100013Q00047B012Q002100010012592Q0100033Q00200C2Q010001000500202Q00010001000600202Q00010001000400122Q000300076Q00010003000200062Q0001002100013Q00047B012Q002100010012592Q0100033Q00203C2Q010001000500203D00010001000600202Q00010001000700202Q00010001000400122Q000300086Q0001000300022Q004A01026Q00FD00020001000200062C0102003F00013Q00047B012Q003F000100062C2Q01003F00013Q00047B012Q003F00010020130003000100090012C30005000A4Q003900030005000200062C0103003000013Q00047B012Q0030000100201300030001000B2Q00B50003000200020006E7000300310001000100047B012Q0031000100203C01030001000C0012590104000C3Q00205C00040004000D00122Q0005000E3Q00122Q0006000F3Q00122Q0007000E6Q0004000700024Q00040003000400102Q0002000C000400122Q000400103Q00202Q00040004001100122Q000500126Q00040002000100302Q00020013001400044Q0042000100062C0102004200013Q00047B012Q004200010030C7000200130014001259010300103Q00203C0103000300150006A601043Q000100032Q004A012Q00014Q004A012Q00024Q004A012Q00034Q00A501030002000100047B012Q004F00012Q004A2Q016Q00FD00010001000200062C2Q01004F00013Q00047B012Q004F00010030C70001001300162Q00BD3Q00013Q00013Q00083Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001F00013Q00047B012Q001F00012Q004A016Q002058014Q000300206Q00046Q000200016Q00013Q00064Q00170001000100047B012Q001700012Q004A012Q00023Q0020135Q00050012C3000200064Q00393Q0002000200062C012Q001700013Q00047B012Q001700012Q004A012Q00023Q00203C014Q00060020135Q00050012C3000200074Q00393Q0002000200062C012Q001D00013Q00047B012Q001D00010012592Q0100083Q0006A601023Q000100012Q00A88Q00A52Q01000200012Q00AA016Q00047B014Q00012Q00BD3Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q004A016Q0020135Q00012Q00A5012Q000200012Q00BD3Q00017Q00043Q0003073Q0067657467656E76030E3Q004175746F4275795765696768747303043Q007461736B03053Q00737061776E010C3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100022Q004A017Q004A012Q00014Q00A52Q01000200012Q00BD3Q00013Q00013Q000A3Q0003073Q0067657467656E76030E3Q004175746F42757957656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q002800013Q00047B012Q002800012Q004A016Q0006E73Q001B0001000100047B012Q001B00012Q004A012Q00013Q0020135Q00030012C3000200044Q00393Q0002000200062C012Q001B00013Q00047B012Q001B00012Q004A012Q00013Q002021014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q00047B012Q001B00012Q004A012Q00013Q00203D5Q000400206Q000500206Q000300122Q000200068Q0002000200062C012Q002200013Q00047B012Q002200010012592Q0100073Q00203C2Q01000100080006A601023Q000100012Q00A88Q00A52Q01000200010012592Q0100073Q0020112Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q001259012Q00013Q0006A62Q013Q000100012Q004A017Q00A5012Q000200012Q00BD3Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q0057656967687403073Q0049736C616E647300064Q00567Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q00043Q0003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E010C3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100022Q004A017Q004A012Q00014Q00A52Q01000200012Q00BD3Q00013Q00013Q000A3Q0003073Q0067657467656E76030A3Q004175746F427579444E41030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q002800013Q00047B012Q002800012Q004A016Q0006E73Q001B0001000100047B012Q001B00012Q004A012Q00013Q0020135Q00030012C3000200044Q00393Q0002000200062C012Q001B00013Q00047B012Q001B00012Q004A012Q00013Q002021014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q00047B012Q001B00012Q004A012Q00013Q00203D5Q000400206Q000500206Q000300122Q000200068Q0002000200062C012Q002200013Q00047B012Q002200010012592Q0100073Q00203C2Q01000100080006A601023Q000100012Q00A88Q00A52Q01000200010012592Q0100073Q0020112Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00BD3Q00013Q00013Q00063Q00026Q00F03F026Q005E4003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E00133Q0012C33Q00013Q0012C3000100023Q0012C3000200013Q0004A0012Q00120001001259010400034Q00FD00040001000200203C0104000400040006E70004000A0001000100047B012Q000A000100047B012Q00120001001259010400053Q00203C0104000400060006A601053Q000100022Q004A017Q00A83Q00034Q00A50104000200012Q00AA01035Q000478012Q000400012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q001259012Q00013Q0006A62Q013Q000100022Q004A017Q004A012Q00014Q00A5012Q000200012Q00BD3Q00013Q00013Q00033Q00030C3Q00496E766F6B655365727665722Q033Q00444E4103073Q0049736C616E647300074Q0076016Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E010C3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100022Q004A017Q004A012Q00014Q00A52Q01000200012Q00BD3Q00013Q00013Q000A3Q0003073Q0067657467656E76030D3Q004175746F427579426F64696573030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q002800013Q00047B012Q002800012Q004A016Q0006E73Q001B0001000100047B012Q001B00012Q004A012Q00013Q0020135Q00030012C3000200044Q00393Q0002000200062C012Q001B00013Q00047B012Q001B00012Q004A012Q00013Q002021014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q00047B012Q001B00012Q004A012Q00013Q00203D5Q000400206Q000500206Q000300122Q000200068Q0002000200062C012Q002200013Q00047B012Q002200010012592Q0100073Q00203C2Q01000100080006A601023Q000100012Q00A88Q00A52Q01000200010012592Q0100073Q0020112Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00BD3Q00013Q00013Q00073Q00027Q0040025Q00802Q40026Q00F03F03073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E00133Q0012C33Q00013Q0012C3000100023Q0012C3000200033Q0004A0012Q00120001001259010400044Q00FD00040001000200203C0104000400050006E70004000A0001000100047B012Q000A000100047B012Q00120001001259010400063Q00203C0104000400070006A601053Q000100022Q004A017Q00A83Q00034Q00A50104000200012Q00AA01035Q000478012Q000400012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q001259012Q00013Q0006A62Q013Q000100022Q004A017Q004A012Q00014Q00A5012Q000200012Q00BD3Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572030B3Q00426F64795570677261646503073Q0049736C616E647300074Q0076016Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B65745765696768747303043Q007461736B03053Q00737061776E010C3Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q000B00013Q00047B012Q000B00010012592Q0100033Q00203C2Q01000100040006A601023Q000100022Q004A017Q004A012Q00014Q00A52Q01000200012Q00BD3Q00013Q00013Q000A3Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B657457656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q002800013Q00047B012Q002800012Q004A016Q0006E73Q001B0001000100047B012Q001B00012Q004A012Q00013Q0020135Q00030012C3000200044Q00393Q0002000200062C012Q001B00013Q00047B012Q001B00012Q004A012Q00013Q002021014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q00047B012Q001B00012Q004A012Q00013Q00203D5Q000400206Q000500206Q000300122Q000200068Q0002000200062C012Q002200013Q00047B012Q002200010012592Q0100073Q00203C2Q01000100080006A601023Q000100012Q00A88Q00A52Q01000200010012592Q0100073Q0020112Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00BD3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q001259012Q00013Q0006A62Q013Q000100012Q004A017Q00A5012Q000200012Q00BD3Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q00576569676874030B3Q0053757065726D61726B657400064Q00567Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q001A3Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B657403053Q0053686F707303093Q0052696E67417265617303063Q00536572766572030B3Q0052616E676553797374656D030F3Q0053757065726D61726B657453652Q6C03043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001633Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q005D00013Q00047B012Q005D00010012592Q0100033Q0020130001000100040012C3000300054Q00390001000300020006DE0002000E0001000100047B012Q000E00010020130002000100040012C3000400064Q00390002000400022Q0079010300033Q00062C0102003400013Q00047B012Q003400010020130004000200040012C3000600074Q00390004000600020006E7000400190001000100047B012Q001900010020130004000200040012C3000600084Q00390004000600020006DE000500290001000400047B012Q002900010020130005000400040012C3000700094Q00390005000700020006E7000500290001000100047B012Q002900010020130005000400040012C30007000A4Q003900050007000200062C0105002900013Q00047B012Q0029000100203C01050004000A0020130005000500040012C3000700094Q003900050007000200062C0105003400013Q00047B012Q003400010020130006000500040012C30008000B4Q003900060008000200061C010300340001000600047B012Q003400010020130006000500040012C30008000C4Q00390006000800022Q00A8000300064Q004A01046Q00FD00040001000200062C0104005200013Q00047B012Q0052000100062C0103005200013Q00047B012Q0052000100201300050003000D0012C30007000E4Q003900050007000200062C0105004300013Q00047B012Q0043000100201300050003000F2Q00B50005000200020006E7000500440001000100047B012Q0044000100203C010500030010001259010600103Q00205C00060006001100122Q000700123Q00122Q000800133Q00122Q000900126Q0006000900024Q00060005000600102Q00040010000600122Q000600143Q00202Q00060006001500122Q000700166Q00060002000100302Q00040017001800044Q0055000100062C0104005500013Q00047B012Q005500010030C7000400170018001259010500143Q00203C0105000500190006A601063Q000100032Q004A012Q00014Q004A012Q00024Q004A012Q00034Q00A501050002000100047B012Q006200012Q004A2Q016Q00FD00010001000200062C2Q01006200013Q00047B012Q006200010030C700010017001A2Q00BD3Q00013Q00013Q00083Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001F00013Q00047B012Q001F00012Q004A016Q002058014Q000300206Q00046Q000200016Q00013Q00064Q00170001000100047B012Q001700012Q004A012Q00023Q0020135Q00050012C3000200064Q00393Q0002000200062C012Q001700013Q00047B012Q001700012Q004A012Q00023Q00203C014Q00060020135Q00050012C3000200074Q00393Q0002000200062C012Q001D00013Q00047B012Q001D00010012592Q0100083Q0006A601023Q000100012Q00A88Q00A52Q01000200012Q00AA016Q00047B014Q00012Q00BD3Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q004A016Q0020135Q00012Q00A5012Q000200012Q00BD3Q00017Q00083Q0003073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E67030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01143Q0012592Q0100014Q00FD0001000100020010152Q0100023Q00062C012Q001300013Q00047B012Q001300010012592Q0100014Q00AD00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q0001000100080006A601023Q000100012Q004A017Q00A52Q01000200012Q00BD3Q00013Q00013Q00093Q0003023Q00543103023Q00543203023Q00543303093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B6574030B3Q0054652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573001B4Q002D3Q00033Q0012C3000100013Q0012C3000200023Q0012C3000300034Q004C012Q000300010012592Q0100043Q0020130001000100050012C3000300064Q00390001000300020006DE0002000E0001000100047B012Q000E00010020130002000100050012C3000400074Q00390002000400020006DE000300130001000200047B012Q001300010020130003000200050012C3000500084Q003900030005000200062C0103001A00013Q00047B012Q001A00012Q004A01046Q00A8000500034Q00A800065Q0012C3000700094Q003B0104000700012Q00BD3Q00017Q00043Q0003073Q00456E61626C656403073Q0067657467656E76030B3Q0057616C6B546F4375626573010001084Q004A2Q015Q0010152Q0100013Q00062C012Q000700013Q00047B012Q000700010012592Q0100024Q00FD0001000100020030C70001000300042Q00BD3Q00017Q00013Q0003053Q0053702Q656401034Q004A2Q015Q0010152Q0100014Q00BD3Q00017Q00053Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E7303093Q0043756265576F726C6403063Q0047726F756E6400103Q001259012Q00013Q0020135Q00020012C3000200034Q00393Q000200020006DE0001000900013Q00047B012Q0009000100201300013Q00020012C3000300044Q00390001000300020006DE0002000E0001000100047B012Q000E00010020130002000100020012C3000400054Q00390002000400022Q00A2000200024Q00BD3Q00017Q00083Q00030F3Q0067657447726F756E64466F6C6465722Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903043Q0053697A65027Q0040001E4Q004A016Q00203C014Q00012Q00FD3Q000100020006E73Q00070001000100047B012Q000700012Q00792Q0100014Q00A2000100023Q00201300013Q00020012C3000300034Q003900010003000200062C2Q01000E00013Q00047B012Q000E000100061C2Q01001200013Q00047B012Q0012000100201300013Q00040012C3000300034Q0080010400014Q00390001000400020006E7000100160001000100047B012Q001600012Q0079010200024Q00A2000200023Q00203C01020001000500203F00020002000600202Q00030001000700202Q00030003000600202Q0003000300084Q0002000200034Q000200028Q00017Q00133Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F7073030C3Q0067657447726F756E64546F7003063Q00697061697273030B3Q004765744368696C6472656E03093Q00626C61636B6C697374030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03013Q0059027Q00C003093Q006D617848656967687403053Q007461626C6503063Q00696E7365727403043Q006375626503043Q0070617274003B3Q00129B012Q00013Q00206Q000200122Q000200038Q000200024Q00015Q00202Q0001000100044Q00010001000200064Q000B00013Q00047B012Q000B00010006E70001000D0001000100047B012Q000D00012Q002D00026Q00A2000200024Q002D00025Q00125E000300053Q00202Q00043Q00064Q000400056Q00033Q000500044Q003700012Q004A01085Q00203C0108000800072Q00510008000800070006E7000800370001000100047B012Q003700010020130008000700020012C3000A00084Q00390008000A00020006E7000800250001000100047B012Q002500010020130008000700020012C3000A00094Q00390008000A00020006E7000800250001000100047B012Q0025000100201300080007000A0012C3000A000B4Q00390008000A000200062C0108003700013Q00047B012Q0037000100203C01090008000C00203C01090009000D2Q0038000900090001000E2F010E00370001000900047B012Q003700012Q004A010A5Q00203C010A000A000F0006B8000900370001000A00047B012Q00370001001259010A00103Q002023010A000A00114Q000B00026Q000C3Q000200102Q000C0012000700102Q000C001300084Q000A000C00010006F7000300130001000200047B012Q001300012Q00A2000200024Q00BD3Q00017Q00063Q0003073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465020D3Q001271000200013Q00202Q00020002000200202Q00033Q000300202Q0004000100034Q00030003000400122Q000400043Q00202Q00053Q000500202Q0006000100054Q0005000500064Q00020005000200202Q0002000200064Q000200028Q00017Q00083Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303073Q0073652Q73696F6E026Q00F03F03073Q00456E61626C6564010003043Q007461736B03053Q00737061776E011A3Q0012972Q0100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D0001000100047B012Q000D00012Q00BD3Q00014Q004A010200013Q0030C7000200050006001259010200073Q00203C0102000200080006A601033Q000100062Q004A012Q00024Q00A83Q00014Q004A017Q004A012Q00034Q004A012Q00044Q004A012Q00054Q00A50102000200012Q00BD3Q00013Q00013Q00203Q0003043Q006D61746803043Q0068756765028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q006375626503063Q00506172656E7403043Q007061727403063Q00697061697273030D3Q00676574466C2Q6F72437562657303083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03073Q0067657467656E76030D3Q0057616C6B437562654465627567030C3Q0067657447726F756E64546F7003053Q007072696E7403353Q005B57616C6B437562655D2074617267657420257320646973743D252E31662068656967687441626F766547726F756E643D252E316603063Q00666F726D617403043Q004E616D6503013Q005903063Q004D6F7665546F027Q0040026Q001840026Q00244003043Q004D6F766503073Q00566563746F723303043Q007A65726F00983Q0012592Q0100013Q00203C2Q01000100020012C3000200033Q0006A601033Q000100032Q004A017Q004A012Q00014Q004A012Q00023Q0006A601040001000100012Q004A012Q00024Q00A8000500034Q00FD00050001000200062C0105008800013Q00047B012Q00880001001259010500043Q0020E900050005000500122Q000600066Q0005000200014Q000500033Q00202Q00050005000700062Q000600180001000500047B012Q001800010020130006000500080012C3000800094Q00390006000800022Q004A010700044Q00FD00070001000200062C0106001E00013Q00047B012Q001E00010006E7000700200001000100047B012Q002000012Q0079016Q00047B012Q0009000100062C012Q002B00013Q00047B012Q002B000100203C01083Q000A00203C01080008000B00062C0108002A00013Q00047B012Q002A000100203C01083Q000C00203C01080008000B0006E70008002B0001000100047B012Q002B00012Q0079016Q0006E73Q00630001000100047B012Q006300012Q0079010800083Q001236010900013Q00202Q00090009000200122Q000A000D6Q000B00023Q00202Q000B000B000E4Q000B00016Q000A3Q000C00044Q004000012Q004A010F00053Q00201801100007000F00202Q0011000E000C00202Q00110011000F4Q000F0011000200062Q000F00400001000900047B012Q004000012Q00A80010000E4Q00A80009000F4Q00A8000800103Q0006F7000A00360001000200047B012Q0036000100062C0108006300013Q00047B012Q006300012Q00A8000A00084Q0006000B00093Q00122Q000C00103Q00202Q000C000C00114Q000C000100024Q0002000C6Q0001000B8Q000A3Q00122Q000A00126Q000A0001000200202Q000A000A001300062Q000A006300013Q00047B012Q006300012Q004A010A00023Q00203C010A000A00142Q00FD000A000100020006E7000A00570001000100047B012Q005700010012C3000A00033Q001259010B00153Q0012D5000C00163Q00202Q000C000C001700202Q000E0008000A00202Q000E000E00184Q000F00093Q00202Q00100008000C00202Q00100010000F00202Q0010001000194Q00100010000A4Q000C00104Q008E010B3Q000100062C012Q008400013Q00047B012Q008400012Q004A010800053Q00209500090007000F00202Q000A3Q000C00202Q000A000A000F4Q0008000A000200202Q00090006001A00202Q000B3Q000C00202Q000B000B000F4Q0009000B000100202Q00090001001B00062Q000800780001000900047B012Q007800012Q00A8000900083Q00124E010A00103Q00202Q000A000A00114Q000A000100024Q0002000A6Q000100093Q00044Q00090001001259010900103Q00203C0109000900112Q00FD0009000100022Q0038000900090002000EF1001C00090001000900047B012Q000900012Q00A8000900043Q002026000A3Q000A00122Q000B001D6Q0009000B00019Q0000044Q0009000100201300080006001A00203C010A0007000F2Q003B0108000A000100047B012Q000900012Q004A010500033Q00203C01050005000700062C0105009100013Q00047B012Q009100012Q004A010500033Q00203C0105000500070020130005000500080012C3000700094Q003900050007000200062C0105009700013Q00047B012Q0097000100201300060005001E0012590108001F3Q00203C0108000800202Q003B0106000800012Q00BD3Q00013Q00023Q00043Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303053Q00416C69766503073Q0073652Q73696F6E00123Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001000013Q00047B012Q001000012Q004A016Q00203C014Q000300062C012Q001000013Q00047B012Q001000012Q004A012Q00014Q004A2Q0100023Q00203C2Q01000100040006613Q000F0001000100047B012Q000F00012Q00BF8Q0080012Q00014Q00A23Q00024Q00BD3Q00017Q000B3Q0003073Q0067657467656E76030D3Q0057616C6B43756265446562756703053Q007072696E74031E3Q005B57616C6B437562655D20736B692Q70696E6720737475636B206375626503043Q004E616D652Q033Q00666F7203013Q007303093Q00626C61636B6C6973742Q0103043Q007461736B03053Q0064656C617902173Q001259010200014Q00FD00020001000200203C01020002000200062C0102000C00013Q00047B012Q000C0001001259010200033Q0012AF000300043Q00202Q00043Q000500122Q000500066Q000600013Q00122Q000700076Q0002000700012Q004A01025Q00204B00020002000800202Q00023Q000900122Q0002000A3Q00202Q00020002000B4Q000300013Q0006A601043Q000100022Q004A017Q00A88Q003B0102000400012Q00BD3Q00013Q00013Q00023Q0003093Q00626C61636B6C6973742Q00054Q00B37Q00206Q00014Q000100013Q00204Q000100026Q00017Q00033Q0003073Q0067657467656E7603093Q0044697361626C65334403153Q00536574336452656E646572696E67456E61626C656401083Q0012D6000100016Q00010001000200102Q000100026Q00015Q00202Q0001000100034Q00038Q0001000300016Q00017Q00023Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E01043Q0012592Q0100014Q00FD0001000100020010152Q0100024Q00BD3Q00017Q00033Q00034Q0003053Q007063612Q6C2Q0100153Q0012C33Q00013Q0012592Q0100023Q0006A601023Q000100022Q00A88Q004A017Q00A52Q0100020001002693012Q000A0001000100047B012Q000A00012Q00802Q0100014Q00A2000100023Q0012592Q0100023Q000209010200014Q00310001000200020006DE000300130001000100047B012Q00130001002693010200120001000300047B012Q001200012Q00BF00036Q0080010300014Q00A2000300024Q00BD3Q00013Q00023Q00013Q00030F3Q00476574452Q726F724D652Q7361676500054Q007D3Q00013Q00206Q00016Q000200029Q006Q00017Q00083Q0003043Q0067616D65030A3Q004765745365727669636503073Q00436F7265477569030E3Q0046696E6446697273744368696C64030F3Q00526F626C6F7850726F6D7074477569030D3Q0070726F6D70744F7665726C617900030B3Q00452Q726F7250726F6D707400173Q0012A63Q00013Q00206Q000200122Q000200038Q0002000200202Q00013Q000400122Q000300056Q00010003000200062Q0002000C0001000100047B012Q000C00010020130002000100040012C3000400064Q0039000200040002002693010200130001000700047B012Q001300010020130003000200040012C3000500084Q00390003000500020026D3000300140001000700047B012Q001400012Q00BF00036Q0080010300014Q00A2000300024Q00BD3Q00017Q00083Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203073Q0073652Q73696F6E026Q00F03F03043Q0062757379010003043Q007461736B03053Q00737061776E01193Q001269000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100034Q00025Q00302Q00020005000600064Q000F0001000100047B012Q000F00012Q00BD3Q00013Q001259010200073Q00203C0102000200080006A601033Q000100052Q004A012Q00014Q00A83Q00014Q004A017Q004A012Q00024Q004A012Q00034Q00A50102000200012Q00BD3Q00013Q00013Q00173Q0003043Q007461736B03043Q0077616974026Q00F03F030E3Q006973446973636F2Q6E656374656403043Q00627573792Q01028Q0003123Q0054656C65706F7274496E69744661696C656403073Q00436F2Q6E65637403113Q0071756575655F6F6E5F74656C65706F727403073Q0067657467656E7603123Q0056656C6F636974794C6F6164737472696E6703053Q007063612Q6C03023Q006F7303053Q00636C6F636B026Q002E40026Q00E03F030A3Q00446973636F2Q6E65637403043Q006D6174682Q033Q006D696E026Q000840026Q003E40012Q005C3Q0006A6014Q000100032Q004A017Q004A012Q00014Q004A012Q00024Q00A800016Q00FD00010001000200062C2Q01005B00013Q00047B012Q005B00010012592Q0100013Q0020562Q010001000200122Q000200036Q0001000200014Q000100023Q00202Q0001000100044Q00010001000200062Q0001000400013Q00047B012Q000400012Q004A2Q0100023Q00203C2Q01000100050006E7000100040001000100047B012Q000400012Q004A2Q0100023Q0030C70001000500060012C3000100074Q00A800026Q00FD00020001000200062C0102005800013Q00047B012Q005800010020D80001000100032Q008001026Q004A010300033Q00203C0103000300080020130003000300090006A601050001000100012Q00A83Q00024Q00390003000500020012590104000A3Q00062C0104003200013Q00047B012Q003200010012590104000B4Q00FD00040001000200203C01040004000C00062C0104003200013Q00047B012Q003200010012590104000D3Q0012590105000A3Q0012590106000B4Q00FD00060001000200203C01060006000C2Q003B0104000600010012590104000D3Q0006A601050002000100032Q004A012Q00034Q004A012Q00024Q004A012Q00044Q00A50104000200010012590104000E3Q00203C01040004000F2Q00FD0004000100022Q00A800056Q00FD00050001000200062C0105004C00013Q00047B012Q004C00010006E70002004C0001000100047B012Q004C00010012590105000E3Q00203C01050005000F2Q00FD0005000100022Q003800050005000400265C0105004C0001001000047B012Q004C0001001259010500013Q00203C0105000500020012C3000600114Q00A501050002000100047B012Q003B00010020130005000300122Q00B200050002000100122Q000500013Q00202Q00050005000200122Q000600133Q00202Q00060006001400202Q00070001001500122Q000800166Q000600086Q00053Q00014Q00025Q00047B012Q001800012Q004A010200023Q0030C700020005001700047B012Q000400012Q00BD3Q00013Q00033Q00043Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203053Q00416C69766503073Q0073652Q73696F6E00123Q001259012Q00014Q00FD3Q0001000200203C014Q000200062C012Q001000013Q00047B012Q001000012Q004A016Q00203C014Q000300062C012Q001000013Q00047B012Q001000012Q004A012Q00014Q004A2Q0100023Q00203C2Q01000100040006613Q000F0001000100047B012Q000F00012Q00BF8Q0080012Q00014Q00A23Q00024Q00BD3Q00019Q003Q00034Q0080012Q00014Q004A8Q00BD3Q00017Q00033Q0003173Q0054656C65706F7274546F506C616365496E7374616E636503073Q00706C616365496403053Q006A6F62496400094Q0069016Q00206Q00014Q000200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q000400028Q000400016Q00017Q00073Q0003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C756501163Q0012922Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A0001000100047B012Q000A00010020130002000100040012C3000400054Q003900020004000200062C0102001500013Q00047B012Q0015000100062C012Q001300013Q00047B012Q00130001001259010300014Q00FD00030001000200203C01030003000700101501020006000300047B012Q001500012Q004A010300013Q0010150102000600032Q00BD3Q00017Q00073Q0003073Q0067657467656E76030E3Q0057616C6B53702Q656456616C7565030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q0012342Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q00047B012Q001200012Q004A2Q015Q00203C2Q01000100040006DE0002000F0001000100047B012Q000F00010020130002000100050012C3000400064Q003900020004000200062C0102001200013Q00047B012Q00120001001015010200074Q00BD3Q00017Q000A3Q0003073Q0067657467656E76030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C7565026Q00494001163Q0012922Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A0001000100047B012Q000A00010020130002000100040012C3000400054Q003900020004000200062C0102001500013Q00047B012Q0015000100062C012Q001400013Q00047B012Q001400010030C70002000600070012F4000300016Q00030001000200202Q00030003000900102Q00020008000300044Q001500010030C700020008000A2Q00BD3Q00017Q00093Q0003073Q0067657467656E76030E3Q004A756D70506F77657256616C7565030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F77657201143Q0012342Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001300013Q00047B012Q001300012Q004A2Q015Q00203C2Q01000100040006DE0002000F0001000100047B012Q000F00010020130002000100050012C3000400064Q003900020004000200062C0102001300013Q00047B012Q001300010030C7000200070008001015010200094Q00BD3Q00017Q00023Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7001043Q0012592Q0100014Q00FD0001000100020010152Q0100024Q00BD3Q00017Q00043Q0003083Q00746F737472696E6703043Q0067737562030C3Q005E25732A282E2D2925732A2403023Q00253101093Q0012282Q0100016Q00028Q00010002000200202Q00010001000200122Q000300033Q00122Q000400046Q0001000400024Q000100028Q00017Q00123Q00028Q0003063Q00737472696E6703053Q00752Q70657203043Q006773756203013Q002C034Q002Q033Q0025732B026Q00F03F03043Q0066696E6403013Q004B025Q00408F4003013Q004D024Q0080842E4103013Q0042024Q00652QCD4103013Q0054023Q00A0941A6D4203083Q00746F6E756D626572014D3Q0006E73Q00040001000100047B012Q000400010012C3000100014Q00A2000100023Q0012592Q0100023Q00204B2Q01000100034Q00028Q0001000200026Q00013Q00202Q00013Q000400122Q000300053Q00122Q000400066Q00010004000200202Q00010001000400122Q000300073Q0012C3000400064Q00990001000400026Q00013Q00122Q000100083Q00202Q00023Q000900122Q0004000A6Q00020004000200062Q0002001F00013Q00047B012Q001F00010012C30001000B3Q00202A01023Q000400122Q0004000A3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201300023Q00090012C30004000C4Q003900020004000200062C0102002B00013Q00047B012Q002B00010012C30001000D3Q00202A01023Q000400122Q0004000C3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201300023Q00090012C30004000E4Q003900020004000200062C0102003700013Q00047B012Q003700010012C30001000F3Q00202A01023Q000400122Q0004000E3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201300023Q00090012C3000400104Q003900020004000200062C0102004200013Q00047B012Q004200010012C3000100113Q0020DF00023Q000400122Q000400103Q00122Q000500066Q0002000500026Q00023Q001259010200124Q00A800036Q00B500020002000200062C0102004A00013Q00047B012Q004A00012Q00CF0003000200010006E70003004B0001000100047B012Q004B00010012C3000300014Q00A2000300024Q00BD3Q00017Q000E3Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D6503063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A65637403053Q007461626C6503063Q00696E7365727403043Q004E616D6503043Q00736F7274003A4Q000C7Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q00047B012Q001E000100201300013Q00010012C3000300034Q003900010003000200062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00030020130001000100010012C3000300044Q003900010003000200062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00030020212Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q00047B012Q001E000100203C2Q013Q000300203D00010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006E7000100220001000100047B012Q002200012Q0079010200024Q00A2000200024Q002D00025Q00125E000300073Q00202Q0004000100084Q000400056Q00033Q000500044Q003200010020130008000700090012C3000A000A4Q00390008000A000200062C0108003200013Q00047B012Q003200010012590108000B3Q00203C01080008000C2Q00A8000900023Q00203C010A0007000D2Q003B0108000A00010006F7000300280001000200047B012Q002800010012590103000B3Q0020AE00030003000E4Q000400026Q0003000200014Q000200028Q00017Q00163Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D65028Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A656374026Q00F03F030E3Q0047657444657363656E64616E747303073Q0056697369626C6503043Q004E616D6503063Q004C6F636B656403043Q004C6F636B03073Q00556E6B6E6F776E030A3Q00496D6167654C6162656C030B3Q00496D616765436F6C6F723303063Q00436F6C6F72332Q033Q006E657700634Q000C7Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q00047B012Q001E000100201300013Q00010012C3000300034Q003900010003000200062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00030020130001000100010012C3000300044Q003900010003000200062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00030020212Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q00047B012Q001E000100203C2Q013Q000300203D00010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006E7000100220001000100047B012Q002200012Q0079010200024Q00A2000200023Q0012C3000200073Q0012C3000300073Q00125E000400083Q00202Q0005000100094Q000500066Q00043Q000600044Q005D000100201300090008000A0012C3000B000B4Q00390009000B000200062C0109005D00013Q00047B012Q005D00010020D800030003000C2Q008800095Q00122Q000A00083Q00202Q000B0008000D4Q000B000C6Q000A3Q000C00044Q00580001002013000F000E000A0012C30011000B4Q0039000F0011000200062C010F004800013Q00047B012Q0048000100203C010F000E000E00062C010F004800013Q00047B012Q0048000100203C010F000E000F002693010F00460001001000047B012Q0046000100203C010F000E000F002693010F00460001001100047B012Q0046000100203C010F000E000F0026D3000F00480001001200047B012Q004800012Q0080010900013Q00047B012Q005A0001002013000F000E000A0012C3001100134Q0039000F0011000200062C010F005800013Q00047B012Q0058000100203C010F000E0014001265001000153Q00202Q00100010001600122Q001100073Q00122Q001200073Q00122Q001300076Q00100013000200062Q000F00580001001000047B012Q005800012Q0080010900013Q00047B012Q005A00010006F7000A00350001000200047B012Q003500010006E70009005D0001000100047B012Q005D00010020D800020002000C0006F7000400290001000200047B012Q002900012Q00A8000400024Q00A8000500034Q0053010400034Q00BD3Q00017Q00043Q00030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657200114Q00967Q00206Q000100122Q000200028Q0002000200064Q000A00013Q00047B012Q000A000100201300013Q00010012C3000300034Q00390001000300022Q00A83Q00013Q0006DE0001000F00013Q00047B012Q000F000100201300013Q00010012C3000300044Q00390001000300022Q00A2000100024Q00BD3Q00017Q00193Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q6665727303063Q0069706169727303053Q00546965723103053Q00546965723203053Q0054696572332Q033Q005065742Q033Q0049734103093Q00546578744C6162656C03073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03043Q006E616D6503043Q005465787403053Q00707269636503013Q003F03093Q00617661696C61626C6503073Q0056697369626C65034Q0003023Q00783000674Q000C7Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q00047B012Q001E000100201300013Q00010012C3000300034Q003900010003000200062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00030020130001000100010012C3000300044Q003900010003000200062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00030020212Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q00047B012Q001E000100203C2Q013Q000300203D00010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006E7000100220001000100047B012Q002200012Q0079010200024Q00A2000200024Q002D00025Q001242010300076Q000400033Q00122Q000500083Q00122Q000600093Q00122Q0007000A6Q0004000300012Q003100030002000500047B012Q006300010020130008000100012Q00A8000A00074Q00390008000A00020006DE000900330001000800047B012Q003300010020130009000800010012C3000B000B4Q00390009000B000200062C0109006300013Q00047B012Q00630001002013000A0009000C0012C3000C000D4Q0039000A000C000200062C010A006300013Q00047B012Q00630001002013000A0008000100120E000C000E6Q000A000C000200202Q000B0008000100122Q000D000F6Q000B000D000200202Q000C0008000100122Q000E00106Q000C000E000200062Q000D00480001000C00047B012Q00480001002013000D000C00010012C3000F00114Q0039000D000F00022Q002D000E3Q000300203C010F00090013001015010E0012000F00062C010D005000013Q00047B012Q0050000100203C010F000D00130006E7000F00510001000100047B012Q005100010012C3000F00153Q001015010E0014000F00062C010A005700013Q00047B012Q0057000100203C010F000A00170006E7000F005F0001000100047B012Q005F000100062C010B005C00013Q00047B012Q005C000100203C010F000B00130006E7000F005D0001000100047B012Q005D00010012C3000F00183Q0026D3000F00600001001900047B012Q006000012Q00BF000F6Q0080010F00013Q001015010E0016000F2Q000B01020007000E0006F70003002B0001000200047B012Q002B00012Q00A2000200024Q00BD3Q00017Q00033Q0003063Q0069706169727303073Q0067657467656E7603123Q0053656C656374656454617267657450657473010F3Q0012B1000100013Q00122Q000200026Q00020001000200202Q0002000200034Q00010002000300044Q000A000100067D0105000A00013Q00047B012Q000A00012Q0080010600014Q00A2000600023Q0006F7000100060001000200047B012Q000600012Q00802Q016Q00A2000100024Q00BD3Q00017Q00093Q0003073Q0067657467656E7603123Q0053656C65637465645461726765745065747303063Q0069706169727303053Q007461626C6503063Q0072656D6F76652Q033Q006D617803043Q004D617820030E3Q00207065747320726561636865642103063Q00696E7365727401263Q0012EF000100016Q00010001000200202Q00010001000200122Q000200036Q000300016Q00020002000400044Q0010000100067D0106001000013Q00047B012Q00100001001259010700043Q0020140007000700054Q000800016Q000900056Q0007000900014Q000700016Q000700023Q0006F7000200070001000200047B012Q000700012Q00C8000200014Q004A01035Q00203C0103000300060006B80003001E0001000200047B012Q001E00012Q008001025Q001232010300076Q00045Q00202Q00040004000600122Q000500086Q0003000300054Q000200033Q001259010200043Q00208C0002000200094Q000300016Q00048Q0002000400014Q000200016Q000200028Q00017Q00043Q0003053Q007461626C6503053Q00636C65617203073Q0067657467656E7603123Q0053656C65637465645461726765745065747300073Q0012ED3Q00013Q00206Q000200122Q000100036Q00010001000200202Q0001000100046Q000200016Q00017Q00063Q0003073Q0067657467656E7603123Q0053656C656374656454617267657450657473028Q0003043Q004E6F6E65026Q00F03F2Q033Q00202B2000143Q001248012Q00018Q0001000200206Q00024Q00015Q00262Q000100080001000300047B012Q000800010012C3000100044Q00A2000100024Q00C800015Q0026D30001000D0001000500047B012Q000D000100203C2Q013Q00052Q00A2000100023Q00203C2Q013Q00050012F3000200066Q00035Q00202Q0003000300054Q0001000100034Q000100028Q00017Q00033Q0003073Q0067657467656E7603143Q004D65726368616E744D6178507269636554657874034Q0001073Q0012592Q0100014Q00FD00010001000200061C0102000500013Q00047B012Q000500010012C3000200033Q0010152Q01000200022Q00BD3Q00017Q00063Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01173Q0012972Q0100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D0001000100047B012Q000D00012Q00BD3Q00013Q001259010200053Q00203C0102000200060006A601033Q000100052Q00A83Q00014Q004A017Q004A012Q00014Q004A012Q00024Q004A012Q00034Q00A50102000200012Q00BD3Q00013Q00013Q00283Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657203063Q0069706169727303123Q0053656C65637465645461726765745065747303063Q00737472696E6703053Q00752Q7065722Q0103043Q006E65787403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q66657273030A3Q007061727365507269636503143Q004D65726368616E744D617850726963655465787403053Q00546965723103053Q00546965723203053Q0054696572332Q033Q0050657403073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03073Q0056697369626C6503043Q0054657874034Q002Q033Q0049734103093Q00546578744C6162656C03023Q007830028Q0003053Q007063612Q6C0003043Q007461736B03043Q0077616974026Q33D33F00BF4Q002D8Q00792Q0100013Q001259010200014Q00FD00020001000200203C01020002000200062C010200BE00013Q00047B012Q00BE00012Q004A01026Q004A010300013Q00203C01030003000300067D010200BE0001000300047B012Q00BE00010006E70001001E0001000100047B012Q001E00012Q004A010200023Q0020130002000200040012C3000400054Q003900020004000200062C0102001800013Q00047B012Q001800010020130003000200040012C3000500064Q00390003000500022Q00A8000200033Q0006DE0001001E0001000200047B012Q001E00010020130003000200040012C3000500074Q00390003000500022Q00A8000100034Q002D00025Q0012B1000300083Q00122Q000400016Q00040001000200202Q0004000400094Q00030002000500044Q002C00010012590108000A3Q00200001080008000B4Q000900036Q000A00076Q0009000A6Q00083Q000200202Q00020008000C0006F7000300250001000200047B012Q0025000100062C2Q0100B900013Q00047B012Q00B900010012590103000D4Q00A8000400024Q00B500030002000200062C010300B900013Q00047B012Q00B900012Q004A010300043Q0020130003000300040012C30005000E4Q00390003000500020006DE000400530001000300047B012Q005300010020130004000300040012C30006000F4Q003900040006000200062C0104005300013Q00047B012Q0053000100203C01040003000F0020130004000400040012C3000600104Q003900040006000200062C0104005300013Q00047B012Q0053000100203C01040003000F00202101040004001000202Q00040004000400122Q000600116Q00040006000200062Q0004005300013Q00047B012Q0053000100203C01040003000F00203D00040004001000202Q00040004001100202Q00040004000400122Q000600126Q00040006000200062C010400B700013Q00047B012Q00B700012Q004A010500013Q00208B01050005001300122Q000600016Q00060001000200202Q0006000600144Q00050002000200122Q000600086Q000700033Q00122Q000800153Q00122Q000900163Q00122Q000A00176Q0007000300012Q003100060002000800047B012Q00B40001002013000B000400042Q00A8000D000A4Q0039000B000D00022Q0080010C5Q00062C010B00B000013Q00047B012Q00B00001002013000D000B00040012C3000F00184Q0039000D000F0002002013000E000B000400120E001000196Q000E0010000200202Q000F000B000400122Q0011001A6Q000F0011000200202Q0010000B000400122Q0012001B6Q00100012000200062Q0011007A0001001000047B012Q007A00010020130011001000040012C30013001C4Q00390011001300020006DE0012007D0001000E00047B012Q007D000100203C0112000E001D00062C010F008200013Q00047B012Q0082000100203C0113000F001E0006E7001300830001000100047B012Q008300010012C30013001F3Q00062C010D00B000013Q00047B012Q00B000010020130014000D00200012C3001600214Q003900140016000200062C011400B000013Q00047B012Q00B000010006E7001200B00001000100047B012Q00B00001002693011300B00001002200047B012Q00B000010012590114000A3Q00201001140014000B4Q001500033Q00202Q0016000D001E4Q001500166Q00143Q00024Q00140002001400062Q001400B000013Q00047B012Q00B0000100062C0111009F00013Q00047B012Q009F00012Q004A011400013Q00203C01140014001300203C01150011001E2Q00B50014000200020006E7001400A00001000100047B012Q00A000010012C3001400233Q002664010500A40001002300047B012Q00A400010006B8001400B00001000500047B012Q00B000012Q0080010C00014Q005100153Q000A0006E7001500B00001000100047B012Q00B00001001259011500243Q0006A601163Q000100022Q00A83Q00014Q00A83Q000A4Q00B500150002000200062C011500B000013Q00047B012Q00B00001002044012Q000A000C0006E7000C00B30001000100047B012Q00B30001002044012Q000A00252Q00AA01095Q0006F7000600630001000200047B012Q0063000100047B012Q00B900012Q002D00056Q00A83Q00053Q001259010300263Q00203C0103000300270012C3000400284Q00A501030002000100047B012Q000200012Q00BD3Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q005D016Q00206Q00014Q000200018Q000200016Q00017Q00053Q0003083Q00496E7374616E63652Q033Q006E657703053Q00706169727303063Q00506172656E7403063Q00697061697273031D3Q001259010300013Q00203C0103000300022Q00A800046Q00B5000300020002001259010400034Q00A8000500014Q003100040002000600047B012Q000B00010026930107000B0001000400047B012Q000B00012Q000B0103000700080006F7000400080001000200047B012Q0008000100062C0102001600013Q00047B012Q00160001001259010400054Q00A8000500024Q003100040002000600047B012Q001400010010150108000400030006F7000400130001000200047B012Q0013000100203C01040001000400062C0104001B00013Q00047B012Q001B000100203C0104000100040010150103000400042Q00A2000300024Q00BD3Q00017Q00073Q0003083Q005549436F726E6572030C3Q00436F726E657252616469757303043Q005544696D2Q033Q006E6577028Q00026Q00144003063Q00506172656E74020F4Q000200025Q00122Q000300016Q00043Q000200122Q000500033Q00202Q00050005000400122Q000600053Q00062Q000700090001000100047B012Q000900010012C3000700064Q003900050007000200108200040002000500102Q000400076Q000200046Q00029Q0000017Q00083Q0003083Q0055495374726F6B6503053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q00E03F03093Q00546869636B6E652Q73026Q00F03F03063Q00506172656E7404144Q004A01045Q0012C3000500014Q002D00063Q000400061C010700070001000100047B012Q000700012Q004A010700013Q00203C01070007000300101501060002000700061C0107000B0001000200047B012Q000B00010012C3000700053Q00101501060004000700061C0107000F0001000300047B012Q000F00010012C3000700073Q001015010600060007001015010600084Q000F010400064Q000501046Q00BD3Q00017Q00013Q0003053Q007063612Q6C030E3Q001259010300013Q0006A601043Q000100042Q004A017Q00A83Q00014Q00A83Q00024Q00A88Q003100030002000400062C0103000B00013Q00047B012Q000B000100061C0105000C0001000400047B012Q000C00012Q0079010500054Q00A2000500024Q00BD3Q00013Q00013Q00093Q0003083Q005549536861646F77030A3Q00426C757252616469757303043Q005544696D2Q033Q006E6577028Q00026Q002840030C3Q005472616E73706172656E6379026Q00E03F03063Q00506172656E7400164Q007A7Q00122Q000100016Q00023Q000300122Q000300033Q00202Q00030003000400122Q000400056Q000500013Q00062Q0005000A0001000100047B012Q000A00010012C3000500064Q00390003000500020010150102000200032Q004A010300023Q0006E7000300100001000100047B012Q001000010012C3000300083Q0010150102000700032Q0029010300033Q00102Q0002000900036Q00029Q008Q00017Q00023Q0003063Q0043726561746503043Q00506C6179030C4Q004A01035Q0020130003000300012Q00A800055Q00061C010600060001000100047B012Q000600012Q004A010600014Q00A8000700024Q008F01030007000200202Q0004000300024Q0004000200014Q000300028Q00017Q00133Q0003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0054657874034Q0003083Q00466F6E744661636503043Q00466F6E742Q033Q006E657703043Q00456E756D030A3Q00466F6E7457656967687403043Q00426F6C6403073Q00526567756C6172030A3Q0054657874436F6C6F7233030A3Q00546578745363616C65642Q0103043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574026Q002C40042A4Q004300045Q00122Q000500016Q00063Q000600302Q00060002000300062Q0007000700013Q00047B012Q000700010012C3000700053Q001015010600040007001259010700073Q00203C0107000700082Q004A010800013Q00062C0102001200013Q00047B012Q00120001001259010900093Q00203C01090009000A00203C01090009000B0006E7000900150001000100047B012Q00150001001259010900093Q00203C01090009000A00203C01090009000C2Q003900070009000200101501060006000700061C0107001B0001000300047B012Q001B00012Q004A010700023Q00203C0107000700040010150106000D00070030C70006000E000F001259010700113Q00203C01070007001200061C010800220001000100047B012Q002200010012C3000800133Q00061C010900250001000100047B012Q002500010012C3000900134Q00390007000900020010150106001000072Q000F010400064Q000501046Q00BD3Q00017Q00153Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q00284003083Q00506F736974696F6E03063Q00506172656E74026Q0028C003053Q007461626C6503063Q00696E7365727403063Q00697061697273030A3Q00496E707574426567616E03073Q00436F2Q6E656374030C3Q00496E7075744368616E67656402953Q00061C010600030001000100047B012Q000300012Q002D00066Q004A01075Q00121C000800016Q00093Q000600302Q00090002000300302Q00090004000500302Q00090006000700122Q000A00093Q00202Q000A000A000A00122Q000B00073Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000C6Q000A000E000200102Q00090008000A00122Q000A00093Q00202Q000A000A000A00122Q000B000B3Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000B6Q000A000E000200102Q0009000D000A00102Q0009000E6Q0007000900024Q00085Q00122Q000900016Q000A3Q000600302Q000A0002000300302Q000A0004000500302Q000A0006000700122Q000B00093Q00202Q000B000B000A00122Q000C00073Q00122Q000D000B3Q00122Q000E000B3Q00122Q000F000C6Q000B000F000200102Q000A0008000B00122Q000B00093Q00202Q000B000B000A00122Q000C000B3Q00122Q000D000B3Q00122Q000E00073Q00122Q000F000F6Q000B000F000200102Q000A000D000B00102Q000A000E6Q0008000A00024Q00095Q00122Q000A00016Q000B3Q000600302Q000B0002000300302Q000B0004000500302Q000B0006000700122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000C3Q00122Q000F00073Q00122Q0010000B6Q000C0010000200102Q000B0008000C00122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000B3Q00122Q000F000B3Q00122Q0010000B6Q000C0010000200102Q000B000D000C00102Q000B000E6Q0009000B00024Q000A5Q00122Q000B00016Q000C3Q000600302Q000C0002000300302Q000C0004000500302Q000C0006000700122Q000D00093Q00202Q000D000D000A00122Q000E000B3Q0012C3000F000C3Q0012BB001000073Q00122Q0011000B6Q000D0011000200102Q000C0008000D00122Q000D00093Q00202Q000D000D000A00122Q000E00073Q00122Q000F000F3Q00122Q0010000B3Q00122Q0011000B6Q000D0011000200102Q000C000D000D00102Q000C000E6Q000A000C000200122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00076Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00086Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00096Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D000A6Q000B000D000100122Q000B00126Q000C00066Q000B0002000D00044Q0088000100203C0110000F00130020130010001000140006A601123Q000100042Q00A83Q00024Q00A83Q00044Q00A83Q00054Q00A88Q003B01100012000100203C0110000F00150020130010001000140006A601120001000100012Q00A83Q00034Q003B0110001200010006F7000B007B0001000200047B012Q007B00012Q004A010B00014Q004A010C00023Q00203C010C000C00150006A6010D0002000100052Q00A83Q00034Q00A83Q00024Q00A83Q00044Q00A88Q00A83Q00054Q003B010B000D00012Q00BD3Q00013Q00033Q00073Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636803083Q00506F736974696F6E03073Q004368616E67656403073Q00436F2Q6E656374011A3Q0020E400013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C0001000200047B012Q000C000100203C2Q013Q0001001259010200023Q00203C01020002000100203C01020002000400067D2Q0100190001000200047B012Q001900012Q00802Q0100014Q009200015Q00202Q00013Q00054Q000100016Q000100033Q00202Q0001000100054Q000100023Q00202Q00013Q000600202Q0001000100070006A601033Q000100022Q00A88Q004A017Q003B2Q01000300012Q00BD3Q00013Q00013Q00033Q00030E3Q0055736572496E707574537461746503043Q00456E756D2Q033Q00456E64000A4Q001B016Q00206Q000100122Q000100023Q00202Q00010001000100202Q00010001000300064Q00090001000100047B012Q000900012Q0080017Q004A3Q00014Q00BD3Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F756368010E3Q0020E400013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C0001000200047B012Q000C000100203C2Q013Q0001001259010200023Q00203C01020002000100203C01020002000400067D2Q01000D0001000200047B012Q000D00012Q004A8Q00BD3Q00017Q00073Q0003083Q00506F736974696F6E03053Q005544696D322Q033Q006E657703013Q005803053Q005363616C6503063Q004F2Q6673657403013Q0059011F4Q004A2Q015Q00067D012Q001E0001000100047B012Q001E00012Q004A2Q0100013Q00062C2Q01001E00013Q00047B012Q001E000100203C2Q013Q00012Q0072010200026Q0001000100024Q000200033Q00122Q000300023Q00202Q0003000300034Q000400043Q00202Q00040004000400202Q0004000400054Q000500043Q00202Q00050005000400202Q00050005000600202Q0006000100044Q0005000500064Q000600043Q00202Q00060006000700202Q0006000600054Q000700043Q00202Q00070007000700202Q00070007000600202Q0008000100074Q0007000700084Q00030007000200102Q0002000100032Q00BD3Q00017Q00043Q00030A3Q00496E707574426567616E03073Q00436F2Q6E656374030A3Q00496E707574456E646564030C3Q00496E7075744368616E67656402184Q008001025Q0006A601033Q000100022Q00A88Q00A83Q00013Q00203C01043Q00010020130004000400020006A601060001000100022Q00A83Q00024Q00A83Q00034Q003B0104000600012Q004A01046Q004A010500013Q00203C0105000500030006A601060002000100012Q00A83Q00024Q003B0104000600012Q004A01046Q004A010500013Q00203C0105000500040006A601060003000100022Q00A83Q00024Q00A83Q00034Q003B0104000600012Q00BD3Q00013Q00043Q00093Q0003103Q004162736F6C757465506F736974696F6E030C3Q004162736F6C75746553697A6503043Q006D61746803053Q00636C616D7003083Q00506F736974696F6E03013Q0058028Q00026Q00F03F03013Q0059011F4Q00032Q015Q00202Q0001000100014Q00025Q00202Q00020002000200122Q000300033Q00202Q00030003000400202Q00043Q000500202Q00040004000600202Q0005000100064Q00040004000500203C0105000200062Q00C20004000400050012C3000500073Q0012C3000600084Q0039000300060002001259010400033Q00203C01040004000400203C01053Q000500203C01050005000900203C0106000100092Q003800050005000600203C0106000200092Q004001050005000600122Q000600073Q00122Q000700086Q0004000700024Q000500016Q000600036Q000700046Q0005000700016Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636801123Q0020E400013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C0001000200047B012Q000C000100203C2Q013Q0001001259010200023Q00203C01020002000100203C01020002000400067D2Q0100110001000200047B012Q001100012Q00802Q0100014Q004A00016Q004A2Q0100014Q00A800026Q00A52Q01000200012Q00BD3Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F756368010F3Q0020E400013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C0001000200047B012Q000C000100203C2Q013Q0001001259010200023Q00203C01020002000100203C01020002000400067D2Q01000E0001000200047B012Q000E00012Q00802Q016Q004A00016Q00BD3Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F75636801134Q004A2Q015Q00062C2Q01001200013Q00047B012Q0012000100203C2Q013Q0001001259010200023Q00203C01020002000100203C0102000200030006610001000F0001000200047B012Q000F000100203C2Q013Q0001001259010200023Q00203C01020002000100203C01020002000400067D2Q0100120001000200047B012Q001200012Q004A2Q0100014Q00A800026Q00A52Q01000200012Q00BD3Q00017Q00093Q0003063Q0067657468756903053Q007063612Q6C030B3Q004C6F63616C506C61796572030C3Q0057616974466F724368696C6403093Q00506C61796572477569026Q00144003043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900223Q001259012Q00013Q00062C012Q000B00013Q00047B012Q000B0001001259012Q00023Q0012592Q0100014Q00313Q0002000100062C012Q000B00013Q00047B012Q000B000100062C2Q01000B00013Q00047B012Q000B00012Q00A2000100023Q001259012Q00023Q0002092Q016Q00313Q0002000100062C012Q001300013Q00047B012Q0013000100062C2Q01001300013Q00047B012Q001300012Q00A2000100024Q004A01025Q00203C01020002000300062C0102001C00013Q00047B012Q001C00010020130003000200040012C3000500053Q0012C3000600064Q000F010300064Q000501035Q001259010300073Q0020E300030003000800122Q000500096Q000300056Q00039Q0000013Q00013Q00043Q0003083Q00636C6F6E6572656603043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900113Q001259012Q00013Q00062C012Q000B00013Q00047B012Q000B0001001259012Q00013Q001204000100023Q00202Q00010001000300122Q000300046Q000100039Q00000200064Q000F0001000100047B012Q000F0001001259012Q00023Q0020135Q00030012C3000200044Q00393Q000200022Q00A23Q00024Q00BD3Q00017Q00023Q002Q033Q0073796E030B3Q0070726F746563745F677569000C3Q001259012Q00013Q00062C012Q000B00013Q00047B012Q000B0001001259012Q00013Q00203C014Q000200062C012Q000B00013Q00047B012Q000B0001001259012Q00013Q00203C014Q00022Q004A2Q016Q00A5012Q000200012Q00BD3Q00017Q003E3Q0003053Q00416C697665010003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C65030B3Q004175746F47656D57616C6B030F3Q004A756D70506F776572546F2Q676C6503083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E6703093Q0044697361626C65334403063Q0069706169727303083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q6703103Q004175746F486174636843756265452Q6703063Q004E6F636C6970030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030C3Q00496E66696E6974654A756D70030E3Q004D65726368616E74536E69706572030B3Q0057616C6B546F437562657303073Q00456E61626C656403073Q0073652Q73696F6E026Q00F03F03053Q007063612Q6C03053Q007461626C6503053Q00636C65617203093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403093Q004A756D70506F776572026Q004940030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F72656403093Q00776F726B7370616365030A3Q00426F2Q734D6F64656C73030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q004B657953797374656D426C757203073Q0044657374726F7903133Q0056656C6F63697479487562496E7374616E63650001B24Q004A2Q015Q00203C2Q01000100010006E7000100050001000100047B012Q000500012Q00BD3Q00014Q004A2Q015Q0030C70001000100020012592Q0100034Q00FD00010001000200203C0102000100040006E70002000D0001000100047B012Q000D000100203C01020001000500203C01030001000600203C0104000100070006E7000400120001000100047B012Q0012000100203C01040001000800203C01050001000900204800060001000A00122Q0007000B6Q000800183Q00122Q0009000C3Q00122Q000A000D3Q00122Q000B000E3Q00122Q000C000F3Q00122Q000D00103Q00122Q000E00113Q00122Q000F00123Q001234001000133Q00122Q001100143Q00122Q001200153Q00122Q001300053Q00122Q001400073Q00122Q001500083Q00122Q001600093Q00122Q001700163Q00122Q001800173Q00122Q001900183Q001234001A00193Q00122Q001B001A3Q00122Q001C001B3Q00122Q001D001C3Q00122Q001E001D3Q00122Q001F001E3Q00122Q0020000A3Q00122Q0021001F3Q00122Q002200203Q00122Q002300043Q0012C3002400063Q0012C3002500213Q0012C3002600223Q0012C3002700234Q004C0108001F00012Q003100070002000900047B012Q003900010020442Q01000B00020006F7000700380001000200047B012Q003800012Q004A010700013Q0030150007002400024Q000700026Q000800023Q00202Q00080008002500202Q00080008002600102Q0007002500084Q000700036Q000800033Q00202Q00080008002500202Q00080008002600102Q0007002500084Q000700046Q000800043Q00202Q00080008002500202Q00080008002600102Q0007002500084Q000700056Q00070001000100122Q0007000B6Q000800066Q00070002000900044Q00570001001259010C00273Q0006A6010D3Q000100012Q00A83Q000B4Q00A5010C000200012Q00AA010A5Q0006F7000700520001000200047B012Q00520001001259010700283Q0020770007000700294Q000800066Q0007000200014Q000700073Q00202Q00070007002A00062Q000800640001000700047B012Q0064000100201300080007002B0012C3000A002C4Q00390008000A000200062C0108007100013Q00047B012Q0071000100062C0102006E00013Q00047B012Q006E000100201300090008002D001202010B002E3Q00202Q000B000B002F4Q0009000B00014Q000900083Q00102Q00080030000900062C0103007100013Q00047B012Q007100010030C700080031003200062C0104007B00013Q00047B012Q007B00010006DE000900780001000700047B012Q007800010020130009000700330012C3000B00344Q00390009000B000200062C0109007B00013Q00047B012Q007B00010030C700090035000200062C0105009500013Q00047B012Q00950001001259010900363Q0020130009000900330012C3000B00374Q00390009000B000200062C0109009500013Q00047B012Q00950001001259010A000B3Q002013000B000900382Q0076000B000C4Q0057000A3Q000C00047B012Q00930001002013000F000E00390012C30011003A4Q0039000F0011000200062C010F009000013Q00047B012Q00900001002013000F000E00330012C3001100344Q0039000F0011000200062C010F009300013Q00047B012Q009300010030C7000F003500020006F7000A00880001000200047B012Q0088000100062C0106009B00013Q00047B012Q009B0001001259010900273Q0006A6010A0001000100012Q004A012Q00094Q00A50109000200012Q004A0109000A3Q0020130009000900330012C3000B003B4Q00390009000B000200062C010900A300013Q00047B012Q00A30001002013000A0009003C2Q00A5010A00020001001259010A00034Q00FD000A0001000200203C010A000A003D2Q004A010B5Q00067D010A00AC0001000B00047B012Q00AC0001001259010A00034Q00FD000A000100020030C7000A003D003E0006E73Q00B10001000100047B012Q00B100012Q004A010A000B3Q002013000A000A003C2Q00A5010A000200012Q00BD3Q00013Q00023Q00013Q00030A3Q00446973636F2Q6E65637400044Q004A016Q0020135Q00012Q00A5012Q000200012Q00BD3Q00017Q00013Q0003153Q00536574336452656E646572696E67456E61626C656400054Q001B7Q00206Q00014Q000200018Q000200016Q00019Q003Q00044Q004A017Q00802Q016Q00A5012Q000200012Q00BD3Q00019Q003Q00044Q004A017Q00802Q0100014Q00A5012Q000200012Q00BD3Q00017Q003C3Q0003083Q00496E7374616E63652Q033Q006E6577030A3Q00426C7572452Q6665637403043Q004E616D65030D3Q004B657953797374656D426C757203043Q0053697A65026Q00344003063Q00506172656E74028Q0003053Q004672616D65030E3Q004B657953797374656D4672616D65030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E03053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240025Q00C0624003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03063Q004163746976652Q01026Q00184003063Q005374726F6B65026Q00304003093Q00546578744C6162656C03043Q005465787403173Q004B65792053797374656D20566572696669636174696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40026Q002840026Q00F03F03093Q005365636F6E64617279025Q00804640026Q006E40026Q003E40026Q00104003073Q0054657874426F78034Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q005375625465787403103Q00436C656172546578744F6E466F6375730100026Q0024C0026Q001440030A3Q005465787442752Q746F6E030A3Q005375626D6974204B657903063Q00412Q63656E74025Q00805640030F3Q004175746F42752Q746F6E436F6C6F7203093Q0041637469766174656403073Q00436F2Q6E65637402E23Q001259010200013Q00203C0102000200020012C3000300034Q00B50002000200020030C70002000400050030C70002000600072Q004A01035Q0010150102000800030012C3000300094Q004A010400013Q0012C30005000A4Q002D00063Q00090030C700060004000B0012590107000D3Q00200E01070007000200122Q0008000E3Q00122Q0009000E6Q00070009000200102Q0006000C000700122Q000700103Q00202Q00070007000200122Q0008000E3Q00122Q000900093Q00122Q000A000E3Q00122Q000B00096Q0007000B000200102Q0006000F0007001259010700103Q00205B00070007001100122Q000800123Q00122Q000900136Q00070009000200102Q0006000600072Q004A010700023Q00203C0107000700150010150106001400070030C70006001600170030C70006001800090030C700060019001A2Q004A010700033Q0010150106000800072Q00390004000600022Q004A010500044Q00D0000600043Q00122Q0007001B6Q0005000700014Q000500056Q000600046Q000700023Q00202Q00070007001C00122Q0008000E6Q0005000800014Q000500064Q00A8000600043Q0012C30007001D3Q0012730108000E6Q0005000800014Q000500013Q00122Q0006001E6Q00073Q000800302Q0007001F00204Q000800073Q00102Q0007002100084Q000800023Q00202Q00080008001F0010150107002200080030C7000700230024001259010800103Q00204700080008000200122Q000900093Q00122Q000A00093Q00122Q000B00093Q00122Q000C00256Q0008000C00020010150107000F0008001259010800103Q00204700080008000200122Q000900263Q00122Q000A00093Q00122Q000B00093Q00122Q000C00076Q0008000C000200106501070006000800302Q00070016002600102Q0007000800044Q0005000700024Q000600013Q00122Q0007000A6Q00083Q00064Q000900023Q00202Q00090009002700102Q0008001400090012590109000D3Q00200E01090009000200122Q000A000E3Q00122Q000B00096Q0009000B000200102Q0008000C000900122Q000900103Q00202Q00090009000200122Q000A000E3Q00122Q000B00093Q00122Q000C00093Q00122Q000D00286Q0009000D000200102Q0008000F0009001259010900103Q00204700090009000200122Q000A00093Q00122Q000B00293Q00122Q000C00093Q00122Q000D002A6Q0009000D000200101D00080006000900302Q00080018000900102Q0008000800044Q0006000800024Q000700046Q000800063Q00122Q0009002B6Q0007000900014Q000700056Q000800064Q004A010900023Q00201E00090009001C4Q000A00086Q0007000A00024Q000800013Q00122Q0009002C6Q000A3Q000B00302Q000A0016002600302Q000A001F002D00302Q000A002E002F4Q000B00023Q00203C010B000B0031001020000A0030000B4Q000B00093Q00102Q000A0021000B4Q000B00023Q00202Q000B000B001F00102Q000A0022000B00302Q000A0023002500302Q000A0032003300122Q000B00103Q002047000B000B000200122Q000C00263Q00122Q000D00343Q00122Q000E00263Q00122Q000F00096Q000B000F0002001015010A0006000B001259010B00103Q002047000B000B000200122Q000C00093Q00122Q000D00353Q00122Q000E00093Q00122Q000F00096Q000B000F0002001015010A000F000B001015010A000800062Q00390008000A00022Q004A010900013Q00129F010A00366Q000B3Q000A00302Q000B001F00374Q000C00073Q00102Q000B0021000C4Q000C00023Q00202Q000C000C001F00102Q000B0022000C00302Q000B002300254Q000C00023Q00202Q000C000C003800102Q000B0014000C00122Q000C000D3Q00202Q000C000C000200122Q000D000E3Q00122Q000E00096Q000C000E000200102Q000B000C000C00122Q000C00103Q00202Q000C000C000200122Q000D000E3Q00122Q000E00093Q00122Q000F00093Q00122Q001000396Q000C0010000200102Q000B000F000C00122Q000C00103Q00202Q000C000C000200122Q000D00093Q00122Q000E00293Q00122Q000F00093Q00122Q0010002A6Q000C0010000200102Q000B0006000C00302Q000B003A003300102Q000B000800044Q0009000B00024Q000A00046Q000B00093Q00122Q000C002B6Q000A000C00010006A6010A3Q0001000B2Q00A83Q00084Q00A88Q00A83Q00024Q004A012Q000A4Q00A83Q00044Q004A012Q000B4Q00A83Q00014Q00A83Q00034Q004A012Q000C4Q00A83Q00074Q004A012Q00023Q002033000B0009003B00202Q000B000B003C4Q000D000A6Q000B000D00014Q000B000D6Q000C00046Q000D00016Q000E00056Q000D000100012Q003B010B000D00012Q00BD3Q00013Q00013Q00173Q0003043Q005465787403073Q0044657374726F7903043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240028Q0003043Q007461736B03043Q0077616974029A5Q99C93F026Q00F03F026Q00084003043Q004B69636B030B3Q00496E76616C6964204B6579034Q00030F3Q00506C616365686F6C6465725465787403063Q00737472696E6703063Q00666F726D617403133Q00496E76616C6964204B657921202825642F332903053Q00436F6C6F7203073Q005761726E696E6703053Q0064656C6179026Q00F83F004A4Q004A016Q00203C014Q00012Q004A2Q0100013Q00067D012Q001D0001000100047B012Q001D00012Q004A012Q00023Q0020285Q00026Q000200016Q00036Q000100046Q000200056Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300046Q0003000100124Q00083Q00206Q000900122Q0001000A8Q000200016Q00043Q00206Q00026Q000200016Q00068Q0001000100044Q004900012Q004A012Q00073Q0020D85Q000B2Q004A3Q00074Q004A012Q00073Q000E2F010C002E00013Q00047B012Q002E00012Q004A012Q00023Q00062C012Q002900013Q00047B012Q002900012Q004A012Q00023Q0020135Q00022Q00A5012Q000200012Q004A012Q00083Q0020135Q000D0012C30002000E4Q003B012Q000200012Q00BD3Q00014Q004A016Q0030C73Q0001000F2Q004A016Q0012592Q0100113Q00203C2Q01000100120012C3000200134Q004A010300074Q0039000100030002001015012Q001000012Q00E53Q00036Q000100096Q000200056Q00033Q00014Q0004000A3Q00202Q00040004001500102Q0003001400046Q0003000100124Q00083Q00206Q001600122Q000100173Q0006A601023Q000100052Q004A017Q004A012Q00034Q004A012Q00094Q004A012Q00054Q004A012Q000A4Q003B012Q000200012Q00BD3Q00013Q00013Q00043Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03053Q00436F6C6F7203063Q005374726F6B65000B4Q004A016Q0030C73Q000100022Q00643Q00016Q000100026Q000200036Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00613Q0003063Q00412Q63656E7403043Q005461627303083Q005F63752Q72656E740003053Q004672616D6503043Q004E616D6503063Q0057696E646F77030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407A40026Q00744003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03103Q00436C69707344657363656E64616E74732Q0103063Q0041637469766503063Q00506172656E74026Q00184003063Q005374726F6B65026Q00304003063Q00546F7042617203093Q005365636F6E64617279026Q00F03F026Q00414003063Q005A496E646578026Q001440026Q0020400285EB51B81E85EB3F03043Q006D2Q6F6E026Q00324003063Q00507572706C6503093Q00546578744C6162656C03053Q005469746C6503043Q0054657874030C3Q0056656C6F636974792048756203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q002Q40026Q0059C0026Q00344003013Q0078026Q0020C003063Q00436F6C6F723303073Q0066726F6D524742025Q00E06F40025Q0080564003053Q006D696E7573026Q0041C003083Q006B6579626F617264026Q004EC003093Q0041637469766174656403073Q00436F2Q6E65637403043Q00426F6479030E3Q005363726F2Q6C696E674672616D6503073Q005461624C697374029A5Q99C93F025Q00C05C40030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q7303103Q005363726F2Q6C696E67456E61626C6564030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D026Q00084003093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E67526967687403073Q00436F6E74656E74026Q005D40026Q005DC0030B3Q005369646544697669646572027Q0040030A3Q00496E707574426567616E03093Q00437265617465546162029C012Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100023Q00203C01020001000100062C0102000A00013Q00047B012Q000A00012Q004A01025Q00203C0103000100010010150102000100032Q002D00023Q00022Q00FE00035Q00102Q00020002000300302Q0002000300044Q000300013Q00122Q000400056Q00053Q000A00302Q00050006000700122Q000600093Q00200E01060006000A00122Q0007000B3Q00122Q0008000B6Q00060008000200102Q00050008000600122Q0006000D3Q00202Q00060006000A00122Q0007000B3Q00122Q0008000E3Q00122Q0009000B3Q00122Q000A000E6Q0006000A000200102Q0005000C00060012590106000D3Q00205B00060006001000122Q000700113Q00122Q000800126Q00060008000200102Q0005000F00062Q004C00065Q00202Q00060006001400102Q00050013000600302Q00050015001600302Q00050017000E00302Q00050018001900302Q0005001A00194Q000600023Q00102Q0005001B00064Q0003000500022Q004A010400034Q00D0000500033Q00122Q0006001C6Q0004000600014Q000400046Q000500036Q00065Q00202Q00060006001D00122Q0007000B6Q0004000700014Q000400054Q00A8000500033Q0012C30006001E3Q0012C30007000B4Q003B0104000700012Q004A010400013Q0012C3000500054Q002D00063Q00080030C700060006001F2Q004A01075Q00203C0107000700200010150106001300070030C70006001500160012590107000D3Q00204700070007000A00122Q000800213Q00122Q0009000E3Q00122Q000A000E3Q00122Q000B00226Q0007000B00020010460006000F000700302Q00060017000E00302Q00060023002400302Q0006001A001900102Q0006001B00034Q0004000600024Q000500046Q000600046Q00075Q00202Q00070007001D2Q004A010800064Q003B0105000800012Q004A010500054Q00A8000600043Q0012C3000700253Q0012C3000800264Q003B0105000800012Q004A010500073Q0012C3000600273Q0012C3000700284Q0080010800014Q004A01095Q00203C0109000900292Q003900050009000200121F010600093Q00202Q00060006000A00122Q0007000E3Q00122Q0008000B6Q00060008000200102Q00050008000600122Q0006000D3Q00202Q00060006000A00122Q0007000E3Q00122Q000800253Q00122Q0009000B3Q00122Q000A000E6Q0006000A000200102Q0005000C000600102Q0005001B00044Q000600013Q00122Q0007002A6Q00083Q000C00302Q00080006002B00202Q00090001002B00062Q0009007D0001000100047B012Q007D00010012C30009002D3Q0010150108002C00092Q00B9000900083Q00102Q0008002E00094Q00095Q00202Q00090009002C00102Q0008002F000900302Q00080030003100122Q000900333Q00202Q00090009003200202Q00090009003400102Q000800320009001259010900333Q00205B01090009003500202Q00090009003600102Q00080035000900302Q00080015002100122Q000900093Q00200E01090009000A00122Q000A000E3Q00122Q000B000B6Q0009000B000200102Q00080008000900122Q0009000D3Q00202Q00090009000A00122Q000A000E3Q00122Q000B00373Q00122Q000C000B3Q00122Q000D000E6Q0009000D000200102Q0008000C00090012590109000D3Q00204700090009000A00122Q000A00213Q00122Q000B00383Q00122Q000C000E3Q00122Q000D00396Q0009000D00020010150108000F00090010150108001B00042Q00390006000800020006A601073Q000100072Q004A012Q00014Q004A017Q00A83Q00044Q004A012Q00034Q004A012Q00074Q004A012Q00094Q004A012Q000A4Q0014010800073Q00122Q0009003A3Q00122Q000A003B3Q00122Q000B003C3Q00202Q000B000B003D00122Q000C003E3Q00122Q000D003F3Q00122Q000E003F6Q000B000E6Q00083Q00022Q00A8000900073Q0012C3000A00403Q0012C3000B00414Q004A010C5Q00203C010C000C002C2Q00390009000C00022Q00A8000A00073Q0012C3000B00423Q0012C3000C00434Q004A010D5Q00203C010D000D00292Q0039000A000D00022Q0080010B5Q00203C010C000A0044002013000C000C00450006A6010E00010001000D2Q004A012Q00024Q00A83Q000B4Q004A012Q00014Q004A017Q004A012Q00034Q004A012Q00044Q004A012Q00054Q004A012Q00084Q004A012Q000B4Q004A012Q000C4Q004A012Q00094Q004A012Q000A4Q004A012Q000D4Q003B010C000E00012Q004A010C00013Q0012C3000D00054Q002D000E3Q00050030C7000E000600460030C7000E00150021001259010F000D3Q002047000F000F000A00122Q0010000E3Q00122Q0011000E3Q00122Q0012000E3Q00122Q001300226Q000F00130002001015010E000C000F001259010F000D3Q002047000F000F000A00122Q001000213Q00122Q0011000E3Q00122Q001200213Q00122Q001300416Q000F001300020010FA000E000F000F00102Q000E001B00034Q000C000E00024Q000D00013Q00122Q000E00476Q000F3Q000B00302Q000F000600484Q00105Q00202Q00100010002000102Q000F001300100030C7000F001500490030C7000F0017000E0012590110000D3Q00204700100010000A00122Q0011000E3Q00122Q0012004A3Q00122Q001300213Q00122Q0014000E6Q001000140002001015010F000F00100012590110000D3Q00203C01100010000A2Q00FD001000010002001015010F004B0010001259011000333Q00203C01100010004D00203C01100010004E001015010F004C00100030C7000F004F000E0030C7000F005000190030C7000F001A0019001015010F001B000C2Q002D001000014Q004A011100013Q0012C3001200514Q002D00133Q0002001259011400533Q00205B00140014000A00122Q0015000E3Q00122Q001600546Q00140016000200102Q001300520014001288011400333Q00202Q00140014005500202Q00140014005600102Q0013005500144Q0011001300024Q001200013Q00122Q001300576Q00143Q000300122Q001500533Q00205B00150015000A00122Q0016000E3Q00122Q0017001C6Q00150017000200102Q001400580015001259011500533Q00205B00150015000A00122Q0016000E3Q00122Q0017001C6Q00150017000200102Q001400590015001259011500533Q00205B00150015000A00122Q0016000E3Q00122Q0017001C6Q00150017000200102Q0014005A00152Q0099011200144Q000D01103Q00012Q0039000D001000022Q0071010E00013Q00122Q000F00056Q00103Q000500302Q00100006005B00302Q00100015002100122Q0011000D3Q00202Q00110011000A00122Q0012000E3Q00122Q0013005C3Q00122Q0014000E3Q00122Q0015000E6Q00110015000200102Q0010000C001100122Q0011000D3Q00202Q00110011000A00122Q001200213Q00122Q0013005D3Q00122Q001400213Q00122Q0015000E6Q00110015000200102Q0010000F001100102Q0010001B000C4Q000E001000024Q000F00013Q00122Q001000056Q00113Q000800302Q00110006005E4Q00125Q00202Q00120012001D00102Q0011001300124Q001200063Q00102Q00110015001200302Q00110017000E00122Q0012000D3Q00202Q00120012000A00122Q0013000E3Q00122Q0014004A3Q00122Q0015000E3Q00122Q0016000E6Q00120016000200102Q0011000C001200122Q0012000D3Q00202Q00120012000A00122Q0013000E3Q00122Q001400213Q00122Q001500213Q00122Q0016000E6Q00120016000200102Q0011000F001200302Q00110023005F00102Q0011001B000C4Q000F001100014Q000F000D6Q001000036Q001100036Q001200046Q001300066Q001400056Q0011000300012Q003B010F0011000100203C010F00080044002013000F000F00450006A601110002000100052Q004A012Q000E4Q004A012Q00094Q00A83Q00034Q004A012Q000A4Q004A012Q00024Q003B010F001100012Q0080010F5Q00203C0110000900440020130010001000450006A601120003000100052Q00A83Q000F4Q00A83Q000C4Q004A012Q00094Q00A83Q00034Q004A012Q000F4Q00960110001200014Q00108Q0011000B6Q0012000C3Q00202Q0012001200600006A601130004000100032Q00A83Q000B4Q00A83Q00104Q00A83Q00034Q003B0111001300010006A601110005000100102Q004A012Q00014Q004A017Q00A83Q000D4Q004A012Q00034Q004A012Q00074Q004A012Q00104Q00A83Q000E4Q00A83Q00024Q004A012Q00094Q004A012Q000F4Q004A012Q000A4Q004A012Q00044Q004A012Q00064Q004A012Q00084Q004A012Q00114Q004A012Q00123Q0010150102006100112Q00A2000200024Q00BD3Q00013Q00063Q001C3Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574026Q00364003063Q005A496E646578026Q00184003063Q00506172656E74026Q001440026Q002A4003073Q0053756254657874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503534Q003301035Q00122Q000400016Q00053Q000900302Q00050002000300302Q0005000400054Q000600013Q00202Q00060006000700102Q00050006000600302Q00050008000900122Q0006000B3Q00202Q00060006000C00122Q000700093Q00122Q0008000D6Q00060008000200102Q0005000A000600122Q0006000F3Q00202Q00060006000C00122Q000700096Q000800013Q00122Q0009000D3Q00122Q000A00106Q0006000A000200102Q0005000E000600122Q0006000F3Q00202Q00060006001200122Q000700133Q00122Q000800136Q00060008000200102Q00050011000600302Q0005001400154Q000600023Q00102Q0005001600064Q0003000500024Q000400036Q000500033Q00122Q000600176Q0004000600014Q000400046Q00055Q00122Q000600186Q00078Q000800013Q00202Q0008000800194Q00040008000200122Q0005000B3Q00202Q00050005000C00122Q0006000D3Q00122Q0007000D6Q00050007000200102Q0004000A000500122Q0005000F3Q00202Q00050005000C00122Q0006000D3Q00122Q000700103Q00122Q0008000D3Q00122Q000900106Q00050009000200102Q0004000E000500302Q00040014001500102Q00040016000300202Q00050003001A00202Q00050005001B0006A601073Q000100062Q004A012Q00054Q00A83Q00034Q004A012Q00064Q00A83Q00044Q00A83Q00024Q004A012Q00014Q003B01050007000100203C01050003001C00201300050005001B0006A601070001000100052Q004A012Q00054Q00A83Q00034Q004A012Q00064Q00A83Q00044Q004A012Q00014Q000A0005000700014Q000500036Q000600046Q000500038Q00013Q00023Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379028Q00030A3Q0054657874436F6C6F723303043Q005465787400124Q004A017Q0005000100016Q000200026Q00033Q000100302Q0003000100026Q000300012Q004A017Q004A2Q0100034Q004A010200024Q002D00033Q00012Q004A010400043Q0006E70004000F0001000100047B012Q000F00012Q004A010400053Q00203C0104000400040010150103000300042Q003B012Q000300012Q00BD3Q00017Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030A3Q0054657874436F6C6F723303073Q0053756254657874000F4Q00DB9Q00000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00463Q00030E3Q0046696E6446697273744368696C6403143Q004B657962696E645069636B65724F7665726C617903073Q0044657374726F7903053Q004672616D6503043Q004E616D65030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407040025Q00805B4003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E64030F3Q00426F7264657253697A65506978656C03063Q005A496E646578026Q00594003063Q004163746976652Q0103063Q00506172656E74026Q00204003063Q00507572706C65029A5Q99C93F026Q00F83F026Q003440029A5Q99D93F03093Q00546578744C6162656C03043Q005465787403173Q00437573746F6D204B657962696E642053652Q74696E677303083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40026Q002440026Q00F03F026Q00324003163Q004261636B67726F756E645472616E73706172656E6379025Q0040594003093Q005365636F6E64617279026Q004240025Q00806B40026Q003E40026Q001440030A3Q0043552Q52454E543A205B03063Q00737472696E6703053Q00752Q70657203073Q0067657467656E7603093Q00546F2Q676C654B657903123Q005D0A5052452Q5320414E59204B45593Q2E026Q002640030E3Q005465787458416C69676E6D656E7403043Q00456E756D03063Q0043656E746572025Q00805940030A3Q005465787442752Q746F6E03063Q0043616E63656C03073Q005375625465787403073Q00456C656D656E74025Q00805240026Q003640030F3Q004175746F42752Q746F6E436F6C6F720100026Q00104003093Q0041637469766174656403073Q00436F2Q6E656374030A3Q00496E707574426567616E00F14Q00967Q00206Q000100122Q000200028Q0002000200064Q000B00013Q00047B012Q000B000100201300013Q00032Q00A52Q01000200012Q00802Q016Q004A000100014Q00BD3Q00014Q00802Q0100014Q0085000100016Q000100023Q00122Q000200046Q00033Q000900302Q00030005000200122Q000400073Q00200E01040004000800122Q000500093Q00122Q000600096Q00040006000200102Q00030006000400122Q0004000B3Q00202Q00040004000800122Q000500093Q00122Q0006000C3Q00122Q000700093Q00122Q0008000C6Q00040008000200102Q0003000A00040012590104000B3Q00205B00040004000E00122Q0005000F3Q00122Q000600106Q00040006000200102Q0003000D00042Q004A010400033Q00203C0104000400120010150103001100040030C700030013000C0030C70003001400150030C70003001600172Q004A01045Q0010150103001800042Q00390001000300022Q004A010200044Q00A8000300013Q001217010400196Q0002000400014Q000200056Q000300016Q000400033Q00202Q00040004001A00122Q0005001B3Q00122Q0006001C6Q0002000600014Q000200064Q00A8000300013Q0012C30004001D3Q0012730105001E6Q0002000500014Q000200023Q00122Q0003001F6Q00043Q000900302Q0004002000214Q000500073Q00102Q0004002200054Q000500033Q00202Q0005000500200010150104002300050030C70004002400250012590105000B3Q00204700050005000800122Q0006000C3Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900266Q0005000900020010150104000A00050012590105000B3Q00204700050005000800122Q000600273Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900286Q0005000900020010900004000D000500302Q00040029002700302Q00040014002A00102Q0004001800014Q0002000400024Q000300023Q00122Q000400046Q00053Q00074Q000600033Q00202Q00060006002B001015010500110006001259010600073Q00200E01060006000800122Q000700093Q00122Q0008000C6Q00060008000200102Q00050006000600122Q0006000B3Q00202Q00060006000800122Q000700093Q00122Q0008000C3Q00122Q0009000C3Q00122Q000A002C6Q0006000A000200102Q0005000A00060012590106000B3Q00204700060006000800122Q0007000C3Q00122Q0008002D3Q00122Q0009000C3Q00122Q000A002E6Q0006000A00020010150105000D00060030C700050013000C0030C700050014002A0010150105001800012Q00390003000500022Q004A010400044Q0049000500033Q00122Q0006002F6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006001A00122Q000700096Q0004000700024Q000500023Q0012C30006001F4Q002D00073Q00090012C3000800303Q001259010900313Q00203C010900090032001259010A00334Q003E010A0001000200202Q000A000A003400202Q000A000A00054Q00090002000200122Q000A00356Q00080008000A00102Q0007002000084Q000800073Q00102Q0007002200084Q000800033Q0020A901080008001A00102Q00070023000800302Q00070024003600122Q000800383Q00202Q00080008003700202Q00080008003900102Q00070037000800122Q0008000B3Q00204700080008000800122Q000900273Q00122Q000A000C3Q00122Q000B00273Q00122Q000C000C6Q0008000C00020010150107000D00080030C70007002900270030C700070014003A0010150107001800032Q00390005000700022Q004A010600023Q0012C30007003B4Q002D00083Q000B0030C700080020003C2Q00A3000900073Q00102Q0008002200094Q000900033Q00202Q00090009003D00102Q00080023000900302Q0008002400364Q000900033Q00202Q00090009003E00102Q00080011000900122Q000900073Q00202Q00090009000800122Q000A00093Q00122Q000B000C6Q0009000B000200102Q00080006000900122Q0009000B3Q00202Q00090009000800122Q000A00093Q00122Q000B000C3Q00122Q000C000C3Q00122Q000D003F6Q0009000D000200102Q0008000A000900122Q0009000B3Q00202Q00090009000800122Q000A000C3Q00122Q000B00153Q00122Q000C000C3Q00122Q000D00406Q0009000D000200102Q0008000D000900302Q00080041004200302Q00080014002A00102Q0008001800014Q0006000800024Q000700046Q000800063Q00122Q000900436Q0007000900014Q000700073Q0006A601083Q000100032Q00A83Q00074Q00A83Q00014Q004A012Q00013Q0020EE00090006004400202Q0009000900454Q000B00086Q0009000B00014Q000900086Q000A00093Q00202Q000A000A00460006A6010B0001000100062Q004A012Q00014Q00A83Q00084Q00A83Q00054Q004A012Q000A4Q00A83Q00044Q004A012Q000B4Q00390009000B00022Q00A8000700094Q004A0109000C4Q00A8000A00014Q002D000B00014Q00A8000C00024Q004C010B000100012Q003B0109000B00012Q00BD3Q00013Q00023Q00023Q00030A3Q00446973636F2Q6E65637403073Q0044657374726F7900114Q004A016Q00062C012Q000800013Q00047B012Q000800012Q004A016Q0020135Q00012Q00A5012Q000200012Q0079017Q004A8Q004A012Q00013Q00062C012Q000E00013Q00047B012Q000E00012Q004A012Q00013Q0020135Q00022Q00A5012Q000200012Q0080017Q004A3Q00024Q00BD3Q00017Q00163Q00030D3Q0055736572496E7075745479706503043Q00456E756D03083Q004B6579626F61726403073Q0067657467656E7603093Q00546F2Q676C654B657903073Q004B6579436F646503043Q0054657874030E3Q004E4557204B455942494E443A205B03063Q00737472696E6703053Q00752Q70657203043Q004E616D6503013Q005D030A3Q0054657874436F6C6F723303063Q00436F6C6F723303073Q0066726F6D524742026Q005E40025Q00E06F40026Q00644003053Q00436F6C6F7203043Q007461736B03043Q0077616974026Q00E03F02354Q004A01025Q0006E7000200060001000100047B012Q000600012Q004A010200014Q00F80002000100012Q00BD3Q00013Q00203C01023Q0001001259010300023Q00203C01030003000100203C01030003000300067D010200340001000300047B012Q00340001001259010200044Q005A01020001000200202Q00033Q000600102Q0002000500034Q000200023Q00122Q000300083Q00122Q000400093Q00202Q00040004000A00202Q00053Q000600202Q00050005000B4Q0004000200020012C30005000C4Q004501030003000500102Q0002000700034Q000200023Q00122Q0003000E3Q00202Q00030003000F00122Q000400103Q00122Q000500113Q00122Q000600126Q00030006000200102Q0002000D00032Q004A010200034Q0060010300046Q000400056Q00053Q000100122Q0006000E3Q00202Q00060006000F00122Q000700103Q00122Q000800113Q00122Q000900126Q00060009000200102Q0005001300062Q003B010200050001001259010200143Q00203C0102000200150012C3000300164Q00A50102000200012Q004A010200014Q00F80002000100012Q00BD3Q00017Q000A3Q0003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574030C3Q004162736F6C75746553697A6503013Q0058028Q0003043Q007461736B03043Q0077616974020AD7A3703D0AC73F03073Q0044657374726F7900184Q00789Q00000100018Q000200016Q00016Q000100026Q000200036Q00033Q000100122Q000400023Q00202Q0004000400034Q000500023Q00203C01050005000400203C0105000500050012C3000600064Q00390004000600020010150103000100042Q003B012Q00030001001259012Q00073Q00203C014Q00080012C3000100094Q00A5012Q000200012Q004A012Q00043Q0020135Q000A2Q00A5012Q000200012Q00BD3Q00017Q000C3Q0003073Q0056697369626C65010003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00407A40026Q004140026Q00744003043Q007461736B03043Q007761697402B81E85EB51B8BE3F2Q0100264Q004A019Q007Q004A8Q004A016Q00062C012Q001400013Q00047B012Q001400012Q004A012Q00013Q0030C73Q000100022Q004A012Q00024Q004A2Q0100034Q004A010200044Q002D00033Q0001001259010400043Q00205B00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300042Q003B012Q0003000100047B012Q002500012Q004A012Q00024Q006D2Q0100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600086Q00040006000200102Q0003000300046Q0003000100124Q00093Q00206Q000A00122Q0001000B8Q000200016Q00013Q00304Q0001000C2Q00BD3Q00017Q00063Q0003073Q004B6579436F646503073Q0067657467656E7603093Q00546F2Q676C654B657903043Q00456E756D03013Q004B03073Q0056697369626C6502193Q0006E7000100050001000100047B012Q000500012Q004A01025Q00062C0102000600013Q00047B012Q000600012Q00BD3Q00013Q00203C01023Q0001001259010300024Q00FD00030001000200203C0103000300030006E70003000F0001000100047B012Q000F0001001259010300043Q00203C01030003000100203C01030003000500067D010200180001000300047B012Q001800012Q004A010200014Q0083010200026Q000200016Q000200026Q000300016Q000300033Q00102Q0002000600032Q00BD3Q00017Q00593Q0003063Q005F6F72646572028Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577026Q003C4003073Q0056697369626C6503063Q0048692Q64656E03063Q00506172656E74026Q00144003093Q0049636F6E436F6C6F7203073Q005375625465787403043Q0049636F6E03063Q00636972636C65026Q002E40030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q00184003093Q00546578744C6162656C03043Q004E616D652Q033Q0054616203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q003A40026Q003EC003053Q004672616D6503043Q0050616765030F3Q00426F7264657253697A65506978656C030E3Q005363726F2Q6C696E674672616D65030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q73027Q004003143Q005363726F2Q6C426172496D616765436F6C6F723303063Q005374726F6B65031A3Q005363726F2Q6C426172496D6167655472616E73706172656E6379030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D03093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70026Q002040030D3Q0050612Q64696E67426F2Q746F6D030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E675269676874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403043Q005F62746E03053Q005F69636F6E03053Q005F6E616D6503053Q005F7061676503053Q005F7772617003073Q005F73656C656374030A3Q005F69636F6E436F6C6F7203053Q007461626C6503063Q00696E7365727403043Q0054616273030D3Q0043726561746553656374696F6E030C3Q00437265617465546F2Q676C65030C3Q00437265617465536C69646572030D3Q0043726561746554657874626F78030B3Q004372656174654C6162656C030C3Q0043726561746542752Q746F6E03123Q00437265617465436F726E657242752Q746F6E030C3Q00437265617465427579526F770256012Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100024Q002D00023Q00010030D40002000100024Q00035Q00122Q000400036Q00053Q000700302Q00050004000500302Q0005000600074Q000600013Q00202Q00060006000900102Q00050008000600302Q0005000A000B00122Q0006000D3Q00202Q00060006000E00122Q0007000B3Q00122Q000800023Q00122Q000900023Q00122Q000A000F6Q0006000A000200102Q0005000C000600202Q0006000100114Q000600063Q00102Q0005001000064Q000600023Q00102Q0005001200064Q0003000500024Q000400036Q000500033Q00122Q000600136Q00040006000100202Q00040001001400062Q000400260001000100047B012Q002600012Q004A010400013Q00203C0104000400152Q004A010500043Q00203C0106000100160006E70006002B0001000100047B012Q002B00010012C3000600173Q0012C3000700184Q006C01088Q000900046Q00050009000200122Q0006001A3Q00202Q00060006000E00122Q000700023Q00122Q0008001B6Q00060008000200102Q00050019000600122Q0006000D3Q00202Q00060006000E00122Q000700023Q00122Q0008001D3Q00122Q0009001B3Q00122Q000A00026Q0006000A000200102Q0005001C000600102Q0005001200034Q00065Q00122Q0007001E6Q00083Q000B00302Q0008000A000B00202Q00090001001F00062Q000900460001000100047B012Q004600010012C3000900203Q0010150108000400092Q003F010900053Q00102Q0008002100094Q000900013Q00202Q00090009001500102Q00080022000900302Q00080023002400122Q000900263Q00202Q00090009002500202Q00090009002700102Q00080025000900122Q000900263Q00202Q00090009002800202Q00090009002900102Q00080028000900122Q0009001A3Q00202Q00090009000E00122Q000A00023Q00122Q000B001B6Q0009000B000200102Q00080019000900122Q0009000D3Q00202Q00090009000E00122Q000A00023Q00122Q000B002A3Q00122Q000C001B3Q00122Q000D00026Q0009000D000200102Q0008001C000900122Q0009000D3Q00202Q00090009000E00122Q000A000B3Q00122Q000B002B3Q00122Q000C000B3Q00122Q000D00026Q0009000D000200102Q0008000C000900102Q0008001200034Q0006000800024Q00075Q00122Q0008002C6Q00093Q000600302Q0009001F002D00302Q0009000A000B00302Q0009002E000200122Q000A000D3Q00202Q000A000A000E00122Q000B000B3Q00122Q000C00023Q00122Q000D000B3Q00122Q000E00026Q000A000E000200102Q0009000C000A00302Q0009001000074Q000A00063Q00102Q00090012000A4Q0007000900024Q00085Q00122Q0009002F6Q000A3Q000900302Q000A000A000B00302Q000A002E000200122Q000B000D3Q00202Q000B000B000E00122Q000C000B3Q00122Q000D00023Q00122Q000E000B3Q00122Q000F00026Q000B000F000200102Q000A000C000B00122Q000B000D3Q00202Q000B000B000E4Q000B0001000200102Q000A0030000B00122Q000B00263Q00202Q000B000B003200202Q000B000B003300102Q000A0031000B00302Q000A003400354Q000B00013Q00202Q000B000B0037001015010A0036000B0030C7000A0038001B001015010A001200072Q002D000B00014Q004A010C5Q0012C3000D00394Q002D000E3Q0002001259010F003B3Q00205B000F000F000E00122Q001000023Q00122Q0011001D6Q000F0011000200102Q000E003A000F001288010F00263Q00202Q000F000F003C00202Q000F000F003D00102Q000E003C000F4Q000C000E00024Q000D5Q00122Q000E003E6Q000F3Q000400122Q0010003B3Q00205B00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F003F00100012590110003B3Q00205B00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F004100100012590110003B3Q00205B00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F004200100012590110003B3Q00205B00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F004300102Q0099010D000F4Q000D010B3Q00012Q00390008000B00020006A601093Q0001000B2Q004A012Q00074Q00A83Q00024Q00A83Q00014Q00A83Q00074Q004A012Q00084Q004A012Q00094Q004A012Q000A4Q004A012Q00014Q00A83Q00034Q00A83Q00054Q00A83Q00063Q00203C010A00030044002013000A000A00450006A6010C0001000100042Q00A83Q00074Q004A012Q00084Q00A83Q00034Q004A012Q000A4Q003B010A000C000100203C010A00030046002013000A000A00450006A6010C0002000100042Q00A83Q00074Q004A012Q00084Q00A83Q00034Q004A012Q000A4Q009C010A000C000100202Q000A0003004700202Q000A000A00454Q000C00096Q000A000C00014Q000A00036Q000B00056Q000C00066Q000D00086Q000E00073Q00102Q0002004D000900102Q0002004C000E00102Q0002004B000D00102Q0002004A000C00102Q00020049000B00102Q00020048000A00102Q0002004E000400122Q000A004F3Q00202Q000A000A00504Q000B00073Q00202Q000B000B00514Q000C00026Q000A000C00014Q000A00073Q00202Q000A000A00514Q000A000A3Q00262Q000A2Q002Q01000B00047B013Q002Q012Q00A8000A00094Q00F8000A000100010006A6010A0003000100072Q00A83Q00024Q004A017Q004A012Q00014Q00A83Q00084Q004A012Q00034Q004A012Q000B4Q004A012Q000C3Q0006A6010B0004000100062Q00A83Q00024Q004A017Q00A83Q00084Q004A012Q00044Q004A012Q00014Q004A012Q000D3Q00101501020052000B0006A6010B0005000100082Q00A83Q000A4Q004A017Q004A012Q00044Q004A012Q00014Q004A012Q00054Q004A012Q00034Q004A012Q00084Q004A012Q000A3Q00101501020053000B0006A6010B0006000100072Q00A83Q000A4Q004A017Q004A012Q00054Q004A012Q00014Q004A012Q000D4Q004A012Q00034Q004A012Q000E3Q00101501020054000B0006A6010B0007000100092Q00A83Q000A4Q004A017Q004A012Q00054Q004A012Q00014Q004A012Q00034Q004A012Q000B4Q004A012Q000C4Q004A012Q00084Q004A012Q000A3Q00101501020055000B0006A6010B0008000100062Q00A83Q000A4Q004A012Q00044Q004A012Q00014Q004A017Q004A012Q00054Q004A012Q000D3Q00101501020056000B0006A6010B0009000100072Q00A83Q000A4Q004A017Q004A012Q00044Q004A012Q00014Q004A012Q000D4Q004A012Q00084Q004A012Q000A3Q00101501020057000B0006A6010B000A0001000A2Q00A83Q00084Q004A017Q004A012Q00014Q00A83Q00074Q004A012Q00034Q004A012Q000B4Q004A012Q00044Q004A012Q000D4Q004A012Q00084Q004A012Q000A3Q00101501020058000B0006A6010B000B000100072Q00A83Q000A4Q004A017Q004A012Q00054Q004A012Q00014Q004A012Q000D4Q004A012Q00034Q004A012Q000F3Q00101501020059000B2Q00A2000200024Q00BD3Q00013Q000C3Q00193Q0003083Q005F63752Q72656E7403063Q0048692Q64656E03063Q0069706169727303043Q005461627303053Q005F7772617003073Q0056697369626C6501002Q0103083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577027B14AE47E17AA43F028Q0003043Q005F62746E03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03053Q005F69636F6E030A3Q0054657874436F6C6F7233030A3Q005F69636F6E436F6C6F7203073Q005375625465787403053Q005F6E616D6503113Q0053656C656374656449636F6E436F6C6F7203093Q0049636F6E436F6C6F7203063Q00412Q63656E7403043Q005465787400914Q004A016Q00203C014Q00012Q004A2Q0100013Q00067D012Q00060001000100047B012Q000600012Q00BD3Q00014Q004A012Q00023Q00203C014Q000200062C012Q003200013Q00047B012Q00320001001259012Q00034Q004A2Q015Q00203C2Q01000100042Q00313Q0002000200047B012Q001400012Q004A010500013Q000661000400140001000500047B012Q0014000100203C0105000400050030C70005000600070006F73Q000F0001000200047B012Q000F00012Q004A017Q005F000100013Q00104Q000100016Q00033Q00304Q000600086Q00033Q00122Q0001000A3Q00204700010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q00010005000200104E3Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00204700040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q0004000800020010150103000900042Q003B012Q000300012Q00BD3Q00013Q001259012Q00034Q004A2Q015Q00203C2Q01000100042Q00313Q0002000200047B012Q005500012Q004A010500013Q0006610004003C0001000500047B012Q003C000100203C0105000400050030C70005000600072Q004A010500043Q00203C01060004000E2Q004A010700064Q002D00083Q00010030C70008000F00102Q003B0105000800012Q004A010500043Q00203C0106000400112Q004A010700064Q002D00083Q000100203C0109000400130006E70009004B0001000100047B012Q004B00012Q004A010900073Q00203C0109000900140010150108001200092Q00EC0005000800014Q000500043Q00202Q0006000400154Q000700066Q00083Q00014Q000900073Q00202Q00090009001400102Q0008001200094Q0005000800010006F73Q00370001000200047B012Q003700012Q004A017Q005F000100013Q00104Q000100016Q00033Q00304Q000600086Q00033Q00122Q0001000A3Q00204700010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q00010005000200104E3Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00204700040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q0004000800020010150103000900042Q003B012Q000300012Q004A012Q00044Q0005000100086Q000200066Q00033Q000100302Q0003000F000D6Q000300012Q0067012Q00046Q000100096Q000200066Q00033Q00014Q000400023Q00202Q00040004001600062Q000400860001000100047B012Q008600012Q004A010400023Q00203C0104000400170006E7000400860001000100047B012Q008600012Q004A010400073Q00203C0104000400180010150103001200042Q00D23Q000300016Q00046Q0001000A6Q000200066Q00033Q00014Q000400073Q00202Q00040004001900102Q0003001200046Q000300016Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q33E33F000B4Q004A016Q00203C014Q00010006E73Q000A0001000100047B012Q000A00012Q004A012Q00014Q0005000100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q00BD3Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F000B4Q004A016Q00203C014Q00010006E73Q000A0001000100047B012Q000A00012Q004A012Q00014Q0005000100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q00BD3Q00017Q000F3Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003C40030B3Q004C61796F75744F72646572030F3Q00426F7264657253697A65506978656C03063Q00506172656E74026Q00144003063Q005374726F6B6501284Q00A700018Q00025Q00202Q00020002000100202Q00020002000200102Q0001000100024Q000100013Q00122Q000200036Q00033Q00054Q000400023Q00202Q00040004000500102Q00030004000400122Q000400073Q00202Q00040004000800122Q000500023Q00122Q000600093Q00122Q000700093Q00062Q0008001300013Q00047B012Q001300010012C30008000A4Q00390004000800020010110003000600044Q00045Q00202Q00040004000100102Q0003000B000400302Q0003000C00094Q000400033Q00102Q0003000D00044Q0001000300024Q000200046Q000300013Q0012C30004000E4Q003B0102000400012Q004A010200054Q00A8000300014Q004A010400023Q00203C01040004000F2Q004A010500064Q003B0102000500012Q00A2000100024Q00BD3Q00017Q00213Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503163Q004261636B67726F756E645472616E73706172656E637903043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003640030B3Q004C61796F75744F7264657203063Q00506172656E7403043Q0049636F6E030D3Q00637562652D7665727465786573026Q002C4003093Q0049636F6E436F6C6F7203063Q00412Q63656E74030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E027Q004003093Q00546578744C6162656C03043Q005465787403043Q004E616D6503073Q0053656374696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q0036C002643Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100024Q004A01026Q006700035Q00202Q00030003000100202Q00030003000200102Q0002000100034Q000200013Q00122Q000300036Q00043Q000400302Q00040004000200122Q000500063Q00204700050005000700122Q000600023Q00122Q000700083Q00122Q000800083Q00122Q000900096Q0005000900020010150104000500052Q004A01055Q00203C0105000500010010150104000A00052Q004A010500023Q0010150104000B00052Q00390002000400022Q004A010300033Q00203C01040001000C0006E7000400200001000100047B012Q002000010012C30004000D3Q0012C30005000E4Q008001065Q00203C01070001000F0006E7000700270001000100047B012Q002700012Q004A010700043Q00203C0107000700102Q003900030007000200121F010400123Q00202Q00040004000700122Q000500083Q00122Q000600136Q00040006000200102Q00030011000400122Q000400063Q00202Q00040004000700122Q000500083Q00122Q000600153Q00122Q000700133Q00122Q000800086Q00040008000200102Q00030014000400102Q0003000B00024Q000400013Q00122Q000500166Q00063Q000A00302Q00060004000200202Q00070001001800062Q0007003F0001000100047B012Q003F00010012C3000700193Q0010150106001700072Q0043010700053Q00102Q0006001A00074Q000700043Q00202Q00070007001700102Q0006001B000700302Q0006001C001D00122Q0007001F3Q00202Q00070007001E00202Q00070007002000102Q0006001E000700122Q000700123Q00202Q00070007000700122Q000800083Q00122Q000900136Q00070009000200102Q00060011000700122Q000700063Q00202Q00070007000700122Q000800083Q00122Q000900093Q00122Q000A00133Q00122Q000B00086Q0007000B000200102Q00060014000700122Q000700063Q00202Q00070007000700122Q000800023Q00122Q000900213Q00122Q000A00023Q00122Q000B00086Q0007000B000200102Q00060005000700102Q0006000B00024Q0004000600014Q000200028Q00017Q00343Q0003073Q0044656661756C74026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00546578744C6162656C03043Q004E616D6503063Q00546F2Q676C6503083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674025Q0080474003053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q66026Q0020C0030A3Q0066726F6D4F2Q66736574026Q002Q40026Q003040030F3Q00426F7264657253697A65506978656C026Q002CC0027Q0040026Q0018402Q033Q005365742Q033Q0047657403093Q0041637469766174656403073Q00436F2Q6E65637403083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03083Q00496E7374616E636502D43Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100023Q00203C0102000100010006E7000200080001000100047B012Q000800012Q008001026Q004A01035Q001242000400026Q0003000200024Q000400013Q00122Q000500036Q00063Q000400302Q00060004000500302Q00060006000700122Q000700093Q00202Q00070007000A00122Q000800073Q00122Q0009000B3Q00122Q000A00073Q00122Q000B000B6Q0007000B000200102Q00060008000700102Q0006000C00034Q00040006000200122Q0005000D3Q00202Q00060001000E00062Q0006003500013Q00047B012Q003500012Q004A010600023Q00203C01070001000E0012C30008000F4Q008001096Q004A010A00033Q00203C010A000A00102Q00390006000A0002001259010700123Q00200E01070007000A00122Q0008000B3Q00122Q000900136Q00070009000200102Q00060011000700122Q000700093Q00202Q00070007000A00122Q0008000B3Q00122Q0009000D3Q00122Q000A00133Q00122Q000B000B6Q0007000B000200102Q0006001400070010150106000C00040012C3000500154Q004A010600013Q001225000700166Q00083Q000A00302Q00080006000700202Q00090001001700062Q0009003D0001000100047B012Q003D00010012C3000900183Q0010150108000400092Q00B9000900043Q00102Q0008001900094Q000900033Q00202Q00090009000400102Q0008001A000900302Q0008001B001C00122Q0009001E3Q00202Q00090009001D00202Q00090009001F00102Q0008001D0009001259010900123Q00205B00090009000A00122Q000A000B3Q00122Q000B00136Q0009000B000200102Q000800110009001238010900093Q00202Q00090009000A00122Q000A000B6Q000B00053Q00122Q000C00133Q00122Q000D000B6Q0009000D000200102Q00080014000900122Q000900093Q00202Q00090009000A0012C3000A00073Q002037000B000500204Q000B000B3Q00122Q000C00073Q00122Q000D000B6Q0009000D000200102Q00080008000900102Q0008000C00044Q0006000800014Q000600013Q00122Q000700214Q002D00083Q000600062C0102006A00013Q00047B012Q006A00012Q004A010900033Q00203C0109000900230006E70009006C0001000100047B012Q006C00012Q004A010900033Q00203C010900090024001015010800220009001259010900123Q00200E01090009000A00122Q000A00073Q00122Q000B00136Q0009000B000200102Q00080011000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00122Q000B00253Q00122Q000C00133Q00122Q000D000B6Q0009000D000200102Q000800140009001259010900093Q00205B00090009002600122Q000A00273Q00122Q000B00286Q0009000B000200102Q0008000800090030C700080029000B0010150108000C00042Q00390006000800022Q004A010700054Q00A8000800063Q0012C30009000D4Q003B0107000900012Q004A010700013Q0012C3000800214Q007E00093Q00064Q000A00033Q00202Q000A000A000400102Q00090022000A00122Q000A00123Q00205B000A000A000A00122Q000B000B3Q00122Q000C00136Q000A000C000200102Q00090011000A00062C0102009F00013Q00047B012Q009F0001001259010A00093Q002047000A000A000A00122Q000B00073Q00122Q000C002A3Q00122Q000D00133Q00122Q000E000B6Q000A000E00020006E7000A00A60001000100047B012Q00A60001001259010A00093Q002047000A000A000A00122Q000B000B3Q00122Q000C002B3Q00122Q000D00133Q00122Q000E000B6Q000A000E000200101501090014000A0012A5000A00093Q00202Q000A000A002600122Q000B001C3Q00122Q000C001C6Q000A000C000200102Q00090008000A00302Q00090029000B00102Q0009000C00064Q0007000900024Q000800056Q000900073Q00122Q000A002C6Q0008000A00014Q00085Q0006A601093Q000100072Q00A83Q00024Q004A012Q00064Q00A83Q00064Q004A012Q00074Q004A012Q00034Q00A83Q00074Q00A83Q00013Q0010150108002D00090006A601090001000100012Q00A83Q00023Q0010150108002E000900203C01090004002F0020130009000900300006A6010B0002000100022Q00A83Q00084Q00A83Q00024Q003B0109000B000100062C010200D100013Q00047B012Q00D1000100203C01090001003100062C010900D100013Q00047B012Q00D10001001259010900323Q00203C01090009003300203C010A000100312Q0080010B00014Q003B0109000B00010010150108003400032Q00A2000800024Q00BD3Q00013Q00033Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02344Q00C100018Q000200016Q000300026Q000400036Q00053Q00014Q00065Q00062Q0006000C00013Q00047B012Q000C00012Q004A010600043Q00203C0106000600020006E70006000E0001000100047B012Q000E00012Q004A010600043Q00203C0106000600030010150105000100062Q001A0102000500014Q000200016Q000300056Q000400036Q00053Q00014Q00065Q00062Q0006002000013Q00047B012Q00200001001259010600053Q00204700060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A00020006E7000600270001000100047B012Q00270001001259010600053Q00204700060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A00020010150105000400062Q003B0102000500012Q004A010200063Q00203C01020002000C00062C0102003300013Q00047B012Q003300010012590102000D3Q00200D00020002000E4Q000300063Q00202Q00030003000C4Q00048Q0002000400012Q00BD3Q00019Q002Q0001034Q004A2Q016Q00A2000100024Q00BD3Q00017Q00013Q002Q033Q0053657400064Q008C016Q00206Q00014Q000200016Q000200028Q000200016Q00017Q003A3Q002Q033Q004D696E028Q002Q033Q004D6178026Q00594003093Q00496E6372656D656E74026Q00F03F03043Q006D61746803053Q00636C616D7003073Q0044656661756C74026Q00454003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E637903043Q005465787403043Q004E616D6503063Q00536C6964657203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q002040026Q00104003043Q0053697A65026Q004EC0026Q002C4003063Q00506172656E7403083Q00746F737472696E6703063Q00412Q63656E7403053Q005269676874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q0020C0026Q004940030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66026Q00E03F026Q0026C0026Q0030C0026Q002440030F3Q00426F7264657253697A65506978656C026Q00084003053Q004672616D65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578027Q0040026Q0018402Q033Q005365742Q033Q0047657403083Q00496E7374616E636502E53Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100023Q00203C0102000100010006E7000200080001000100047B012Q000800010012C3000200023Q00203C0103000100030006E70003000C0001000100047B012Q000C00010012C3000300043Q00203C0104000100050006E7000400100001000100047B012Q001000010012C3000400063Q001259010500073Q00203C01050005000800203C0106000100090006E7000600160001000100047B012Q001600012Q00A8000600024Q00A8000700024Q00A8000800034Q00390005000800022Q004A01065Q0012DA0007000A6Q0006000200024Q000700013Q00122Q0008000B6Q00093Q000900302Q0009000C000600202Q000A0001000E00062Q000A00240001000100047B012Q002400010012C3000A000F3Q0010150109000D000A2Q00B9000A00023Q00102Q00090010000A4Q000A00033Q00202Q000A000A000D00102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A001600102Q00090014000A001259010A00183Q002047000A000A001900122Q000B00023Q00122Q000C001A3Q00122Q000D00023Q00122Q000E001B6Q000A000E000200101501090017000A001259010A00183Q002047000A000A001900122Q000B00063Q00122Q000C001D3Q00122Q000D00023Q00122Q000E001E6Q000A000E000200100B0009001C000A00102Q0009001F00064Q0007000900014Q000700013Q00122Q0008000B6Q00093Q000A00302Q0009000C000600122Q000A00206Q000B00056Q000A000200020010150109000D000A2Q00B9000A00043Q00102Q00090010000A4Q000A00033Q00202Q000A000A002100102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A002200102Q00090014000A001259010A00243Q00200E010A000A001900122Q000B00063Q00122Q000C00026Q000A000C000200102Q00090023000A00122Q000A00183Q00202Q000A000A001900122Q000B00063Q00122Q000C00253Q00122Q000D00023Q00122Q000E001B6Q000A000E000200102Q00090017000A001259010A00183Q002047000A000A001900122Q000B00023Q00122Q000C00263Q00122Q000D00023Q00122Q000E001E6Q000A000E00020010150109001C000A0010150109001F00062Q00390007000900022Q004A010800013Q0012C3000900274Q002D000A3Q00080030C7000A000D00280030C7000A0029002A2Q004A010B00033Q00203C010B000B002C001015010A002B000B001259010B00243Q00200E010B000B001900122Q000C00023Q00122Q000D002D6Q000B000D000200102Q000A0023000B00122Q000B00183Q00202Q000B000B001900122Q000C00023Q00122Q000D001A3Q00122Q000E00063Q00122Q000F002E6Q000B000F000200102Q000A0017000B001259010B00183Q002047000B000B001900122Q000C00063Q00122Q000D002F3Q00122Q000E00023Q00122Q000F00306Q000B000F0002001015010A001C000B0030C7000A00310002001015010A001F00062Q00390008000A00022Q004A010900054Q00A8000A00083Q0012C3000B00324Q003B0109000B00012Q004A010900013Q0012C3000A00334Q007E000B3Q00044Q000C00033Q00202Q000C000C002100102Q000B002B000C00122Q000C00183Q00203C010C000C00192Q0038000D000500022Q0038000E000300022Q00C2000D000D000E0012C3000E00023Q0012C3000F00063Q0012C3001000024Q0039000C00100002001015010B001C000C0030C7000B00310002001015010B001F00082Q00390009000B00022Q004A010A00054Q00A8000B00093Q0012C3000C00324Q003B010A000C00012Q004A010A00013Q0012C3000B00334Q007E000C3Q00074Q000D00033Q00202Q000D000D000D00102Q000C002B000D00122Q000D00243Q00205B000D000D001900122Q000E002D3Q00122Q000F002D6Q000D000F000200102Q000C0023000D001259010D00183Q00203C010D000D00192Q0038000E000500022Q0038000F000300022Q00C2000E000E000F0012C3000F00023Q0012C30010002D3Q0012C3001100024Q0039000D00110002001015010C0017000D001259010D00183Q00205B000D000D003400122Q000E00133Q00122Q000F00136Q000D000F000200102Q000C001C000D0030C7000C003100020030C7000C00350036001015010C001F00082Q0039000A000C00022Q00C4000B00056Q000C000A3Q00122Q000D00376Q000B000D00014Q000B5Q0006A6010C3Q000100082Q00A83Q00024Q00A83Q00034Q00A83Q00054Q00A83Q00044Q00A83Q00094Q00A83Q000A4Q00A83Q00074Q00A83Q00014Q004A010D00064Q00A8000E00083Q0006A6010F0001000100012Q00A83Q000C4Q003B010D000F00010006A6010D0002000100032Q00A83Q000C4Q00A83Q00024Q00A83Q00033Q001015010B0038000D0006A6010D0003000100012Q00A83Q00053Q001015010B0039000D001015010B003A00062Q00A2000B00024Q00BD3Q00013Q00043Q000F3Q0003043Q006D61746803053Q00636C616D7003053Q00666C2Q6F72026Q00E03F028Q0003043Q0053697A6503053Q005544696D322Q033Q006E6577026Q00F03F03083Q00506F736974696F6E03043Q005465787403083Q00746F737472696E6703083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02474Q008F00028Q000300016Q00048Q0003000300044Q000300036Q00020002000300122Q000300013Q00202Q00030003000200122Q000400013Q00202Q0004000400034Q000500036Q00050002000500202Q0005000500044Q0004000200024Q000500036Q0004000400054Q00058Q000600016Q0003000600024Q000300026Q000300016Q00048Q00030003000400262Q0003001C0001000500047B012Q001C00010012C3000300053Q0006E7000300230001000100047B012Q002300012Q004A010300024Q004A01046Q00380003000300042Q004A010400014Q004A01056Q00380004000400052Q00C20003000300042Q004A010400043Q00129A000500073Q00202Q0005000500084Q000600033Q00122Q000700053Q00122Q000800093Q00122Q000900056Q00050009000200102Q0004000600054Q000400053Q00122Q000500073Q00203C0105000500082Q007B000600033Q00122Q000700053Q00122Q000800043Q00122Q000900056Q00050009000200102Q0004000A00054Q000400063Q00122Q0005000C6Q000600026Q0005000200020010150104000B000500062C2Q01004600013Q00047B012Q004600012Q004A010400073Q00203C01040004000D00062C0104004600013Q00047B012Q004600010012590104000E3Q00200D00040004000F4Q000500073Q00202Q00050005000D4Q000600026Q0004000600012Q00BD3Q00019Q002Q0001054Q00DD00018Q00028Q000300016Q0001000300016Q00017Q00023Q0003043Q006D61746803053Q00636C616D7002104Q00F900025Q00122Q000300013Q00202Q0003000300024Q000400016Q000500016Q000600026Q0003000600024Q000400016Q0003000300044Q000400026Q000500016Q0004000400054Q0003000300044Q000400016Q0002000400016Q00019Q002Q0001034Q004A2Q016Q00A2000100024Q00BD3Q00017Q00453Q00026Q003C4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503073Q0054657874626F7803083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65029A5Q99D93F026Q0020C003063Q00506172656E7403053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E64617279026Q0018C0026Q003440030D3Q004175746F6D6174696353697A6503013Q005803103Q00436C69707344657363656E64616E74732Q01030F3Q00426F7264657253697A65506978656C03093Q00554950612Q64696E67030B3Q0050612Q64696E674C65667403043Q005544696D026Q001440030C3Q0050612Q64696E675269676874026Q00104003063Q005374726F6B6503073Q0054657874426F7803073Q0044656661756C74034Q00030F3Q00506C616365686F6C64657254657874030B3Q00506C616365686F6C6465722Q033Q003Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q0053756254657874026Q00264003103Q00436C656172546578744F6E466F637573010003103Q00554953697A65436F6E73747261696E7403073Q004D696E53697A6503083Q004D696E5769647468025Q0080464003073Q004D617853697A6503083Q004D61785769647468025Q0080614003043Q006D61746803043Q006875676503073Q00466F637573656403073Q00436F2Q6E65637403093Q00466F6375734C6F73742Q033Q005365742Q033Q0047657403083Q00496E7374616E636502D23Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100024Q004A01025Q0012DA000300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000A00302Q00050003000400202Q00060001000600062Q0006000F0001000100047B012Q000F00010012C3000600073Q0010150105000500062Q0009000600023Q00102Q0005000800064Q000600033Q00202Q00060006000500102Q00050009000600302Q0005000A000B00122Q0006000D3Q00202Q00060006000C00202Q00060006000E00102Q0005000C000600122Q000600103Q00202Q00060006001100122Q000700123Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00202Q00060006001100122Q000700123Q00122Q000800163Q00122Q000900133Q00122Q000A00126Q0006000A000200102Q00050014000600122Q000600153Q00202Q00060006001100122Q000700183Q00122Q000800193Q00122Q000900043Q00122Q000A00126Q0006000A000200102Q00050017000600102Q0005001A00024Q0003000500014Q000300013Q00122Q0004001B6Q00053Q00084Q000600033Q00202Q00060006001D00102Q0005001C000600122Q000600103Q00202Q00060006001100122Q000700043Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00202Q00060006001100122Q000700043Q00122Q0008001E3Q00122Q000900133Q00122Q000A00126Q0006000A000200102Q00050014000600122Q000600153Q00202Q00060006001100122Q000700123Q00122Q000800123Q00122Q000900123Q00122Q000A001F6Q0006000A000200102Q00050017000600122Q0006000D3Q00202Q00060006002000202Q00060006002100102Q00050020000600302Q00050022002300302Q00050024001200102Q0005001A00024Q00068Q000700013Q00122Q000800256Q00093Q000200122Q000A00273Q00202Q000A000A001100122Q000B00123Q00122Q000C00286Q000A000C000200102Q00090026000A00122Q000A00273Q00205B000A000A001100122Q000B00123Q00122Q000C00286Q000A000C000200102Q00090029000A2Q0099010700094Q000D01063Q00012Q00390003000600022Q003B000400046Q000500033Q00122Q0006002A6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006002B4Q000700066Q0004000700024Q000500013Q00122Q0006002C6Q00073Q000C00302Q00070003000400202Q00080001002D00062Q0008007A0001000100047B012Q007A00010012C30008002E3Q00101501070005000800203C0108000100300006E70008007F0001000100047B012Q007F00010012C3000800313Q0010150107002F00082Q004A010800033Q00203C0108000800330010200007003200084Q000800023Q00102Q0007000800084Q000800033Q00202Q00080008000500102Q00070009000800302Q0007000A003400302Q00070035003600122Q0008000D3Q0020F600080008000C00202Q00080008000E00102Q0007000C000800122Q0008000D3Q00202Q00080008002000202Q00080008002100102Q00070020000800122Q000800153Q00204700080008001100122Q000900123Q00122Q000A00123Q00122Q000B00043Q00122Q000C00126Q0008000C00020010150107001700080010150107001A00032Q002D00086Q004A010900013Q0012C3000A00374Q002D000B3Q0002001259010C00103Q00203C010C000C001100203C010D000100390006E7000D00A50001000100047B012Q00A500010012C3000D003A3Q0012C3000E00124Q00FC000C000E000200102Q000B0038000C00122Q000C00103Q00202Q000C000C001100202Q000D0001003C00062Q000D00AE0001000100047B012Q00AE00010012C3000D003D3Q001259010E003E3Q002059000E000E003F4Q000C000E000200102Q000B003B000C4Q0009000B6Q00083Q00012Q003900050008000200203C0106000500400020130006000600410006A601083Q000100042Q004A012Q00074Q00A83Q00044Q004A012Q00084Q004A012Q00034Q003B01060008000100203C0106000500420020130006000600410006A601080001000100072Q004A012Q00074Q00A83Q00044Q004A012Q00084Q004A012Q00034Q004A012Q00064Q00A83Q00014Q00A83Q00054Q003B0106000800012Q002D00063Q00030006A601070002000100012Q00A83Q00053Q0010150106004300070006A601070003000100012Q00A83Q00053Q0010150106004400070010150106004500022Q00A2000600024Q00BD3Q00013Q00043Q00043Q0003053Q00436F6C6F7203063Q00412Q63656E74030C3Q005472616E73706172656E6379029A5Q99C93F000A4Q00D79Q00000100016Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030001000400302Q0003000300046Q000300016Q00017Q00073Q0003053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E637903083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03043Q005465787400164Q00A7019Q000100016Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q0003000100044Q000400043Q00102Q0003000300046Q000300016Q00053Q00206Q000400064Q001500013Q00047B012Q00150001001259012Q00053Q0020085Q00064Q000100053Q00202Q0001000100044Q000200063Q00202Q0002000200076Q000200012Q00BD3Q00017Q00013Q0003043Q005465787402034Q004A01025Q0010150102000100012Q00BD3Q00017Q00013Q0003043Q005465787400044Q004A016Q00203C014Q00012Q00A23Q00024Q00BD3Q00017Q00253Q00026Q003C40026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D3203063Q00506172656E74026Q003A4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503053Q004C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403043Q0053697A6503053Q0056616C7565034Q0003063Q00412Q63656E7403053Q005269676874026Q0020C02Q033Q005365742Q033Q0047657403083Q00496E7374616E636502853Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100024Q004A01025Q0012BE000300016Q00020002000200122Q000300023Q00202Q00040001000300062Q0004002500013Q00047B012Q002500012Q004A010400013Q00203101050001000300122Q000600046Q00075Q00202Q00080001000500062Q000800140001000100047B012Q001400012Q004A010800023Q00203C0108000800062Q0039000400080002001259010500083Q00200E01050005000900122Q0006000A3Q00122Q0007000B6Q00050007000200102Q00040007000500122Q0005000D3Q00202Q00050005000900122Q0006000A3Q00122Q000700023Q00122Q0008000B3Q00122Q0009000A6Q00050009000200102Q0004000C00050010150104000E00020012C30003000F4Q004A010400033Q001225000500106Q00063Q000A00302Q00060011001200202Q00070001001400062Q0007002D0001000100047B012Q002D00010012C3000700153Q0010150106001300072Q00B9000700043Q00102Q0006001600074Q000700023Q00202Q00070007001300102Q00060017000700302Q00060018001900122Q0007001B3Q00202Q00070007001A00202Q00070007001C00102Q0006001A0007001259010700083Q00205B00070007000900122Q0008000A3Q00122Q0009000B6Q00070009000200102Q0006000700070012380107000D3Q00202Q00070007000900122Q0008000A6Q000900033Q00122Q000A000B3Q00122Q000B000A6Q0007000B000200102Q0006000C000700122Q0007000D3Q00202Q0007000700090012C30008000B4Q00E1000900033Q00122Q000A00123Q00122Q000B000A6Q0007000B000200102Q0006001D000700102Q0006000E00024Q0004000600024Q000500033Q001225000600106Q00073Q000A00302Q00070011001200202Q00080001001E00062Q000800580001000100047B012Q005800010012C30008001F3Q0010150107001300082Q009A010800053Q00102Q0007001600084Q000800023Q00202Q00080008002000102Q00070017000800302Q00070018001900122Q0008001B3Q00202Q00080008001A00202Q00080008002100102Q0007001A000800122Q000800083Q00202Q00080008000900122Q000900123Q00122Q000A000B6Q0008000A000200102Q00070007000800122Q0008000D3Q00202Q00080008000900122Q000900123Q00122Q000A00223Q00122Q000B000B3Q00122Q000C000A6Q0008000C000200102Q0007000C000800122Q0008000D3Q00202Q00080008000900122Q0009000B3Q00122Q000A00223Q00122Q000B00123Q00122Q000C000A6Q0008000C000200102Q0007001D000800102Q0007000E00024Q0005000700024Q00063Q00030006A601073Q000100012Q00A83Q00053Q0010150106002300070006A601070001000100012Q00A83Q00053Q0010150106002400070010150106002500022Q00A2000600024Q00BD3Q00013Q00023Q00023Q0003043Q005465787403083Q00746F737472696E6702064Q00CB00025Q00122Q000300026Q000400016Q00030002000200102Q0002000100036Q00017Q00013Q0003043Q005465787400044Q004A016Q00203C014Q00012Q00A23Q00024Q00BD3Q00017Q002C3Q00026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00526967687449636F6E030E3Q00526967687449636F6E436F6C6F72026Q0020C003093Q00546578744C6162656C03043Q004E616D6503063Q0042752Q746F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E6365030C3Q00536574526967687449636F6E02A53Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100024Q004A01025Q001225010300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000500302Q00050003000400302Q00050005000600302Q00050007000800122Q0006000A3Q00204700060006000B00122Q000700083Q00122Q0008000C3Q00122Q000900083Q00122Q000A000C6Q0006000A00020010150105000900060010150105000D00022Q00390003000500020012C30004000E3Q00203C01050001000F00062C0105003500013Q00047B012Q003500012Q004A010500023Q00203101060001000F00122Q000700106Q00085Q00202Q00090001001100062Q000900240001000100047B012Q002400012Q004A010900033Q00203C0109000900122Q0039000500090002001259010600143Q00200E01060006000B00122Q0007000C3Q00122Q000800156Q00060008000200102Q00050013000600122Q0006000A3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000E3Q00122Q000900153Q00122Q000A000C6Q0006000A000200102Q0005001600060010150105000D00030012C3000400174Q0079010500053Q0012C30006000C3Q00203C01070001001800062C0107005500013Q00047B012Q005500012Q004A010700023Q00203101080001001800122Q000900106Q000A5Q00202Q000B0001001900062Q000B00430001000100047B012Q004300012Q004A010B00033Q00203C010B000B00122Q00390007000B00022Q001E010500073Q00122Q000700143Q00202Q00070007000B00122Q000800083Q00122Q000900156Q00070009000200102Q00050013000700122Q0007000A3Q00202Q00070007000B00122Q000800083Q00122Q0009001A3Q00122Q000A00153Q00122Q000B000C6Q0007000B000200102Q00050016000700102Q0005000D000300122Q000600174Q004A010700013Q0012250008001B6Q00093Q000B00302Q00090007000800202Q000A0001001C00062Q000A005D0001000100047B012Q005D00010012C3000A001D3Q00101501090003000A2Q00B9000A00043Q00102Q0009001E000A4Q000A00033Q00202Q000A000A000300102Q0009001F000A00302Q00090020002100122Q000A00233Q00202Q000A000A002200202Q000A000A002400102Q00090022000A001259010A00233Q00203C010A000A002500203C010A000A002600101501090025000A001259010A00143Q00205B000A000A000B00122Q000B000C3Q00122Q000C00156Q000A000C000200102Q00090013000A001238010A000A3Q00202Q000A000A000B00122Q000B000C6Q000C00043Q00122Q000D00153Q00122Q000E000C6Q000A000E000200102Q00090016000A00122Q000A000A3Q00202Q000A000A000B0012C3000B00084Q0063000C000400062Q004F010C000C3Q0012C3000D00083Q0012C3000E000C4Q0039000A000E000200101501090009000A0010150109000D00032Q003900070009000200203C0108000300270020130008000800280006A6010A3Q000100042Q004A012Q00054Q00A83Q00024Q004A012Q00064Q004A012Q00034Q003B0108000A000100203C0108000300290020130008000800280006A6010A0001000100042Q004A012Q00054Q00A83Q00024Q004A012Q00064Q004A012Q00034Q003B0108000A000100203C01080003002A0020130008000800280006A6010A0002000100052Q004A012Q00054Q00A83Q00024Q004A012Q00064Q004A012Q00034Q00A83Q00014Q003B0108000A00012Q002D00083Q00020010150108002B00020006A601090003000100012Q00A83Q00053Q0010150108002C00092Q00A2000800024Q00BD3Q00013Q00043Q00023Q0003103Q004261636B67726F756E64436F6C6F7233030C3Q00456C656D656E74486F76657200094Q00649Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q00649Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00073Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E7403043Q007461736B03053Q0064656C6179029A5Q99B93F03083Q0043612Q6C6261636B03053Q00737061776E001B4Q00E59Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q0003000100124Q00033Q00206Q000400122Q000100053Q0006A601023Q000100042Q004A017Q004A012Q00014Q004A012Q00024Q004A012Q00034Q003B012Q000200012Q004A012Q00043Q00203C014Q000600062C012Q001A00013Q00047B012Q001A0001001259012Q00033Q00203C014Q00072Q004A2Q0100043Q00203C2Q01000100062Q00A5012Q000200012Q00BD3Q00013Q00013Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q00649Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00013Q0003043Q005465787402064Q004A01025Q00062C0102000500013Q00047B012Q000500012Q004A01025Q0010150102000100012Q00BD3Q00017Q00353Q0003043Q0054657874025Q00805240026Q003A4003153Q0046696E6446697273744368696C644F66436C612Q7303093Q00554950612Q64696E67030D3Q0050612Q64696E67426F2Q746F6D03043Q005544696D2Q033Q006E6577028Q00026Q004640030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E6461727903163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00F03F03083Q00506F736974696F6E03053Q005544696D32026Q0028C0026Q0024C003043Q0053697A65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578026Q00244003063Q00506172656E74026Q002A4003063Q005374726F6B65026Q33E33F03043Q0049636F6E03063Q00636972636C6503073Q0053756254657874026Q002640026Q00E03F026Q0020C003093Q00546578744C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q003EC0030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E636502B43Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100023Q00203C01020001000100062C0102000A00013Q00047B012Q000A00010012C3000300023Q0006E70003000B0001000100047B012Q000B00010012C3000300034Q004A01045Q0020130004000400040012C3000600054Q003900040006000200062C0104001700013Q00047B012Q00170001001259010500073Q00205B00050005000800122Q000600093Q00122Q0007000A6Q00050007000200102Q0004000600052Q004A010500013Q0012AB0106000B6Q00073Q000900302Q00070001000C00302Q0007000D000E4Q000800023Q00202Q00080008001000102Q0007000F000800302Q00070011001200122Q000800143Q00200E01080008000800122Q000900153Q00122Q000A00156Q0008000A000200102Q00070013000800122Q000800173Q00202Q00080008000800122Q000900153Q00122Q000A00183Q00122Q000B00153Q00122Q000C00196Q0008000C000200102Q000700160008001281000800173Q00202Q00080008001B4Q000900033Q00122Q000A00036Q0008000A000200102Q0007001A000800302Q0007001C001D4Q000800033Q00102Q0007001E00084Q0005000700022Q004A010600044Q0049000700053Q00122Q0008001F6Q0006000800014Q000600056Q000700056Q000800023Q00202Q00080008002000122Q000900216Q0006000900024Q000700063Q00203C0108000100220006E7000800470001000100047B012Q004700010012C3000800233Q0012C30009001F4Q0080010A6Q004A010B00023Q00203C010B000B00242Q00390007000B00020030C70007001C00250010150107001E00052Q0079010800083Q00062C0102008900013Q00047B012Q00890001001259010900143Q00207900090009000800122Q000A00153Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00153Q00122Q000B00273Q00122Q000C00263Q00122Q000D00096Q0009000D000200102Q0007001600094Q000900013Q00122Q000A00286Q000B3Q000B00302Q000B0011001500102Q000B000100024Q000C00073Q00102Q000B0029000C4Q000C00023Q00202Q000C000C002400102Q000B002A000C00302Q000B002B002C00122Q000C002E3Q00202Q000C000C002D00202Q000C000C002F00102Q000B002D000C00122Q000C00143Q00202Q000C000C000800122Q000D00093Q00122Q000E00266Q000C000E000200102Q000B0013000C00122Q000C00173Q00202Q000C000C000800122Q000D00093Q00122Q000E00253Q00122Q000F00263Q00122Q001000096Q000C0010000200102Q000B0016000C00122Q000C00173Q00202Q000C000C000800122Q000D00153Q00122Q000E00303Q00122Q000F00153Q00122Q001000096Q000C0010000200102Q000B001A000C00302Q000B001C002500102Q000B001E00054Q0009000B00024Q000800093Q00044Q00970001001259010900143Q00200E01090009000800122Q000A00263Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00263Q00122Q000B00093Q00122Q000C00263Q00122Q000D00096Q0009000D000200102Q00070016000900203C0109000500310020130009000900320006A6010B3Q000100062Q004A012Q00084Q00A83Q00074Q004A012Q00094Q004A012Q00024Q00A83Q00064Q00A83Q00084Q003B0109000B000100203C0109000500330020130009000900320006A6010B0001000100062Q004A012Q00084Q00A83Q00074Q004A012Q00094Q004A012Q00024Q00A83Q00064Q00A83Q00084Q003B0109000B000100203C0109000500340020130009000900320006A6010B0002000100012Q00A83Q00014Q007F0009000B00014Q00093Q000100102Q0009003500054Q000900028Q00013Q00033Q00063Q00030A3Q0054657874436F6C6F723303063Q00412Q63656E7403053Q00436F6C6F72030C3Q005472616E73706172656E6379029A5Q99C93F03043Q0054657874001D4Q0019019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030003000400302Q0003000400056Q000300016Q00053Q00064Q001C00013Q00047B012Q001C00012Q004A017Q009F000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000600102Q0003000100046Q000300012Q00BD3Q00017Q00063Q00030A3Q0054657874436F6C6F723303073Q005375625465787403053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q33E33F001D4Q0019019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000400102Q00030003000400302Q0003000500066Q000300016Q00053Q00064Q001C00013Q00047B012Q001C00012Q004A017Q009F000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q00BD3Q00017Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q004A016Q00203C014Q000100062C012Q000900013Q00047B012Q00090001001259012Q00023Q00203C014Q00032Q004A2Q015Q00203C2Q01000100012Q00A5012Q000200012Q00BD3Q00017Q002D3Q00026Q003E4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403013Q002D03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65025Q004060C003063Q00506172656E74034Q0003063Q00412Q63656E7403053Q005269676874026Q004CC0026Q005040030A3Q005465787442752Q746F6E2Q033Q0042757903103Q004261636B67726F756E64436F6C6F7233030F3Q004175746F42752Q746F6E436F6C6F720100026Q0018C0030A3Q0066726F6D4F2Q66736574026Q004640026Q003440026Q00104003093Q0041637469766174656403073Q00436F2Q6E65637403083Q00496E7374616E63652Q033Q0053657402913Q0006E7000100040001000100047B012Q000400012Q002D00026Q00A8000100024Q004A01025Q001216010300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000B00302Q00050003000400302Q0005000500064Q000600023Q00102Q0005000700064Q000600033Q0020A901060006000500102Q00050008000600302Q00050009000A00122Q0006000C3Q00202Q00060006000B00202Q00060006000D00102Q0005000B000600122Q0006000C3Q00203C01060006000E00203C01060006000F0010150105000E0006001259010600113Q00200E01060006001200122Q000700133Q00122Q000800146Q00060008000200102Q00050010000600122Q000600163Q00202Q00060006001200122Q000700133Q00122Q000800173Q00122Q000900143Q00122Q000A00136Q0006000A000200102Q000500150006001259010600163Q00204700060006001200122Q000700043Q00122Q000800193Q00122Q000900043Q00122Q000A00136Q0006000A000200109101050018000600102Q0005001A00024Q0003000500024Q000400013Q00122Q000500026Q00063Q000A00302Q00060003000400302Q00060005001B4Q000700043Q00102Q0006000700072Q004A010700033Q0020A901070007001C00102Q00060008000700302Q00060009000A00122Q0007000C3Q00202Q00070007000B00202Q00070007001D00102Q0006000B000700122Q000700113Q00200E01070007001200122Q000800043Q00122Q000900146Q00070009000200102Q00060010000700122Q000700163Q00202Q00070007001200122Q000800043Q00122Q0009001E3Q00122Q000A00143Q00122Q000B00136Q0007000B000200102Q000600150007001259010700163Q00204700070007001200122Q000800133Q00122Q0009001F3Q00122Q000A00043Q00122Q000B00136Q0007000B000200100300060018000700102Q0006001A00024Q0004000600024Q000500013Q00122Q000600206Q00073Q000A00302Q0007000500214Q000800043Q00102Q0007000700084Q000800033Q00202Q00080008000500102Q00070008000800302Q00070009000A4Q000800033Q00202Q00080008001C00102Q00070022000800302Q00070023002400122Q000800113Q00202Q00080008001200122Q000900043Q00122Q000A00146Q0008000A000200102Q00070010000800122Q000800163Q00202Q00080008001200122Q000900043Q00122Q000A00253Q00122Q000B00143Q00122Q000C00136Q0008000C000200102Q00070015000800122Q000800163Q00202Q00080008002600122Q000900273Q00122Q000A00286Q0008000A000200102Q00070018000800102Q0007001A00024Q0005000700024Q000600056Q000700053Q00122Q000800296Q00060008000100202Q00060005002A00202Q00060006002B0006A601083Q000100012Q00A83Q00014Q003B0106000800012Q002D00063Q00020010150106002C00020006A601070001000100052Q00A83Q00034Q00A83Q00044Q004A012Q00064Q00A83Q00054Q004A012Q00033Q0010150106002D00072Q00A2000600024Q00BD3Q00013Q00023Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q004A016Q00203C014Q000100062C012Q000900013Q00047B012Q00090001001259012Q00023Q00203C014Q00032Q004A2Q015Q00203C2Q01000100012Q00A5012Q000200012Q00BD3Q00017Q00113Q0003043Q0054657874034Q002Q033Q00737562027Q00C003023Q002D5303103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66030A3Q0054657874436F6C6F723303043Q00536F6C6403053Q005368696E7903063Q00436F6C6F723303073Q0066726F6D524742026Q004440026Q003E40028Q002Q033Q0042757903063Q00412Q63656E74043A4Q004A01045Q0010150104000100012Q004A010400013Q00061C010500060001000200047B012Q000600010012C3000500023Q0010150104000100052Q0090010400026Q000500016Q00040002000200202Q00040004000300122Q000600046Q00040006000200262Q000400100001000500047B012Q001000012Q00BF00046Q0080010400013Q0006E70003001E0001000100047B012Q001E00012Q004A010500034Q004A010600043Q00203C0106000600070010150105000600062Q004A010500034Q004A010600043Q00203C0106000600010010150105000800062Q004A010500033Q0030C700050001000900047B012Q0039000100062C0104002F00013Q00047B012Q002F00012Q004A010500034Q00A1000600043Q00202Q00060006000A00102Q0005000600064Q000500033Q00122Q0006000B3Q00202Q00060006000C00122Q0007000D3Q00122Q0008000E3Q00122Q0009000F6Q0006000900020010150105000800062Q004A010500033Q0030C700050001001000047B012Q003900012Q004A010500034Q004A010600043Q00203C0106000600110010150105000600062Q004A010500034Q004A010600043Q00203C0106000600010010150105000800062Q004A010500033Q0030C70005000100102Q00BD3Q00017Q00BD3Q00030C3Q0043726561746557696E646F7703053Q005469746C6503093Q0043726561746554616203043Q004E616D6503043Q004D61696E03043Q0049636F6E03123Q0033642D637562652D612Q726F772D6C656674030C3Q00437265617465546F2Q676C6503093Q004175746F204C69667403063Q00637572736F7203073Q0044656661756C74010003083Q0043612Q6C6261636B030B3Q005365744175746F4C696674030A3Q004175746F2050756E636803113Q0068616E642D68616C662D652Q6C69707365030C3Q005365744175746F50756E6368030A3Q004175746F2053746F6D702Q033Q006C6567030C3Q005365744175746F53746F6D7003103Q004175746F2054652Q7269746F72696573030C3Q006C6F636174696F6E2D70696E03123Q005365744175746F54652Q7269746F72696573030C3Q00436F2Q6C65637461626C6573030D3Q0062752Q6C65742D666C79696E67030F3Q0057616C6B20746F2041697264726F70030E3Q0070617065722D616972706C616E6503103Q0053657457616C6B546F41697264726F7003103Q0057616C6B20746F207461726765742Q20030E3Q00706572736F6E2D77616C6B696E67030E3Q005365744175746F47656D57616C6B030C3Q00437265617465536C69646572030E3Q0047656D2077616C6B2073702Q65642Q033Q004D696E026Q0034402Q033Q004D6178025Q00408E4003093Q00496E6372656D656E74026Q00F03F030F3Q0053657447656D57616C6B53702Q6564030F3Q004175746F2047656D732054772Q656E030F3Q0062752Q746572666C792D77696E6773030F3Q005365744175746F47656D54772Q656E03103Q0047656D732074772Q656E2073702Q6564026Q004940025Q00208C4003103Q0053657447656D54772Q656E53702Q656403043Q00426F2Q7303063Q0073776F72647303103Q004272696E6720412Q6C20626F2Q73657303163Q0074776F2D612Q726F77732D646F776E2D616E642D7570030C3Q00536574426F2Q734272696E67030C3Q0057616C6B20746F20426F2Q73030D3Q0053657457616C6B546F426F2Q73030A3Q00547020746F20626F2Q73030F3Q005365745470546F426F2Q734B692Q6C030D3Q0043726561746553656374696F6E030C3Q00426F2Q7320747261636B657203053Q0073776F726403093Q0049636F6E436F6C6F7203063Q00507572706C6503043Q007461736B03053Q00737061776E03043Q00452Q67732Q033Q00652Q6703173Q004175746F2068617463682073656C656374656420652Q6703173Q005365744175746F486174636853656C6563746564452Q6703103Q0053656C65637420452Q672028312D3529026Q00144003133Q0053657453656C6563746564452Q67496E646578030A3Q004375626520776F726C64030D3Q00637562652D7665727465786573030E3Q004175746F20486174636820452Q6703133Q005365744175746F486174636843756265452Q6703043Q0053686F70030D3Q0073686F2Q70696E672D6361727403093Q004175746F2053652Q6C03123Q00636972636C652D646F2Q6C61722D7369676E030B3Q005365744175746F53652Q6C03103Q004175746F20427579205765696768747303153Q0073686F2Q70696E672D6261736B65742D636865636B03113Q005365744175746F42757957656967687473030C3Q004175746F2042757920444E412Q033Q0070696E030D3Q005365744175746F427579444E41030F3Q004175746F2042757920426F6469657303103Q00706572736F6E2D63726F756368696E6703103Q005365744175746F427579426F6469657303063Q004D61726B6574030D3Q006275696C64696E672D62616E6B030C3Q004175746F2042757920412Q6C031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F7269657303083Q004D65726368616E74030F3Q004D65726368616E74206F2Q6665727303063Q0048692Q64656E2Q0103133Q004D65726368616E742050657420536E6970657203113Q005365744D65726368616E74536E69706572030D3Q0043726561746554657874626F7803093Q004D6178207072696365030B3Q00506C616365686F6C64657203153Q00652E672E20354D2028626C616E6B203D20616E792903133Q005365744D65726368616E744D61785072696365030B3Q004372656174654C6162656C03063Q0054617267657403053Q0056616C756503043Q004E6F6E6503063Q00537461747573034Q00030A3Q005065747320666F756E64030E3Q004F70656E2050657420496E646578025Q00408F40030A3Q0053656172636820706574030B3Q00706574206E616D653Q2E030C3Q0043726561746542752Q746F6E03103Q005265667265736820706574206C69737403183Q00726F746174652D636F756E7465722D636C6F636B7769736503113Q00436C65617220612Q6C207461726765747303013Q007803273Q005069636B20796F75722070657473202874617020746F2073656C6563742F756E73656C6563742903093Q00526967687449636F6E03103Q00612Q726F772D6C617267652D646F776E03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203123Q00437265617465436F726E657242752Q746F6E03113Q00612Q726F772D6C617267652D726967687403043Q005465787403063Q004F2Q6665727303103Q004261636B20746F204D65726368616E7403103Q00612Q726F772D6C617267652D6C65667403063Q0069706169727303053Q00546965723103053Q00546965723203053Q005469657233030C3Q00437265617465427579526F7703043Q004375626503043Q006375626503153Q0055736520617420796F7572206F776E207269736B2103123Q00637562652D7175657374696F6E2D6D61726B030A3Q004175746F204375626573030C3Q005365744175746F4375626573030B3Q0054772Q656E2053702Q6564026Q004440030C3Q005365744375626553702Q6564030D3Q0057616C6B20746F204375626573030E3Q0053657457616C6B546F437562657303043Q004D69736303103Q0063726F2Q7368616972732D736C61736803143Q0044697361626C652033442052656E646572696E6703103Q00636F6D70757465722D6D6F6E69746F72030C3Q0053657444697361626C653344030B3Q004175746F2052656A6F696E03133Q00612Q726F772D6375726C2D746F2D7269676874030D3Q005365744175746F52656A6F696E030E3Q004175746F2052656A6F696E205632030F3Q005365744175746F52656A6F696E563203103Q00437573746F6D2057616C6B53702Q656403063Q00706572736F6E03123Q0053657457616C6B53702Q6564546F2Q676C65030F3Q0057616C6B53702Q65642056616C7565025Q0070974003113Q0053657457616C6B53702Q656456616C756503103Q00437573746F6D204A756D70506F776572030E3Q00706572736F6E2D66612Q6C696E6703123Q005365744A756D70506F776572546F2Q676C65030F3Q004A756D70506F7765722056616C7565025Q00407F4003113Q005365744A756D70506F77657256616C7565030D3Q00496E66696E697465204A756D70030F3Q00536574496E66696E6974654A756D7003053Q005374617473030A3Q00636861696E2D6C696E6B2Q033Q00465053031A3Q0063686576726F6E2D6C617267652D636F6E64656E7365642D7570030A3Q004C6967687447722Q656E03013Q0030030C3Q004E6574776F726B2050696E6703103Q00776966692D7369676E616C2D6661697203043Q0030206D73030C3Q00456C61707365642054696D6503053Q00636C6F636B03083Q002Q303A2Q303A2Q30030A3Q0047656D73202F204D696E03193Q00636C6F636B2D7370696E2D726576657273652D646173686564030B3Q0047656D73204561726E656403073Q006469616D6F6E64030B3Q005265736574205374617473007E023Q0055016Q00206Q00014Q00023Q00014Q000300013Q00102Q0002000200036Q0002000200202Q00013Q00034Q00033Q000200302Q00030004000500302Q0003000600074Q00010003000200202Q0002000100084Q00043Q000400302Q00040004000900302Q00040006000A00302Q0004000B000C4Q000500023Q00202Q00050005000E00102Q0004000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004000F00302Q00040006001000302Q0004000B000C4Q000500023Q00202Q00050005001100102Q0004000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001200302Q00040006001300302Q0004000B000C4Q000500023Q00202Q00050005001400102Q0004000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001500302Q00040006001600302Q0004000B000C4Q000500023Q00202Q00050005001700102Q0004000D00054Q00020004000100202Q00023Q00034Q00043Q000200302Q00040004001800302Q0004000600194Q00020004000200202Q0003000200084Q00053Q000400302Q00050004001A00302Q00050006001B00302Q0005000B000C4Q000600023Q00202Q00060006001C00102Q0005000D00064Q00030005000100202Q0003000200084Q00053Q000400302Q00050004001D00302Q00050006001E00302Q0005000B000C4Q000600023Q00202Q00060006001F00102Q0005000D00064Q00030005000100202Q0003000200204Q00053Q000600302Q00050004002100302Q00050022002300302Q00050024002500302Q0005000B002300302Q0005002600274Q000600023Q00202Q00060006002800102Q0005000D00062Q003B0103000500010020130003000200082Q006101053Q000400302Q00050004002900302Q00050006002A00302Q0005000B000C4Q000600023Q00202Q00060006002B00102Q0005000D00064Q00030005000100202Q0003000200204Q00053Q00060030C700050004002C0030C700050022002D00305001050024002E00302Q0005000B002D00302Q0005002600274Q000600023Q00202Q00060006002F00102Q0005000D00064Q00030005000100202Q00033Q00034Q00053Q000200302Q0005000400300030C70005000600312Q00390003000500020020130004000300082Q006101063Q000400302Q00060004003200302Q00060006003300302Q0006000B000C4Q000700023Q00202Q00070007003400102Q0006000D00074Q00040006000100202Q0004000300084Q00063Q000400305001060004003500302Q00060006003300302Q0006000B000C4Q000700023Q00202Q00070007003600102Q0006000D00074Q00040006000100202Q0004000300084Q00063Q000400302Q00060004003700309401060006003300302Q0006000B000C4Q000700023Q00202Q00070007003800102Q0006000D00074Q00040006000100202Q0004000300394Q00063Q000300302Q00060004003A00302Q00060006003B2Q004A010700033Q00203C01070007003D0010150106003C00072Q003B0104000600012Q002D00045Q0012590105003E3Q00203C01050005003F0006A601063Q000100032Q004A012Q00044Q00A83Q00044Q00A83Q00034Q00B400050002000100202Q00053Q00034Q00073Q000200302Q00070004004000302Q0007000600414Q00050007000200202Q0006000500084Q00083Q000400302Q00080004004200302Q0008000600410030C70008000B000C2Q0075010900023Q00202Q00090009004300102Q0008000D00094Q00060008000100202Q0006000500204Q00083Q000600302Q00080004004400302Q00080022002700302Q00080024004500302Q0008000B00270030C70008002600272Q004A010900023Q00203C0109000900460010150108000D00092Q003B0106000800010020130006000500392Q002D00083Q00020030C70008000400470030C70008000600482Q003B0106000800010020130006000500082Q006101083Q000400302Q00080004004900302Q00080006004100302Q0008000B000C4Q000900023Q00202Q00090009004A00102Q0008000D00094Q00060008000100202Q00063Q00034Q00083Q00020030C700080004004B0030C700080006004C2Q00390006000800020020130007000600082Q006101093Q000400302Q00090004004D00302Q00090006004E00302Q0009000B000C4Q000A00023Q00202Q000A000A004F00102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400305001090004005000302Q00090006005100302Q0009000B000C4Q000A00023Q00202Q000A000A005200102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005300309401090006005400302Q0009000B000C4Q000A00023Q00202Q000A000A005500102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005600302Q0009000600570030C70009000B000C2Q004A010A00023Q00203C010A000A00580010150109000D000A2Q003B01070009000100205201073Q00034Q00093Q000200302Q00090004005900302Q00090006005A4Q00070009000200202Q0008000700084Q000A3Q000400302Q000A0004005B00302Q000A0006005100302Q000A000B000C4Q000B00023Q00202Q000B000B005C00102Q000A000D000B4Q0008000A000100202Q0008000700084Q000A3Q000400302Q000A0004004D00302Q000A0006004E00302Q000A000B000C4Q000B00023Q00202Q000B000B005D00102Q000A000D000B4Q0008000A000100202Q0008000700084Q000A3Q000400302Q000A0004001500302Q000A0006001600302Q000A000B000C4Q000B00023Q00202Q000B000B005E00102Q000A000D000B4Q0008000A000100202Q00083Q00034Q000A3Q000200302Q000A0004005F00302Q000A000600514Q0008000A000200202Q00093Q00034Q000B3Q000200302Q000B0004006000302Q000B006100624Q0009000B000200202Q000A000800084Q000C3Q000400302Q000C0004006300302Q000C0006005100302Q000C000B000C4Q000D00023Q00202Q000D000D006400102Q000C000D000D4Q000A000C000100202Q000A000800654Q000C3Q000300302Q000C0004006600302Q000C006700684Q000D00023Q00202Q000D000D006900102Q000C000D000D4Q000A000C000100202Q000A0008006A4Q000C3Q000200302Q000C0004006B00302Q000C006C006D4Q000A000C000200202Q000B0008006A4Q000D3Q000200302Q000D0004006E00302Q000D006C006F4Q000B000D000200202Q000C0008006A4Q000E3Q000300302Q000E0004007000302Q000E0006004100302Q000E006C00714Q000C000E000200122Q000D003E3Q00202Q000D000D003F0006A6010E0001000100032Q004A012Q00044Q004A012Q00054Q00A83Q000C4Q00BC000D000200014Q000D8Q000E5Q00122Q000F00726Q001000113Q0006A601120002000100052Q00A83Q000A4Q004A012Q00054Q00A83Q000D4Q004A012Q00034Q004A012Q00063Q0006A601130003000100032Q00A83Q00104Q00A83Q000D4Q00A83Q000E3Q0006A601140004000100032Q00A83Q000E4Q00A83Q00114Q00A83Q00133Q0020130015000800652Q002D00173Q00030030C70017000400730030C70017006700740006A601180005000100022Q00A83Q00144Q00A83Q00133Q0010150117000D00182Q00390015001700022Q00A8001000153Q0006A601150006000100082Q00A83Q000D4Q004A012Q00054Q00A83Q000B4Q00A83Q00084Q004A012Q00024Q00A83Q00124Q00A83Q000F4Q00A83Q00133Q0020190016000800754Q00183Q000300302Q00180004007600302Q00180006007700102Q0018000D00154Q00160018000100202Q0016000800754Q00183Q000300302Q00180004007800302Q0018000600790006A601190007000100042Q004A012Q00024Q00A83Q000B4Q004A012Q00054Q00A83Q00123Q0010470118000D00194Q00160018000100202Q0016000800754Q00183Q000500302Q00180004007A00302Q0018000600414Q001900033Q00202Q00190019003D00102Q0018003C001900302Q0018007B007C0006A601190008000100022Q00A83Q00144Q00A83Q000E3Q0010A90018000D00194Q0016001800024Q001100163Q00202Q00160011007D00202Q0017000F002700102Q0016007E00174Q001600146Q00178Q0016000200014Q001600156Q00160001000100202Q00160008007F4Q00183Q000300302Q00180006008000302Q0018008100820006A601190009000100012Q00A83Q00093Q0010150118000D00192Q003B0116001800010020130016000900752Q002D00183Q00030030C70018000400830030C70018000600840006A60119000A000100012Q00A83Q00083Q0010150118000D00192Q003B01160018000100201300160009006A2Q002D00183Q00020030C700180004006E0030C70018006C006F2Q00390016001800020020130017000900392Q002D00193Q00030030C70019000400600030C70019000600512Q002E001A00033Q00202Q001A001A003D00102Q0019003C001A4Q0017001900014Q00178Q00185Q00122Q001900856Q001A00033Q00122Q001B00863Q00122Q001C00873Q0012C3001D00884Q004C011A000300012Q003100190002001B00047B012Q00B52Q01002013001E000900892Q002D00203Q00010006A60121000B000100042Q00A83Q00184Q00A83Q001D4Q004A012Q00054Q00A83Q00163Q0010150120000D00212Q0039001E002000022Q000B0117001D001E2Q00AA011C5Q0006F7001900AA2Q01000200047B012Q00AA2Q010012590119003E3Q00203C01190019003F0006A6011A000C000100042Q004A012Q00044Q004A012Q00054Q00A83Q00174Q00A83Q00184Q008901190002000100202Q00193Q00034Q001B3Q000200302Q001B0004008A00302Q001B0006008B4Q0019001B000200202Q001A001900394Q001C3Q000300302Q001C0004008C00302Q001C0006008D4Q001D00033Q00202Q001D001D003D00102Q001C003C001D4Q001A001C000100202Q001A001900084Q001C3Q000400302Q001C0004008E00302Q001C0006000700302Q001C000B000C4Q001D00023Q00202Q001D001D008F00102Q001C000D001D4Q001A001C000100202Q001A001900204Q001C3Q000600302Q001C0004009000302Q001C0022002300302Q001C0024009100302Q001C000B009100302Q001C002600274Q001D00023Q00202Q001D001D009200102Q001C000D001D4Q001A001C000100202Q001A001900084Q001C3Q000400302Q001C0004009300302Q001C0006001E00302Q001C000B000C4Q001D00023Q00202Q001D001D009400102Q001C000D001D4Q001A001C000100202Q001A3Q00034Q001C3Q000200302Q001C0004009500302Q001C000600964Q001A001C000200202Q001B001A00084Q001D3Q000400302Q001D0004009700302Q001D0006009800302Q001D000B000C4Q001E00023Q00202Q001E001E009900102Q001D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D0004009A00302Q001D0006009B00302Q001D000B000C4Q001E00023Q00202Q001E001E009C00102Q001D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D0004009D00302Q001D0006009B00302Q001D000B000C4Q001E00023Q00202Q001E001E009E00102Q001D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D0004009F00302Q001D000600A000302Q001D000B000C2Q004A011E00023Q002004011E001E00A100102Q001D000D001E4Q001B001D000100202Q001B001A00204Q001D3Q000600302Q001D000400A200302Q001D0022002300302Q001D002400A300302Q001D000B002300302Q001D002600274Q001E00023Q00202Q001E001E00A400102Q001D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D000400A500302Q001D000600A600302Q001D000B000C4Q001E00023Q00202Q001E001E00A700102Q001D000D001E4Q001B001D000100202Q001B001A00204Q001D3Q000600302Q001D000400A800302Q001D0022002D00302Q001D002400A900302Q001D000B002D00302Q001D002600274Q001E00023Q00202Q001E001E00AA00102Q001D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000300302Q001D000400AB00302Q001D000B000C4Q001E00023Q00202Q001E001E00AC00102Q001D000D001E4Q001B001D000100202Q001B3Q00034Q001D3Q000200302Q001D000400AD00302Q001D000600AE4Q001B001D000200202Q001C001B006A4Q001E3Q000400302Q001E000400AF00302Q001E000600B04Q001F00033Q00202Q001F001F00B100102Q001E003C001F00302Q001E006C00B24Q001C001E000200202Q001D001B006A4Q001F3Q000300302Q001F000400B300302Q001F000600B400302Q001F006C00B54Q001D001F000200202Q001E001B006A4Q00203Q000300302Q0020000400B600302Q0020000600B700302Q0020006C00B84Q001E0020000200202Q001F001B006A4Q00213Q000300302Q0021000400B900302Q0021000600BA00302Q0021006C00B24Q001F0021000200202Q0020001B006A4Q00223Q000300302Q0022000400BB00302Q0022000600BC00302Q0022006C00B24Q0020002200020020130021001B00752Q002D00233Q00030030C70023000400BD0030C70023000600770006A60124000D000100062Q004A012Q00074Q004A012Q00084Q004A012Q00094Q00A83Q001E4Q00A83Q00204Q00A83Q001F3Q0010150123000D00242Q003B0121002300010012590121003E3Q00203C01210021003F0006A60122000E0001000D2Q004A012Q00044Q00A83Q001C4Q004A012Q000A4Q004A012Q000B4Q00A83Q001D4Q004A012Q00074Q00A83Q001E4Q004A012Q000C4Q004A012Q00094Q004A012Q00084Q00A83Q00204Q004A012Q000D4Q00A83Q001F4Q00A50121000200012Q00BD3Q00013Q000F3Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03083Q00486974734C65667403073Q004D61784869747303043Q004E616D652Q0103053Q0056616C7565030B3Q004372656174654C6162656C03083Q00746F737472696E672Q033Q00202F202Q033Q0053657403053Q00706169727303083Q00496E7374616E636503073Q0044657374726F792Q00614Q004A016Q00203C014Q000100062C012Q006000013Q00047B012Q00600001001259012Q00023Q0020A4014Q000300122Q000100048Q0002000100124Q00053Q00206Q000600122Q000200078Q000200024Q00015Q00064Q004E00013Q00047B012Q004E0001001259010200083Q00201300033Q00092Q0076000300044Q005700023Q000400047B012Q004C000100201300070006000A0012C30009000B4Q003900070009000200062C0107004C00013Q00047B012Q004C00010020130007000600060012E60009000C6Q00070009000200202Q00080006000600122Q000A000D6Q0008000A000200062Q0007004C00013Q00047B012Q004C000100062C0108004C00013Q00047B012Q004C000100203C01090006000E0020442Q010009000F00203C01090007001000203C010A000800102Q004A010B00013Q00203C010C0006000E2Q0051000B000B000C0006E7000B003F0001000100047B012Q003F00012Q004A010B00013Q0020E8000C0006000E4Q000D00023Q00202Q000D000D00114Q000F3Q000200202Q00100006000E00102Q000F000E001000122Q001000126Q001100096Q00100002000200122Q001100133Q00122Q001200126Q0013000A6Q0012000200024Q00100010001200102Q000F001000104Q000D000F00024Q000B000C000D00044Q004C00012Q004A010B00013Q0020DC000C0006000E4Q000B000B000C00202Q000B000B001400122Q000D00126Q000E00096Q000D0002000200122Q000E00133Q00122Q000F00126Q0010000A6Q000F000200024Q000D000D000F4Q000B000D00010006F7000200140001000200047B012Q00140001001259010200154Q004A010300014Q003100020002000400047B012Q005D00012Q00510007000100050006E70007005D0001000100047B012Q005D000100203C01070006001600062C0107005B00013Q00047B012Q005B000100203C0107000600160020130007000700172Q00A50107000200012Q004A010700013Q0020440107000500180006F7000200520001000200047B012Q0052000100047B014Q00012Q00BD3Q00017Q00083Q0003053Q00416C697665030D3Q00676574496E646578436F756E742Q033Q005365742Q033Q00202F2003143Q004F70656E2050657420496E64657820666972737403043Q007461736B03043Q0077616974027Q004000194Q004A016Q00203C014Q000100062C012Q001800013Q00047B012Q001800012Q004A012Q00013Q0020F05Q00026Q000100014Q000200023Q00202Q00020002000300064Q001100013Q00047B012Q001100012Q00A800045Q0012C3000500044Q00A8000600014Q003D0104000400060006E7000400120001000100047B012Q001200010012C3000400054Q003B010200040001001207000200063Q00202Q00020002000700122Q000300086Q00020002000100046Q00012Q00BD3Q00017Q00123Q002Q033Q00536574030A3Q007461726765745465787403063Q00697061697273030A3Q006973546172676574656403043Q006E616D652Q033Q00726F7703153Q0046696E6446697273744368696C644F66436C612Q7303083Q0055495374726F6B6503053Q00436F6C6F7203063Q00412Q63656E7403063Q005374726F6B65030C3Q005472616E73706172656E6379029A5Q99B93F03163Q0046696E6446697273744368696C64576869636849734103093Q00546578744C6162656C030A3Q0054657874436F6C6F7233030A3Q004C6967687447722Q656E03043Q005465787400374Q004A016Q0020135Q00012Q004A010200013Q00203C0102000200022Q0054010200014Q008E014Q0001001259012Q00034Q004A2Q0100024Q00313Q0002000200047B012Q003400012Q004A010500013Q00208D00050005000400202Q0006000400054Q00050002000200202Q00060004000600202Q00060006000700122Q000800086Q00060008000200062Q0006002400013Q00047B012Q0024000100062C0105001A00013Q00047B012Q001A00012Q004A010700033Q00203C01070007000A0006E70007001C0001000100047B012Q001C00012Q004A010700033Q00203C01070007000B00101501060009000700062C0105002200013Q00047B012Q002200010012C30007000D3Q0006E7000700230001000100047B012Q002300012Q004A010700043Q0010150106000C000700203C0107000400060020D100070007000E00122Q0009000F6Q000A00016Q0007000A000200062Q0007003400013Q00047B012Q0034000100062C0105003100013Q00047B012Q003100012Q004A010800033Q00203C0108000800110006E7000800330001000100047B012Q003300012Q004A010800033Q00203C0108000800120010150107001000080006F73Q000A0001000200047B012Q000A00012Q00BD3Q00017Q000B3Q0003063Q00737472696E6703053Q006C6F7765722Q033Q00476574034Q0003063Q006970616972732Q033Q00726F7703073Q0056697369626C6503043Q0066696E6403043Q006E616D65026Q00F03F2Q00283Q001259012Q00013Q00203C014Q00022Q004A2Q015Q00062C2Q01000A00013Q00047B012Q000A00012Q004A2Q015Q00203C2Q01000100032Q00FD0001000100020006E70001000B0001000100047B012Q000B00010012C3000100044Q00B53Q000200020012592Q0100054Q004A010200014Q003100010002000300047B012Q0025000100203C0106000500062Q004A010700023Q00062C0107002400013Q00047B012Q00240001002693012Q00230001000400047B012Q00230001001259010700013Q00203C010700070008001259010800013Q00203C01080008000200203C0109000500092Q00B50008000200022Q00A800095Q0012C3000A000A4Q0080010B00014Q00390007000B00020026D3000700230001000B00047B012Q002300012Q00BF00076Q0080010700013Q0010150106000700070006F7000100100001000200047B012Q001000012Q00BD3Q00017Q00033Q00030C3Q00536574526967687449636F6E030E3Q00612Q726F772D6C617267652D757003103Q00612Q726F772D6C617267652D646F776E01104Q004A8Q004A2Q0100013Q00062C2Q01000D00013Q00047B012Q000D00012Q004A2Q0100013Q00201300010001000100062C012Q000B00013Q00047B012Q000B00010012C3000300023Q0006E70003000C0001000100047B012Q000C00010012C3000300034Q003B2Q01000300012Q004A2Q0100024Q00F80001000100012Q00BD3Q00017Q00013Q00034Q0001093Q002693012Q00060001000100047B012Q000600012Q004A2Q016Q0080010200014Q00A52Q010002000100047B012Q000800012Q004A2Q0100014Q00F80001000100012Q00BD3Q00017Q00133Q0003063Q006970616972732Q033Q00726F7703073Q0044657374726F79030B3Q006765745065744E616D65732Q033Q0053657403153Q004F70656E2050657420496E6465782066697273742103073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564030C3Q0043726561746542752Q746F6E03043Q004E616D6503083Q0043612Q6C6261636B03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203053Q007461626C6503063Q00696E7365727403043Q006E616D6500483Q001259012Q00014Q004A2Q016Q00313Q0002000200047B012Q000A000100203C01050004000200062C0105000A00013Q00047B012Q000A000100203C0105000400020020130005000500032Q00A50105000200010006F73Q00040001000200047B012Q000400012Q002D8Q00179Q003Q00013Q00206Q00046Q0001000200064Q00180001000100047B012Q001800012Q004A2Q0100023Q0020130001000100050012C3000300064Q003B2Q01000300012Q00BD3Q00014Q004A2Q0100023Q00207F2Q010001000500122Q000300076Q00030001000200202Q0003000300084Q000300033Q00122Q000400096Q000500013Q00202Q00050005000A00122Q0006000B6Q0003000300062Q003B2Q01000300010012592Q0100014Q00A800026Q003100010002000300047B012Q004100012Q004A010600033Q00201300060006000C2Q002D00083Q00020010150108000D00050006A601093Q000100052Q004A012Q00044Q00A83Q00054Q004A012Q00024Q004A012Q00014Q004A012Q00053Q0010400008000E00094Q00060008000200202Q00070006000F4Q000800066Q00080008000400102Q00070010000800122Q000700113Q00202Q0007000700124Q00088Q00093Q000200101501090013000500203C010A0006000F00101501090002000A2Q003B0107000900012Q00AA01045Q0006F7000100280001000200047B012Q002800012Q004A2Q0100074Q00F80001000100012Q004A2Q0100054Q00F80001000100012Q00BD3Q00013Q00013Q00073Q0003143Q00546F2Q676C654D65726368616E745461726765742Q033Q0053657403073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564001A4Q0070016Q00206Q00014Q000100018Q0002000100064Q001300013Q00047B012Q001300012Q004A010200023Q00204101020002000200122Q000400036Q00040001000200202Q0004000400044Q000400043Q00122Q000500056Q000600033Q00202Q00060006000600122Q000700076Q0004000400074Q00020004000100044Q001700012Q004A010200023Q0020130002000200022Q00A8000400014Q003B0102000400012Q004A010200044Q00F80002000100012Q00BD3Q00017Q00053Q0003143Q00436C6561724D65726368616E74546172676574732Q033Q0053657403023Q00302F2Q033Q006D617803093Q00207461726765746564000E4Q004A016Q00203C014Q00012Q00F83Q000100012Q004A012Q00013Q0020135Q00020012C3000200034Q004A010300023Q00203C0103000300040012C3000400054Q003D0102000200042Q003B012Q000200012Q004A012Q00034Q00F83Q000100012Q00BD3Q00019Q003Q00054Q004A017Q004A2Q0100016Q000100014Q00A5012Q000200012Q00BD3Q00017Q00013Q0003073Q005F73656C65637400044Q004A016Q00203C014Q00012Q00F83Q000100012Q00BD3Q00017Q00013Q0003073Q005F73656C65637400044Q004A016Q00203C014Q00012Q00F83Q000100012Q00BD3Q00017Q00093Q0003093Q0067657452656D6F746503093Q00617661696C61626C652Q033Q00536574030D3Q004E6F7420617661696C61626C6503103Q0052656D6F7465206E6F7420666F756E6403053Q007063612Q6C03073Q00426F756768742003043Q006E616D65030A3Q00427579206661696C656400284Q004A017Q004A2Q0100014Q00515Q00012Q004A2Q0100023Q00203C2Q01000100012Q00FD00010001000200062C012Q000B00013Q00047B012Q000B000100203C01023Q00020006E7000200100001000100047B012Q001000012Q004A010200033Q0020130002000200030012C3000400044Q003B01020004000100047B012Q002700010006E7000100170001000100047B012Q001700012Q004A010200033Q0020130002000200030012C3000400054Q003B01020004000100047B012Q00270001001259010200063Q0006A601033Q000100022Q00A83Q00014Q004A012Q00014Q00B50002000200022Q004A010300033Q00201300030003000300062C0102002500013Q00047B012Q002500010012C3000500073Q00203C01063Q00082Q003D0105000500060006E7000500260001000100047B012Q002600010012C3000500094Q003B0103000500012Q00BD3Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q005D016Q00206Q00014Q000200018Q000200016Q00017Q000C3Q0003053Q00416C69766503093Q006765744F2Q6665727303053Q0070616972732Q033Q0053657403043Q006E616D6503053Q00707269636503093Q00617661696C61626C6503203Q004F70656E20746865204D65726368616E7420746F206C6F6164206F2Q66657273034Q0003043Q007461736B03043Q0077616974026Q00E03F00254Q004A016Q00203C014Q000100062C012Q002400013Q00047B012Q002400012Q004A012Q00013Q00209C5Q00026Q0001000200122Q000100036Q000200026Q00010002000300044Q001D00010006DE0006000E00013Q00047B012Q000E00012Q005100063Q00042Q004A010700034Q000B01070004000600062C0106001800013Q00047B012Q0018000100201300070005000400203C01090006000500203C010A0006000600203C010B000600072Q003B0107000B000100047B012Q001D00010020130007000500040012C3000900083Q0012C3000A00094Q0080010B6Q003B0107000B00010006F70001000B0001000200047B012Q000B00010012592Q01000A3Q00203C2Q010001000B0012C30002000C4Q00A52Q010002000100047B014Q00012Q00BD3Q00017Q00063Q0003023Q006F7303043Q0074696D65028Q002Q033Q0053657403083Q002Q303A2Q303A2Q3003013Q003000153Q00124D012Q00013Q00206Q00026Q000100029Q0000124Q00038Q00019Q008Q00028Q00033Q00206Q000400122Q000200058Q000200016Q00043Q00206Q000400122Q000200068Q000200016Q00053Q00206Q000400122Q000200068Q000200016Q00017Q00153Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00F03F2Q033Q0053657403083Q00746F737472696E67028Q0003053Q007063612Q6C2Q033Q00206D7303023Q006F7303043Q0074696D6503043Q006D61746803053Q00666C2Q6F72025Q0020AC40026Q004E4003063Q00737472696E6703063Q00666F726D6174030E3Q00253032643A253032643A2530326403053Q0056616C75650003013Q0030005E4Q004A016Q00203C014Q000100062C012Q005D00013Q00047B012Q005D0001001259012Q00023Q00208B5Q000300122Q000100048Q000200016Q00013Q00206Q000500122Q000200066Q000300026Q000200039Q00000100124Q00073Q00122Q000100083Q0006A601023Q000100022Q00A88Q004A012Q00034Q00010001000200014Q000100043Q00202Q00010001000500122Q000300066Q00048Q00030002000200122Q000400096Q0003000300044Q00010003000100122Q0001000A3Q00203C2Q010001000B2Q00FD0001000100022Q004A010200054Q00380001000100020012590102000C3Q00203C01020002000D0020CE00030001000E2Q00B50002000200020012590103000C3Q00203C01030003000D00202900040001000E0020CE00040004000F2Q002100030002000200202Q00040001000F4Q000500063Q00202Q00050005000500122Q000700103Q00202Q00070007001100122Q000800126Q000900026Q000A00036Q000B00044Q00990107000B4Q008E01053Q00012Q004A010500074Q00FD00050001000200062C0105005B00013Q00047B012Q005B000100203C0106000500132Q004A010700083Q002693010700440001001400047B012Q004400012Q004A010700084Q0038000700060007000EF1000700440001000700047B012Q004400012Q004A010800094Q00630008000800072Q004A000800094Q004A000600084Q00510107000A3Q00202Q0007000700054Q0009000B6Q000A00096Q0009000A6Q00073Q0001000E2Q000700570001000100047B012Q005700012Q004A010700094Q00C20007000700010020A301070007000F2Q004A0108000C3Q0020130008000800052Q004A010A000B4Q00A8000B00074Q0076000A000B4Q008E01083Q000100047B012Q005B00012Q004A0107000C3Q0020130007000700050012C3000900154Q003B0107000900012Q00AA016Q00047B014Q00012Q00BD3Q00013Q00013Q00043Q0003043Q006D61746803053Q00666C2Q6F72030E3Q004765744E6574776F726B50696E67025Q00408F4000093Q0012833Q00013Q00206Q00024Q000100013Q00202Q0001000100034Q00010002000200202Q0001000100046Q000200029Q006Q00017Q00", GetFEnv(), ...);
