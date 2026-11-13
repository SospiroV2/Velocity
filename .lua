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
											elseif (Enum == 1) then
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
												local A = Inst[2];
												Stk[A] = Stk[A]();
											end
										elseif (Enum <= 3) then
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
										elseif (Enum == 4) then
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
									elseif (Enum <= 8) then
										if (Enum <= 6) then
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
										elseif (Enum == 7) then
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
											Stk[Inst[2]][Inst[3]] = Inst[4];
										end
									elseif (Enum <= 10) then
										if (Enum > 9) then
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
										else
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
										end
									elseif (Enum == 11) then
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
									else
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
									end
								elseif (Enum <= 18) then
									if (Enum <= 15) then
										if (Enum <= 13) then
											local A = Inst[2];
											do
												return Stk[A], Stk[A + 1];
											end
										elseif (Enum > 14) then
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
									elseif (Enum > 17) then
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
									end
								elseif (Enum <= 21) then
									if (Enum <= 19) then
										local A = Inst[2];
										local T = Stk[A];
										local B = Inst[3];
										for Idx = 1, B do
											T[Idx] = Stk[A + Idx];
										end
									elseif (Enum == 20) then
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
									elseif (Stk[Inst[2]] < Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum <= 23) then
									if (Enum > 22) then
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
									end
								elseif (Enum > 24) then
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
							elseif (Enum <= 38) then
								if (Enum <= 31) then
									if (Enum <= 28) then
										if (Enum <= 26) then
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
										elseif (Enum == 27) then
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
											do
												return;
											end
										end
									elseif (Enum <= 29) then
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
									elseif (Enum == 30) then
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
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									end
								elseif (Enum <= 34) then
									if (Enum <= 32) then
										local A = Inst[2];
										Stk[A] = Stk[A](Stk[A + 1]);
									elseif (Enum > 33) then
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
								elseif (Enum <= 36) then
									if (Enum == 35) then
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
								elseif (Enum == 37) then
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
							elseif (Enum <= 45) then
								if (Enum <= 41) then
									if (Enum <= 39) then
										Stk[Inst[2]][Inst[3]] = Inst[4];
									elseif (Enum > 40) then
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
										local A = Inst[2];
										local Results = {Stk[A](Unpack(Stk, A + 1, Top))};
										local Edx = 0;
										for Idx = A, Inst[4] do
											Edx = Edx + 1;
											Stk[Idx] = Results[Edx];
										end
									end
								elseif (Enum <= 43) then
									if (Enum > 42) then
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
									end
								elseif (Enum == 44) then
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
									Stk[Inst[2]] = Inst[3];
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
							elseif (Enum <= 50) then
								if (Enum > 49) then
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
							elseif (Enum == 51) then
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
						elseif (Enum <= 79) then
							if (Enum <= 65) then
								if (Enum <= 58) then
									if (Enum <= 55) then
										if (Enum <= 53) then
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
										elseif (Enum > 54) then
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
										end
									elseif (Enum <= 56) then
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
									elseif (Enum > 57) then
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
								elseif (Enum <= 61) then
									if (Enum <= 59) then
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
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									elseif (Enum > 60) then
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
										Stk[Inst[2]][Inst[3]] = Inst[4];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									end
								elseif (Enum <= 63) then
									if (Enum == 62) then
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
								elseif (Enum == 64) then
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
								end
							elseif (Enum <= 72) then
								if (Enum <= 68) then
									if (Enum <= 66) then
										Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
									elseif (Enum == 67) then
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
									end
								elseif (Enum <= 70) then
									if (Enum == 69) then
										local A = Inst[2];
										do
											return Stk[A](Unpack(Stk, A + 1, Inst[3]));
										end
									elseif not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 71) then
									local A = Inst[2];
									do
										return Unpack(Stk, A, Top);
									end
								else
									local Edx;
									local Results;
									local B;
									local A;
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
									A = Inst[2];
									Results = {Stk[A](Stk[A + 1])};
									Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								end
							elseif (Enum <= 75) then
								if (Enum <= 73) then
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
								elseif (Enum > 74) then
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
							elseif (Enum <= 77) then
								if (Enum == 76) then
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
							elseif (Enum == 78) then
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
							else
								local A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							end
						elseif (Enum <= 92) then
							if (Enum <= 85) then
								if (Enum <= 82) then
									if (Enum <= 80) then
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
									elseif (Enum > 81) then
										Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
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
								elseif (Enum <= 83) then
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
									A = Inst[2];
									Stk[A] = Stk[A]();
									VIP = VIP + 1;
									Inst = Instr[VIP];
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum > 84) then
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
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
							elseif (Enum <= 88) then
								if (Enum <= 86) then
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
								elseif (Enum == 87) then
									if (Stk[Inst[2]] < Inst[4]) then
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
							elseif (Enum <= 90) then
								if (Enum == 89) then
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
									A = Inst[2];
									Stk[A](Stk[A + 1]);
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
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
									Stk[Inst[2]] = Inst[3];
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
									Stk[Inst[2]] = Inst[3];
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
									Stk[Inst[2]] = Inst[3];
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
									Stk[Inst[2]] = Inst[3];
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
								end
							elseif (Enum > 91) then
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
						elseif (Enum <= 99) then
							if (Enum <= 95) then
								if (Enum <= 93) then
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
								elseif (Enum > 94) then
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
							elseif (Enum <= 97) then
								if (Enum == 96) then
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
								end
							elseif (Enum == 98) then
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
							elseif (Enum > 101) then
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
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							end
						elseif (Enum <= 104) then
							if (Enum == 103) then
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
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
						elseif (Enum == 105) then
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
							Stk[Inst[2]] = Inst[3];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
						else
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
						end
					elseif (Enum <= 159) then
						if (Enum <= 132) then
							if (Enum <= 119) then
								if (Enum <= 112) then
									if (Enum <= 109) then
										if (Enum <= 107) then
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
										elseif (Enum == 108) then
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
											A = Inst[2];
											Stk[A] = Stk[A]();
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
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
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
								elseif (Enum <= 115) then
									if (Enum <= 113) then
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
									elseif (Enum == 114) then
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
								elseif (Enum <= 117) then
									if (Enum > 116) then
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
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										do
											return;
										end
									elseif (Stk[Inst[2]] <= Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 118) then
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
							elseif (Enum <= 125) then
								if (Enum <= 122) then
									if (Enum <= 120) then
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
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										VIP = Inst[3];
									elseif (Enum == 121) then
										do
											return Stk[Inst[2]];
										end
									else
										Stk[Inst[2]] = #Stk[Inst[3]];
									end
								elseif (Enum <= 123) then
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
								elseif (Enum > 124) then
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
									Stk[Inst[2]] = Inst[3];
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
								end
							elseif (Enum <= 128) then
								if (Enum <= 126) then
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
								elseif (Enum > 127) then
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
							elseif (Enum <= 130) then
								if (Enum > 129) then
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
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								else
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
								end
							elseif (Enum > 131) then
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
								Stk[Inst[2]] = Env[Inst[3]];
							end
						elseif (Enum <= 145) then
							if (Enum <= 138) then
								if (Enum <= 135) then
									if (Enum <= 133) then
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
										B = Stk[Inst[4]];
										if B then
											VIP = VIP + 1;
										else
											Stk[Inst[2]] = B;
											VIP = Inst[3];
										end
									elseif (Enum == 134) then
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
								elseif (Enum <= 136) then
									if (Stk[Inst[2]] == Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 137) then
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
							elseif (Enum <= 141) then
								if (Enum <= 139) then
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
								elseif (Enum == 140) then
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
									Stk[Inst[2]] = Stk[Inst[3]][Stk[Inst[4]]];
								end
							elseif (Enum <= 143) then
								if (Enum > 142) then
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
							elseif (Enum > 144) then
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
						elseif (Enum <= 152) then
							if (Enum <= 148) then
								if (Enum <= 146) then
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
								elseif (Enum > 147) then
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
							elseif (Enum <= 150) then
								if (Enum > 149) then
									local A;
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
								else
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
								end
							elseif (Enum > 151) then
								Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
							else
								local A = Inst[2];
								local T = Stk[A];
								for Idx = A + 1, Inst[3] do
									Insert(T, Stk[Idx]);
								end
							end
						elseif (Enum <= 155) then
							if (Enum <= 153) then
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
							elseif (Enum > 154) then
								local A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
							else
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
							end
						elseif (Enum <= 157) then
							if (Enum > 156) then
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
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
						elseif (Enum == 158) then
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
							Stk[Inst[2]] = Inst[3] ~= 0;
						end
					elseif (Enum <= 186) then
						if (Enum <= 172) then
							if (Enum <= 165) then
								if (Enum <= 162) then
									if (Enum <= 160) then
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
									elseif (Enum == 161) then
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
									else
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
									end
								elseif (Enum <= 163) then
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
								elseif (Enum == 164) then
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
								else
									Stk[Inst[2]] = Inst[3] ~= 0;
									VIP = VIP + 1;
								end
							elseif (Enum <= 168) then
								if (Enum <= 166) then
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
								elseif (Enum > 167) then
									Stk[Inst[2]] = not Stk[Inst[3]];
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
							elseif (Enum <= 170) then
								if (Enum > 169) then
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
								else
									local Edx;
									local Limit;
									local Results;
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
							elseif (Enum == 171) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
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
						elseif (Enum <= 179) then
							if (Enum <= 175) then
								if (Enum <= 173) then
									Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
								elseif (Enum > 174) then
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
								else
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
								end
							elseif (Enum <= 177) then
								if (Enum > 176) then
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
								end
							elseif (Enum == 178) then
								local A = Inst[2];
								local Results, Limit = _R(Stk[A](Stk[A + 1]));
								Top = (Limit + A) - 1;
								local Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
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
						elseif (Enum <= 182) then
							if (Enum <= 180) then
								do
									return Stk[Inst[2]]();
								end
							elseif (Enum > 181) then
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
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							elseif (Stk[Inst[2]] ~= Inst[4]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum <= 184) then
							if (Enum > 183) then
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
						elseif (Enum == 185) then
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
							if (Inst[2] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
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
					elseif (Enum <= 199) then
						if (Enum <= 192) then
							if (Enum <= 189) then
								if (Enum <= 187) then
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
								elseif (Enum == 188) then
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
							elseif (Enum <= 190) then
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
							elseif (Enum == 191) then
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
							end
						elseif (Enum <= 195) then
							if (Enum <= 193) then
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
							elseif (Enum == 194) then
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
							else
								Stk[Inst[2]] = Wrap(Proto[Inst[3]], nil, Env);
							end
						elseif (Enum <= 197) then
							if (Enum == 196) then
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
							end
						elseif (Enum > 198) then
							local Edx;
							local Results;
							local A;
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
							A = Inst[2];
							Stk[A] = Stk[A]();
						end
					elseif (Enum <= 206) then
						if (Enum <= 202) then
							if (Enum <= 200) then
								if (Inst[2] <= Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum > 201) then
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
								Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
							end
						elseif (Enum <= 204) then
							if (Enum == 203) then
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
							else
								local A;
								A = Inst[2];
								Stk[A](Stk[A + 1]);
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A]();
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							end
						elseif (Enum == 205) then
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
					elseif (Enum <= 209) then
						if (Enum <= 207) then
							Upvalues[Inst[3]] = Stk[Inst[2]];
						elseif (Enum == 208) then
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
					elseif (Enum <= 211) then
						if (Enum > 210) then
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
					elseif (Enum > 212) then
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
						local A = Inst[2];
						local Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Inst[3])));
						Top = (Limit + A) - 1;
						local Edx = 0;
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
										elseif (Enum > 215) then
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
											local Edx;
											local Results, Limit;
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
										end
									elseif (Enum <= 217) then
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
										Stk[Inst[2]] = Inst[3];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Env[Inst[3]];
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
									elseif (Enum == 218) then
										if (Stk[Inst[2]] > Inst[4]) then
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
									end
								elseif (Enum <= 222) then
									if (Enum <= 220) then
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
									elseif (Enum > 221) then
										Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
									else
										local A = Inst[2];
										do
											return Stk[A](Unpack(Stk, A + 1, Top));
										end
									end
								elseif (Enum <= 224) then
									if (Enum > 223) then
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
								elseif (Enum == 225) then
									local A = Inst[2];
									local Results = {Stk[A](Stk[A + 1])};
									local Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
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
									A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									do
										return;
									end
								end
							elseif (Enum <= 232) then
								if (Enum <= 229) then
									if (Enum <= 227) then
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
									elseif (Enum == 228) then
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
									end
								elseif (Enum <= 230) then
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
								elseif (Enum > 231) then
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
								else
									local B;
									local A;
									A = Inst[2];
									B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
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
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Env[Inst[3]];
								end
							elseif (Enum <= 235) then
								if (Enum <= 233) then
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
								elseif (Enum == 234) then
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
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 237) then
								if (Enum > 236) then
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
							elseif (Enum > 238) then
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
						elseif (Enum <= 252) then
							if (Enum <= 245) then
								if (Enum <= 242) then
									if (Enum <= 240) then
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
									elseif (Enum > 241) then
										if (Stk[Inst[2]] ~= Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									else
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
									end
								elseif (Enum <= 243) then
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
								elseif (Enum == 244) then
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
									Stk[Inst[2]] = Upvalues[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
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
								end
							elseif (Enum <= 248) then
								if (Enum <= 246) then
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
								elseif (Enum > 247) then
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
							elseif (Enum <= 250) then
								if (Enum > 249) then
									local B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								else
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
								end
							elseif (Enum == 251) then
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
						elseif (Enum <= 259) then
							if (Enum <= 255) then
								if (Enum <= 253) then
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
								elseif (Enum > 254) then
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
							elseif (Enum <= 257) then
								if (Enum == 256) then
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
							elseif (Enum > 258) then
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
						elseif (Enum <= 262) then
							if (Enum <= 260) then
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
							elseif (Enum == 261) then
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
							end
						elseif (Enum <= 264) then
							if (Enum == 263) then
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
								Stk[Inst[2]] = {};
							end
						elseif (Enum > 265) then
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
					elseif (Enum <= 293) then
						if (Enum <= 279) then
							if (Enum <= 272) then
								if (Enum <= 269) then
									if (Enum <= 267) then
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
										A = Inst[2];
										Stk[A] = Stk[A]();
										VIP = VIP + 1;
										Inst = Instr[VIP];
										if Stk[Inst[2]] then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 268) then
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
								elseif (Enum <= 270) then
									local A = Inst[2];
									local Results, Limit = _R(Stk[A]());
									Top = (Limit + A) - 1;
									local Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
									end
								elseif (Enum == 271) then
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
							elseif (Enum <= 275) then
								if (Enum <= 273) then
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
								elseif (Enum > 274) then
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
									local A = Inst[2];
									local B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Inst[4]];
								end
							elseif (Enum <= 277) then
								if (Enum == 276) then
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
							elseif (Enum > 278) then
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
						elseif (Enum <= 286) then
							if (Enum <= 282) then
								if (Enum <= 280) then
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
									Stk[Inst[2]] = Inst[3];
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
								elseif (Enum == 281) then
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
								end
							elseif (Enum <= 284) then
								if (Enum > 283) then
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
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
									VIP = VIP + 1;
									Inst = Instr[VIP];
									VIP = Inst[3];
								else
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
								end
							elseif (Enum == 285) then
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
						elseif (Enum <= 289) then
							if (Enum <= 287) then
								local A;
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
							elseif (Enum > 288) then
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
								VIP = VIP + 1;
								Inst = Instr[VIP];
								if (Inst[2] < Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
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
						elseif (Enum <= 291) then
							if (Enum == 290) then
								local A;
								Stk[Inst[2]] = Stk[Inst[3]];
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
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
							else
								local A = Inst[2];
								local B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Stk[Inst[4]]];
							end
						elseif (Enum == 292) then
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
							VIP = Inst[3];
						end
					elseif (Enum <= 306) then
						if (Enum <= 299) then
							if (Enum <= 296) then
								if (Enum <= 294) then
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
								elseif (Enum > 295) then
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
									Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
								end
							elseif (Enum <= 297) then
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
							elseif (Enum == 298) then
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
								local A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
							end
						elseif (Enum <= 302) then
							if (Enum <= 300) then
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							elseif (Enum == 301) then
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
								Stk[Inst[2]][Inst[3]] = Inst[4];
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
								Stk[Inst[2]] = Stk[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
							end
						elseif (Enum <= 304) then
							if (Enum == 303) then
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
									if (Mvm[1] == 341) then
										Indexes[Idx - 1] = {Stk,Mvm[3]};
									else
										Indexes[Idx - 1] = {Upvalues,Mvm[3]};
									end
									Lupvals[#Lupvals + 1] = Indexes;
								end
								Stk[Inst[2]] = Wrap(NewProto, NewUvals, Env);
							else
								local A;
								A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
						elseif (Enum > 305) then
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
							local B = Inst[3];
							local K = Stk[B];
							for Idx = B + 1, Inst[4] do
								K = K .. Stk[Idx];
							end
							Stk[Inst[2]] = K;
						end
					elseif (Enum <= 313) then
						if (Enum <= 309) then
							if (Enum <= 307) then
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
							elseif (Enum == 308) then
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
							end
						elseif (Enum <= 311) then
							if (Enum == 310) then
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
							end
						elseif (Enum > 312) then
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
						elseif Stk[Inst[2]] then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
					elseif (Enum <= 316) then
						if (Enum <= 314) then
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
						elseif (Enum == 315) then
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
						end
					elseif (Enum <= 318) then
						if (Enum > 317) then
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
					elseif (Enum > 319) then
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
				elseif (Enum <= 373) then
					if (Enum <= 346) then
						if (Enum <= 333) then
							if (Enum <= 326) then
								if (Enum <= 323) then
									if (Enum <= 321) then
										if (Stk[Inst[2]] <= Inst[4]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum == 322) then
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
									elseif (Inst[2] < Stk[Inst[4]]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum <= 324) then
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
								elseif (Enum > 325) then
									local A = Inst[2];
									local T = Stk[A];
									for Idx = A + 1, Top do
										Insert(T, Stk[Idx]);
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
							elseif (Enum <= 329) then
								if (Enum <= 327) then
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
								elseif (Enum == 328) then
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
							elseif (Enum <= 331) then
								if (Enum > 330) then
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
								end
							elseif (Enum > 332) then
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
						elseif (Enum <= 339) then
							if (Enum <= 336) then
								if (Enum <= 334) then
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
								elseif (Enum > 335) then
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
							elseif (Enum <= 337) then
								local B;
								local A;
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
							elseif (Enum == 338) then
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
								Stk[Inst[2]] = Inst[3];
							end
						elseif (Enum <= 342) then
							if (Enum <= 340) then
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
							elseif (Enum == 341) then
								Stk[Inst[2]] = Stk[Inst[3]];
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
						elseif (Enum <= 344) then
							if (Enum == 343) then
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
						elseif (Enum > 345) then
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
						end
					elseif (Enum <= 359) then
						if (Enum <= 352) then
							if (Enum <= 349) then
								if (Enum <= 347) then
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
								elseif (Enum > 348) then
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
									Stk[Inst[2]] = Inst[3] / Stk[Inst[4]];
								end
							elseif (Enum <= 350) then
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
							elseif (Enum > 351) then
								Stk[Inst[2]] = Upvalues[Inst[3]];
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
						elseif (Enum <= 355) then
							if (Enum <= 353) then
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
							elseif (Enum > 354) then
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
						elseif (Enum <= 357) then
							if (Enum == 356) then
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
							end
						elseif (Enum > 358) then
							local K;
							local B;
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
					elseif (Enum <= 366) then
						if (Enum <= 362) then
							if (Enum <= 360) then
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
							elseif (Enum > 361) then
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
						elseif (Enum <= 364) then
							if (Enum > 363) then
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
							end
						elseif (Enum > 365) then
							for Idx = Inst[2], Inst[3] do
								Stk[Idx] = nil;
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
					elseif (Enum <= 369) then
						if (Enum <= 367) then
							local A = Inst[2];
							do
								return Unpack(Stk, A, A + Inst[3]);
							end
						elseif (Enum == 368) then
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
					elseif (Enum <= 371) then
						if (Enum == 370) then
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
					elseif (Enum > 372) then
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
				elseif (Enum <= 400) then
					if (Enum <= 386) then
						if (Enum <= 379) then
							if (Enum <= 376) then
								if (Enum <= 374) then
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
								elseif (Enum == 375) then
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
							elseif (Enum <= 377) then
								local A;
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
								VIP = VIP + 1;
								Inst = Instr[VIP];
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
							elseif (Enum > 378) then
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
							end
						elseif (Enum <= 382) then
							if (Enum <= 380) then
								if (Stk[Inst[2]] == Inst[4]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum == 381) then
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
							end
						elseif (Enum <= 384) then
							if (Enum > 383) then
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
						elseif (Enum > 385) then
							Stk[Inst[2]] = -Stk[Inst[3]];
						else
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
						end
					elseif (Enum <= 393) then
						if (Enum <= 389) then
							if (Enum <= 387) then
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
							elseif (Enum == 388) then
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
						elseif (Enum <= 391) then
							if (Enum > 390) then
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
						elseif (Enum == 392) then
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
					elseif (Enum <= 396) then
						if (Enum <= 394) then
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
						elseif (Enum == 395) then
							local B = Stk[Inst[4]];
							if not B then
								VIP = VIP + 1;
							else
								Stk[Inst[2]] = B;
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
					elseif (Enum <= 398) then
						if (Enum == 397) then
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
						else
							local A;
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
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Upvalues[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							Stk[A] = Stk[A]();
						end
					elseif (Enum > 399) then
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
				elseif (Enum <= 413) then
					if (Enum <= 406) then
						if (Enum <= 403) then
							if (Enum <= 401) then
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
							elseif (Enum == 402) then
								local A = Inst[2];
								Stk[A](Unpack(Stk, A + 1, Top));
							else
								local A;
								Stk[Inst[2]] = {};
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
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
							end
						elseif (Enum <= 404) then
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
						elseif (Enum > 405) then
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
					elseif (Enum <= 409) then
						if (Enum <= 407) then
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
						elseif (Enum == 408) then
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
					elseif (Enum <= 411) then
						if (Enum == 410) then
							Stk[Inst[2]]();
						else
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
						end
					elseif (Enum > 412) then
						Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
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
						A = Inst[2];
						T = Stk[A];
						B = Inst[3];
						for Idx = 1, B do
							T[Idx] = Stk[A + Idx];
						end
					end
				elseif (Enum <= 420) then
					if (Enum <= 416) then
						if (Enum <= 414) then
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
						elseif (Enum > 415) then
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
						else
							local Edx;
							local Results, Limit;
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
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
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
							Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
							VIP = VIP + 1;
							Inst = Instr[VIP];
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
					elseif (Enum <= 418) then
						if (Enum == 417) then
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
					elseif (Enum == 419) then
						local A;
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
						local A = Inst[2];
						local Results = {Stk[A]()};
						local Limit = Inst[4];
						local Edx = 0;
						for Idx = A, Limit do
							Edx = Edx + 1;
							Stk[Idx] = Results[Edx];
						end
					end
				elseif (Enum <= 423) then
					if (Enum <= 421) then
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
					elseif (Enum > 422) then
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
					else
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
					end
				elseif (Enum <= 425) then
					if (Enum == 424) then
						local A = Inst[2];
						Stk[A](Stk[A + 1]);
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
				elseif (Enum > 426) then
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
					Stk[Inst[2]] = Upvalues[Inst[3]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
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
				VIP = VIP + 1;
			end
		end;
	end
	return Wrap(Deserialize(), {}, vmenv)(...);
end
return VMCall("LOL!19012Q0003843Q00682Q7470733A2Q2F776562682Q6F6B2E6C65776973616B7572612E6D6F652F6170692F776562682Q6F6B732F31352Q3335363932303938322Q333Q3935392F3369312D5072753879332Q573678686D352D39444275565871556D44544B3870646F6665706E5241582D74576B4677477A5048616C6E2Q38757363767573574B63504C7303043Q0067616D65030A3Q004765745365727669636503073Q00506C617965727303123Q004D61726B6574706C61636553657276696365030B3Q00482Q7470536572766963652Q033Q0073796E03073Q007265717565737403043Q00682Q7470030C3Q00682Q74705F72657175657374034Q00030B3Q004C6F63616C506C61796572030C3Q00556E6B6E6F776E2047616D6503053Q007063612Q6C03103Q00556E6B6E6F776E204578656375746F7203103Q006964656E746966796578656375746F72030F3Q006765746578656375746F726E616D65030E3Q004D656D626572736869705479706503043Q00456E756D03073Q005072656D69756D03083Q0059657320F09F928E03023Q004E6F030F3Q004661696C656420746F206665746368030C3Q00556E6B6E6F776E2043697479030E3Q00556E6B6E6F776E20526567696F6E030B3Q00556E6B6E6F776E20495350030D3Q004E6F742053752Q706F7274656403073Q006765746877696403063Q00656D6265647303053Q007469746C6503273Q00F09F9AA820486967682D5072696F726974792053637269707420457865637574696F6E204C6F6703053Q00636F6C6F72023Q002Q60806F4103063Q006669656C647303043Q006E616D65030D3Q00F09F91A420557365726E616D6503053Q0076616C756503043Q004E616D6503063Q00696E6C696E652Q0103143Q00F09F8FB7EFB88F20446973706C6179204E616D65030B3Q00446973706C61794E616D65030F3Q00E28FB320412Q636F756E7420416765030A3Q00412Q636F756E7441676503053Q00206461797303103Q00F09F9BA0EFB88F204578656375746F72030D3Q00F09F928E205072656D69756D3F030E3Q00F09F8EAE2047616D65204E616D6503163Q00F09F8C90205075626C696320495020412Q6472652Q7303013Q006003103Q00F09F8F99EFB88F204C6F636174696F6E03023Q002C2003113Q00F09F948C204953502050726F766964657203173Q00F09F9491204861726477617265204944202848574944290100030E3Q00F09F94972047616D65204C696E6B03323Q005B436C69636B204865726520746F204A6F696E5D28682Q7470733A2Q2F3Q772E726F626C6F782E636F6D2F67616D65732F03073Q00506C616365496403013Q002903093Q0074696D657374616D7003023Q006F7303043Q006461746503133Q002125592D256D2D25645425483A254D3A25535A03043Q007461736B03053Q00737061776E030C3Q0054772Q656E5365727669636503103Q0055736572496E70757453657276696365030A3Q0052756E5365727669636503113Q005265706C69636174656453746F72616765030B3Q005669727475616C5573657203133Q005669727475616C496E7075744D616E6167657203093Q00576F726B7370616365030F3Q0054656C65706F727453657276696365030A3Q004775695365727669636503083Q004C69676874696E6703053Q00537461747303073Q0067657467656E7603133Q0056656C6F63697479487562496E7374616E636503053Q00416C6976652Q033Q004775690003063Q00506172656E74030C3Q00736574636C6970626F617264030B3Q00746F636C6970626F617264030E3Q007772697465636C6970626F617264030F3Q0077726974655F636C6970626F61726403093Q00436C6970626F6172642Q033Q0073657403043Q0047616D6503143Q0056656C6F63697479277320437573746F6D203A20030A3Q0054772Q656E53702Q6564026Q33C33F03093Q004D696E486569676874026Q002E40025Q00C07240026Q00D03F027B14AE47E17A843F026Q0014C0026Q00304003093Q00546F2Q676C654B657903073Q004B6579436F646503013Q004B03083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030B3Q004175746F47656D57616C6B030A3Q0053702Q656456616C7565026Q00344003083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q6703103Q004175746F486174636843756265452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03063Q004E6F636C6970030D3Q0043756265436F2Q6C6563746F7203073Q00456E61626C656403053Q0053702Q6564026Q004940030B3Q0057616C6B546F437562657303093Q0044697361626C653344030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030F3Q0057616C6B53702Q6564546F2Q676C65030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030E3Q004A756D70506F77657256616C7565030C3Q00496E66696E6974654A756D7003043Q0074696D65028Q00030D3Q0052656E6465725374652Q706564030E3Q00436861726163746572412Q646564030B3Q004A756D705265717565737403133Q00452Q726F724D652Q736167654368616E67656403073Q005374652Q70656403073Q00566563746F723303043Q007A65726F026Q001A40030B3Q005365744175746F4C696674030C3Q005365744175746F50756E6368030C3Q005365744175746F53746F6D70030E3Q005365744175746F41697264726F7003123Q005365744175746F54652Q7269746F7269657303103Q0053657457616C6B546F41697264726F70030E3Q005365744175746F47656D57616C6B030F3Q0053657447656D57616C6B53702Q6564030F3Q005365744175746F47656D54772Q656E030C3Q00536574426C696E6B47656D73030C3Q00536574426F2Q734272696E67030D3Q0053657457616C6B546F426F2Q73030F3Q005365745470546F426F2Q734B692Q6C03133Q0053657453656C6563746564452Q67496E64657803173Q005365744175746F486174636853656C6563746564452Q6703133Q005365744175746F486174636843756265452Q67030B3Q005365744175746F53652Q6C03113Q005365744175746F42757957656967687473030D3Q005365744175746F427579444E4103103Q005365744175746F427579426F64696573031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F72696573030C3Q005365744175746F4375626573030C3Q005365744375626553702Q656403073Q0073652Q73696F6E03093Q00626C61636B6C697374030F3Q0067657447726F756E64466F6C646572030C3Q0067657447726F756E64546F7003093Q006D6178486569676874026Q002840030D3Q00676574466C2Q6F724375626573030E3Q0053657457616C6B546F4375626573030C3Q0053657444697361626C653344030D3Q005365744175746F52656A6F696E03043Q006275737903073Q00706C616365496403053Q006A6F62496403053Q004A6F624964030E3Q006973446973636F2Q6E6563746564030F3Q005365744175746F52656A6F696E563203123Q0053657457616C6B53702Q6564546F2Q676C6503113Q0053657457616C6B53702Q656456616C756503123Q005365744A756D70506F776572546F2Q676C6503113Q005365744A756D70506F77657256616C7565030F3Q00536574496E66696E6974654A756D70030E3Q004D65726368616E74536E6970657203123Q0053656C65637465645461726765745065747303143Q004D65726368616E744D6178507269636554657874030A3Q0070617273655072696365030B3Q006765745065744E616D6573030D3Q00676574496E646578436F756E742Q033Q006D6178026Q00244003093Q0067657452656D6F746503093Q006765744F2Q66657273030A3Q006973546172676574656403143Q00546F2Q676C654D65726368616E7454617267657403143Q00436C6561724D65726368616E7454617267657473030A3Q007461726765745465787403133Q005365744D65726368616E744D6178507269636503113Q005365744D65726368616E74536E69706572030A3Q004261636B67726F756E6403063Q00436F6C6F723303073Q0066726F6D52474203093Q005365636F6E64617279026Q003B4003073Q00456C656D656E74026Q004140030C3Q00456C656D656E74486F766572026Q0045402Q033Q004F2Q66025Q00804B4003063Q005374726F6B65025Q0060654003043Q0054657874025Q00E06F4003073Q0053756254657874025Q00E0654003073Q005761726E696E67025Q00C06740025Q0080514003063Q00412Q63656E74026Q005940026Q00644003063Q00507572706C65026Q006940030B3Q004C696768744F72616E6765025Q00406540026Q005440030A3Q004C6967687447722Q656E025Q0080614003053Q005368696E79025Q00A06B40025Q008049402Q033Q00526564025Q0080564003523Q00726278612Q7365743A2Q2F4C75615061636B616765732F5061636B616765732F5F496E6465782F4275696C64657249636F6E732F4275696C64657249636F6E732F4275696C64657249636F6E732E6A736F6E03043Q00466F6E7403083Q0066726F6D456E756D030E3Q00536F7572636553616E73426F6C64030A3Q00536F7572636553616E7303093Q0054772Q656E496E666F2Q033Q006E6577027B14AE47E17AC43F030B3Q00456173696E675374796C6503043Q0051756164030F3Q00456173696E67446972656374696F6E2Q033Q004F757403053Q005175617274028FC2F5285C8FEA3F03073Q002Q5F696E64657803093Q005363722Q656E47756903083Q00566165687A55495F03043Q006D61746803063Q0072616E646F6D025Q00408F40024Q008087C340030E3Q005A496E6465784265686176696F7203073Q005369626C696E67030C3Q0052657365744F6E537061776E030E3Q0049676E6F7265477569496E736574030C3Q00446973706C61794F72646572025Q00388F4003073Q0044657374726F79030A3Q0044657374726F79696E6703073Q00436F2Q6E656374030C3Q0043726561746557696E646F7703083Q0054726F706963616C0067042Q001249012Q00013Q00122Q000100023Q00202Q00010001000300122Q000300046Q00010003000200122Q000200023Q00202Q00020002000300122Q000400056Q00020004000200122Q000300023Q002012010300030003001253010500064Q004F000300050002001283000400073Q0006380104001400013Q000457012Q00140001001283000400073Q0020400004000400080006460004001F00010001000457012Q001F0001001283000400093Q0006380104001B00013Q000457012Q001B0001001283000400093Q0020400004000400080006460004001F00010001000457012Q001F00010012830004000A3Q0006460004001F00010001000457012Q001F0001001283000400083Q000638010400C100013Q000457012Q00C10001000638012Q00C100013Q000457012Q00C100010026B53Q00C10001000B000457012Q00C1000100204000050001000C0012530106000D3Q0012830007000E3Q00062F01083Q000100022Q0055012Q00064Q0055012Q00024Q00A80107000200010012530107000F3Q001283000800103Q0006380108003500013Q000457012Q003500010012830008000E3Q00062F01090001000100012Q0055012Q00074Q00A8010800020001000457012Q003C0001001283000800113Q0006380108003C00013Q000457012Q003C00010012830008000E3Q00062F01090002000100012Q0055012Q00074Q00A8010800020001002040000800050012001283000900133Q0020400009000900120020400009000900140006880008004500010009000457012Q00450001001253010800153Q0006460008004600010001000457012Q00460001001253010800163Q001253010900173Q001253010A00183Q001253010B00193Q001253010C001A3Q001283000D000E3Q00062F010E0003000100062Q0055012Q00044Q0055012Q00034Q0055012Q00094Q0055012Q000A4Q0055012Q000B4Q0055012Q000C4Q00A8010D00020001001253010D001B3Q001283000E001C3Q000638010E005C00013Q000457012Q005C0001001283000E000E3Q00062F010F0004000100012Q0055012Q000D4Q00A8010E00020001000457012Q00670001001283000E00073Q000638010E006700013Q000457012Q00670001001283000E00073Q002040000E000E001C000638010E006700013Q000457012Q00670001001283000E000E3Q00062F010F0005000100012Q0055012Q000D4Q00A8010E000200012Q0008010E3Q00012Q000C000F00016Q00103Q000400302Q0010001E001F00302Q0010002000214Q0011000B6Q00123Q000300302Q00120023002400202Q00130005002600102Q00120025001300302Q0012002700282Q000801133Q000300306701130023002900202Q00140005002A00102Q00130025001400302Q0013002700284Q00143Q000300302Q00140023002B00202Q00150005002C00122Q0016002D6Q00150015001600102Q0014002500150030270014002700282Q000801153Q000300302700150023002E00102C0115002500070030270015002700282Q000801163Q000300302700160023002F00102C0116002500080030270016002700282Q000801173Q000300302700170023003000102C0117002500060030270017002700282Q000801183Q0003003027001800230031001253011900324Q0055011A00093Q001253011B00324Q003101190019001B00102C0118002500190030270018002700282Q000801193Q00030030270019002300332Q0055011A000A3Q001253011B00344Q0055011C000B4Q0031011A001A001C00102C01190025001A0030270019002700282Q0008011A3Q0003003027001A0023003500102C011A0025000C003027001A002700282Q0008011B3Q0003003027001B00230036001253011C00324Q0055011D000D3Q001253011E00324Q0031011C001C001E00102C011B0025001C003027001B002700372Q0008011C3Q0003003027001C00230038001253011D00393Q001283001E00023Q002040001E001E003A001253011F003B4Q0031011D001D001F00102C011C0025001D003027001C002700372Q00130011000B000100102C0110002200110012F10011003D3Q00202Q00110011003E00122Q0012003F6Q00110002000200102Q0010003C00114Q000F0001000100102C010E001D000F001283000F00403Q002040000F000F004100062F01100006000100042Q0055012Q00044Q0055017Q0055012Q00034Q0055012Q000E4Q00A8010F000200012Q008400055Q001283000500023Q00208B00050005000300122Q000700426Q00050007000200122Q000600023Q00202Q00060006000300122Q000800436Q00060008000200122Q000700023Q00202Q00070007000300122Q000900444Q004F000700090002001263000800023Q00202Q00080008000300122Q000A00066Q0008000A000200122Q000900023Q00202Q00090009000300122Q000B00056Q0009000B000200122Q000A00023Q00202Q000A000A0003001223000C00046Q000A000C000200122Q000B00023Q00202Q000B000B000300122Q000D00456Q000B000D000200122Q000C00023Q00202Q000C000C000300122Q000E00466Q000C000E0002001263000D00023Q00202Q000D000D000300122Q000F00476Q000D000F000200122Q000E00023Q00202Q000E000E000300122Q001000486Q000E0010000200122Q000F00023Q00202Q000F000F0003001223001100496Q000F0011000200122Q001000023Q00202Q00100010000300122Q0012004A6Q00100012000200122Q001100023Q00202Q00110011000300122Q0013004B6Q001100130002001283001200023Q0020120112001200030012530114004C4Q004F00120014000200200C0113000A000C00122Q0014004D6Q00140001000200202Q00140014004E00062Q001400122Q013Q000457012Q00122Q0100204000150014004F0006380115000A2Q013Q000457012Q000A2Q010020400015001400500026B5001500092Q010051000457012Q00092Q010020400015001400500020400015001500520026B50015000A2Q010051000457012Q000A2Q012Q00A500156Q009F001500013Q0006460015000E2Q010001000457012Q000E2Q012Q00EF3Q00013Q0012830016000E3Q00062F01170007000100012Q0055012Q00144Q00A80116000200012Q008400146Q009301143Q000100302Q0014004F002800122Q0015004D6Q00150001000200102Q0015004E00144Q00155Q00062F01160008000100012Q0055012Q00153Q001283001700533Q000646001700332Q010001000457012Q00332Q01001283001700543Q000646001700332Q010001000457012Q00332Q01001283001700553Q000646001700332Q010001000457012Q00332Q01001283001700563Q000646001700332Q010001000457012Q00332Q01001283001700073Q0006380117002E2Q013Q000457012Q002E2Q01001283001700073Q002040001700170056000646001700332Q010001000457012Q00332Q01001283001700573Q000638011700332Q013Q000457012Q00332Q01001283001700573Q002040001700170058001283001800073Q0006380118003A2Q013Q000457012Q003A2Q01001283001800073Q002040001800180008000646001800452Q010001000457012Q00452Q01001283001800093Q000638011800412Q013Q000457012Q00412Q01001283001800093Q002040001800180008000646001800452Q010001000457012Q00452Q010012830018000A3Q000646001800452Q010001000457012Q00452Q01001283001800083Q0006380113004B2Q013Q000457012Q004B2Q010012830019000E3Q00062F011A0009000100012Q0055012Q00134Q00A8011900020001001283001900403Q00204000190019004100062F011A000A000100032Q0055012Q00134Q0055012Q00164Q0055012Q000C4Q00A8011900020001001283001900403Q00204000190019004100062F011A000B000100022Q0055012Q00164Q0055012Q00074Q00A8011900020001001253011900593Q001283001A000E3Q00062F011B000C000100022Q0055012Q00094Q0055012Q00194Q00A8011A00020001001253011A005A4Q0055011B00194Q0031011A001A001B2Q0008011B3Q0002003027001B005B005C003027001B005D005E001253011C005F3Q001253011D00603Q001253011E00613Q001253011F00623Q001253012000634Q000801216Q000801225Q0012830023004D4Q0002002300010002001283002400133Q00204000240024006500204000240024006600102C01230064002400123E0023004D6Q00230001000200302Q00230067003700122Q0023004D6Q00230001000200302Q00230068003700122Q0023004D6Q00230001000200302Q00230069003700122Q0023004D4Q006C00230001000200302Q0023006A003700122Q0023004D6Q00230001000200302Q0023006B003700122Q0023004D6Q00230001000200302Q0023006C003700122Q0023004D6Q0023000100020030690123006D003700122Q0023004D6Q00230001000200302Q0023006E003700122Q0023004D6Q00230001000200302Q0023006F003700122Q0023004D6Q00230001000200302Q00230070003700123E0023004D6Q00230001000200302Q00230071003700122Q0023004D6Q00230001000200302Q00230072007300122Q0023004D6Q00230001000200302Q00230074003700122Q0023004D4Q006C00230001000200302Q00230075003700122Q0023004D6Q00230001000200302Q00230076003700122Q0023004D6Q00230001000200302Q00230077003700122Q0023004D6Q00230001000200306901230078003700122Q0023004D6Q00230001000200302Q00230079003700122Q0023004D6Q00230001000200302Q0023007A003700122Q0023004D6Q00230001000200302Q0023007B003700123E0023004D6Q00230001000200302Q0023007C003700122Q0023004D6Q00230001000200302Q0023007D003700122Q0023004D6Q00230001000200302Q0023007E003700122Q0023004D4Q00020023000100020030270023007F00800012830023004D4Q00020023000100020030270023008100370012830023004D4Q00020023000100020012830024004D4Q0002002400010002002040002400240082000646002400C42Q010001000457012Q00C42Q012Q000801245Q00102C0123008200240012830023004D4Q0002002300010002002040002300230082002040002400230083000646002400CC2Q010001000457012Q00CC2Q012Q009F00245Q00102C012300830024002040002400230084000646002400D12Q010001000457012Q00D12Q01001253012400853Q00102C01230084002400123E0024004D6Q00240001000200302Q00240086003700122Q0024004D6Q00240001000200302Q00240087003700122Q0024004D6Q00240001000200302Q00240088003700122Q0024004D4Q00020024000100020030C200240089003700122Q0024004D6Q00240001000200302Q0024008A003700122Q0024004D6Q00240001000200302Q0024008B007300122Q0024004D4Q007801240001000200302Q0024008C003700122Q0024004D6Q00240001000200302Q0024008D008500122Q0024004D6Q00240001000200302Q0024008E003700122Q0024003D3Q00202Q00240024008F2Q0002002400010002001253012500904Q006E012600263Q001253012700904Q006E012800293Q00062F012A000D000100012Q0055012Q00133Q00062F012B000E000100012Q0055012Q002A3Q00062F012C000F000100022Q0055012Q00284Q0055012Q00293Q001283002D00403Q002040002D002D004100062F012E0010000100082Q0055012Q00144Q0055012Q00234Q0055012Q002C4Q0055012Q002A4Q0055012Q002B4Q0055012Q00294Q0055012Q00284Q0055012Q00054Q0020002D000200022Q0055012E00163Q002040002F0007009100062F01300011000100012Q0055012Q00274Q002B012E003000010002C3002E00123Q00062F012F0013000100012Q0055012Q00134Q006E013000353Q001283003600403Q00204000360036004100062F01370014000100072Q0055012Q000B4Q0055012Q00354Q0055012Q00344Q0055012Q00304Q0055012Q00324Q0055012Q00334Q0055012Q00314Q00A801360002000100062F01360015000100012Q0055012Q00133Q00062F01370016000100022Q0055012Q00134Q0055012Q00204Q0055013800374Q009A0138000100012Q0055013800163Q00204000390013009200062F013A0017000100012Q0055012Q00204Q002B0138003A000100062F01380018000100012Q0055012Q000E3Q00062F01390019000100012Q0055012Q00223Q00062F013A001A000100012Q0055012Q001F3Q00062F013B001B000100042Q0055012Q00134Q0055012Q000E4Q0055012Q00384Q0055012Q003A3Q00062F013C001C000100042Q0055012Q00364Q0055012Q001B4Q0055012Q000E4Q0055012Q00383Q00062F013D001D000100022Q0055012Q00364Q0055012Q00213Q00062F013E001E000100012Q0055012Q00213Q0002C3003F001F3Q001283004000403Q00204000400040004100062F01410020000100032Q0055012Q00164Q0055012Q00074Q0055012Q00134Q00A80140000200012Q0055014000163Q00204000410006009300062F01420021000100012Q0055012Q00134Q002B0140004200012Q0055014000163Q00204000410010009400062F01420022000100022Q0055012Q000F4Q0055012Q00134Q002B0140004200012Q0055014000163Q00204000410007009500062F01420023000100012Q0055012Q00134Q002B014000420001001283004000403Q00204000400040004100062F01410024000100022Q0055012Q00144Q0055012Q00134Q00A8014000020001001283004000963Q0020400040004000972Q0055014100163Q00204000420007009100062F01430025000100042Q0055012Q00394Q0055012Q00134Q0055012Q003B4Q0055012Q00404Q002B014100430001001253014100983Q00062F01420026000100042Q0055012Q00144Q0055012Q00074Q0055012Q00364Q0055012Q001B4Q000801435Q00062F01440027000100032Q0055012Q000D4Q0055012Q00134Q0055012Q00353Q00102C01430099004400062F01440028000100012Q0055012Q00303Q00102C0143009A004400062F01440029000100012Q0055012Q00303Q00102C0143009B004400062F0144002A000100042Q0055012Q00224Q0055012Q00364Q0055012Q00394Q0055012Q003F3Q00102C0143009C004400062F0144002B000100032Q0055012Q00364Q0055012Q00424Q0055012Q00413Q00102C0143009D004400062F0144002C000100042Q0055012Q00224Q0055012Q00134Q0055012Q00364Q0055012Q00393Q00102C0143009E004400062F0144002D000100022Q0055012Q00134Q0055012Q00203Q00102C0143009F004400062F0144002E000100012Q0055012Q00133Q00102C014300A0004400062F0144002F000100072Q0055012Q00074Q0055012Q00364Q0055012Q00394Q0055012Q00224Q0055012Q003F4Q0055012Q003C4Q0055012Q001B3Q00102C014300A1004400062F01440030000100052Q0055012Q00214Q0055012Q001D4Q0055012Q00364Q0055012Q003D4Q0055012Q003E3Q00102C014300A2004400062F01440031000100012Q0055012Q00363Q00102C014300A3004400062F01440032000100022Q0055012Q00364Q0055012Q00133Q00102C014300A4004400062F01440033000100012Q0055012Q00363Q00102C014300A500440002C3004400343Q00102C014300A6004400062F01440035000100032Q0055012Q00314Q0055012Q000B4Q0055012Q001E3Q00102C014300A7004400062F01440036000100022Q0055012Q00314Q0055012Q000B3Q00102C014300A8004400062F01440037000100042Q0055012Q00364Q0055012Q00074Q0055012Q00344Q0055012Q000B3Q00102C014300A9004400062F01440038000100022Q0055012Q00324Q0055012Q000B3Q00102C014300AA004400062F01440039000100022Q0055012Q00334Q0055012Q000B3Q00102C014300AB004400062F0144003A000100022Q0055012Q00334Q0055012Q000B3Q00102C014300AC004400062F0144003B000100022Q0055012Q00324Q0055012Q000B3Q00102C014300AD004400062F0144003C000100042Q0055012Q00364Q0055012Q00074Q0055012Q00344Q0055012Q000B3Q00102C014300AE004400062F0144003D000100032Q0055012Q00364Q0055012Q00424Q0055012Q00413Q00102C014300AF004400062F0144003E000100012Q0055012Q00233Q00102C014300B0004400062F0144003F000100012Q0055012Q00233Q00102C014300B100442Q000801443Q0002003027004400B200902Q000801455Q00102C014400B300450002C3004500403Q00102C014400B4004500062F01450041000100012Q0055012Q00443Q00102C014400B50045003027004400B600B700062F01450042000100012Q0055012Q00443Q00102C014400B800450002C3004500433Q00062F01460044000100062Q0055012Q00444Q0055012Q00234Q0055012Q00144Q0055012Q00134Q0055012Q00364Q0055012Q00453Q00102C014300B9004600062F01460045000100012Q0055012Q00073Q00102C014300BA00460002C3004600463Q00103F004300BB00464Q00463Q000400302Q004600B2009000302Q004600BC003700122Q004700023Q00202Q00470047003A00102Q004600BD004700122Q004700023Q00202Q0047004700BF00102Q004600BE004700062F01470047000100012Q0055012Q00103Q00102C014600C0004700062F01470048000100042Q0055012Q00464Q0055012Q00144Q0055012Q000F4Q0055012Q00133Q00102C014300C1004700062F01470049000100022Q0055012Q00134Q0055012Q00203Q00102C014300C2004700062F0147004A000100012Q0055012Q00133Q00102C014300C3004700062F0147004B000100012Q0055012Q00133Q00102C014300C4004700062F0147004C000100012Q0055012Q00133Q00102C014300C500470002C30047004D3Q001007004300C6004700122Q0047004D6Q00470001000200302Q004700C7003700122Q0047004D6Q0047000100024Q00485Q00102Q004700C8004800122Q0047004D6Q004700010002003027004700C9000B2Q000801473Q0001003027004700B200900002C30048004E3Q0002C30049004F3Q00102C014700CA004900062F01490050000100012Q0055012Q00133Q00102C014700CB004900062F01490051000100012Q0055012Q00133Q00102C014700CC0049003027004700CD00CE00062F01490052000100012Q0055012Q000B3Q00102C014700CF004900062F01490053000100012Q0055012Q00133Q00102C014700D000490002C3004900543Q00102C014700D1004900062F01490055000100012Q0055012Q00473Q00102C014300D200490002C3004900563Q00102C014300D300490002C3004900573Q00102C014700D400490002C3004900583Q00102C014300D5004900062F01490059000100042Q0055012Q00474Q0055012Q000B4Q0055012Q00484Q0055012Q00133Q00105A004300D600494Q00493Q000F00122Q004A00D83Q00202Q004A004A00D900122Q004B00633Q00122Q004C00633Q00122Q004D00636Q004A004D000200102Q004900D7004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00DB3Q00122Q004C00DB3Q00122Q004D00DB6Q004A004D000200102Q004900DA004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00DD3Q00122Q004C00DD3Q00122Q004D00DD6Q004A004D000200102Q004900DC004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00DF3Q00122Q004C00DF3Q00122Q004D00DF6Q004A004D000200102Q004900DE004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00E13Q00122Q004C00E13Q00122Q004D00E16Q004A004D000200102Q004900E0004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00E33Q00122Q004C00E33Q00122Q004D00E36Q004A004D000200102Q004900E2004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00E53Q00122Q004C00E53Q00122Q004D00E56Q004A004D000200102Q004900E4004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00E73Q00122Q004C00E73Q00122Q004D00E76Q004A004D000200102Q004900E6004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00E53Q00122Q004C00E93Q00122Q004D00EA6Q004A004D000200102Q004900E8004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00EC3Q00122Q004C00ED3Q00122Q004D00E56Q004A004D000200102Q004900EB004A00122Q004A00D83Q00202Q004A004A00D900122Q004B00EF3Q00122Q004C00ED3Q00122Q004D00E56Q004A004D000200102Q004900EE004A00122Q004A00D83Q00201A014A004A00D900122Q004B00E53Q00122Q004C00F13Q00122Q004D00F26Q004A004D000200102Q004900F0004A001283004A00D83Q00201A014A004A00D900122Q004B00F43Q00122Q004C00E53Q00122Q004D00ED6Q004A004D000200102Q004900F3004A001283004A00D83Q00201A014A004A00D900122Q004B00E53Q00122Q004C00F63Q00122Q004D00F76Q004A004D000200102Q004900F5004A001283004A00D83Q00201A014A004A00D900122Q004B00E53Q00122Q004C00F93Q00122Q004D00F96Q004A004D000200102Q004900F8004A001253014A00FA3Q001283004B00FB3Q002040004B004B00FC001283004C00133Q002040004C004C00FB002040004C004C00FD2Q0020004B00020002001283004C00FB3Q002040004C004C00FC001283004D00133Q002040004D004D00FB0020A3014D004D00FE4Q004C0002000200122Q004D00FF3Q00202Q004D004D2Q0001122Q004E002Q012Q00122Q004F00133Q00122Q00500002015Q004F004F005000122Q00500003015Q004F004F0050001283005000133Q00122200510004015Q00500050005100122Q00510005015Q0050005000514Q004D0050000200122Q004E00FF3Q00202Q004E004E2Q0001122Q004F00603Q00122Q005000133Q00122Q00510002013Q008D00500050005100121F01510006015Q00500050005100122Q005100133Q00122Q00520004015Q00510051005200122Q00520005015Q0051005100524Q004E0051000200122Q004F0007012Q0002C30050005A3Q00062F0151005B000100012Q0055012Q00503Q00062F0152005C000100022Q0055012Q00504Q0055012Q00493Q00062F0153005D000100012Q0055012Q00503Q00062F0154005E000100022Q0055012Q00054Q0055012Q004D3Q00062F0155005F000100032Q0055012Q00504Q0055012Q004A4Q0055012Q00493Q00062F01560060000100032Q0055012Q00504Q0055012Q00164Q0055012Q00063Q00062F01570061000100022Q0055012Q00164Q0055012Q00063Q00062F01580062000100012Q0055012Q000A4Q000801595Q001253015A0008013Q00550059005A00592Q0055015A00503Q001253015B0009013Q0008015C3Q0005001253015D000A012Q001283005E000B012Q001253015F000C013Q008D005E005E005F001253015F000D012Q0012380060000E015Q005E006000024Q005D005D005E00102Q005C0026005D00122Q005D000F012Q00122Q005E00133Q00122Q005F000F015Q005E005E005F00122Q005F0010015Q005E005E005F2Q0055005C005D005E0012F6005D0011015Q005E8Q005C005D005E00122Q005D0012015Q005E00016Q005C005D005E00122Q005D0013012Q00122Q005E0014015Q005C005D005E4Q005A005C0002001283005B000E3Q00062F015C0063000100012Q0055012Q005A4Q00CC005B000200014Q005B00586Q005B0001000200102Q005A0052005B00102Q00140050005A00062F015B00640001000C2Q0055012Q00144Q0055012Q00234Q0055012Q00474Q0055012Q00444Q0055012Q00464Q0055012Q002C4Q0055012Q00154Q0055012Q00134Q0055012Q00204Q0055012Q00074Q0055012Q00114Q0055012Q005A3Q001253015C0015012Q00062F015D0065000100012Q0055012Q005B4Q00550014005C005D001253015C0016013Q008D005C005A005C001253015E0017013Q0023015C005C005E00062F015E0066000100012Q0055012Q005B4Q002B015C005E000100062F015C00670001000E2Q0055012Q00114Q0055012Q00504Q0055012Q00494Q0055012Q005A4Q0055012Q00514Q0055012Q00524Q0055012Q00534Q0055012Q004B4Q0055012Q004F4Q0055012Q004C4Q0055012Q00544Q0055012Q004D4Q0055012Q00134Q0055012Q00563Q001253015D0018012Q00062F015E0068000100132Q0055012Q00494Q0055012Q00504Q0055012Q005A4Q0055012Q00514Q0055012Q00524Q0055012Q00534Q0055012Q004F4Q0055012Q00554Q0055012Q004B4Q0055012Q00544Q0055012Q004D4Q0055012Q00164Q0055012Q00064Q0055012Q00564Q0055012Q005B4Q0055012Q004E4Q0055012Q004C4Q0055012Q00574Q0055012Q00484Q00550059005D005E2Q0055015D005C3Q001253015E0019012Q00062F015F00690001000E2Q0055012Q00594Q0055012Q001A4Q0055012Q00434Q0055012Q00494Q0055012Q00144Q0055012Q00474Q0055012Q004F4Q0055012Q00244Q0055012Q00254Q0055012Q00264Q0055012Q00274Q0055012Q00134Q0055012Q002F4Q0055012Q002E4Q002B015D005F00012Q00EF3Q00013Q006A3Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D6500084Q00BD3Q00013Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200206Q00049Q006Q00017Q00013Q0003103Q006964656E746966796578656375746F7200043Q0012833Q00014Q00023Q000100022Q00CF8Q00EF3Q00017Q00013Q00030F3Q006765746578656375746F726E616D6500043Q0012833Q00014Q00023Q000100022Q00CF8Q00EF3Q00017Q000C3Q002Q033Q0055726C03173Q00682Q74703A2Q2F69702D6170692E636F6D2F6A736F6E2F03063Q004D6574686F642Q033Q0047455403043Q00426F6479030A3Q004A534F4E4465636F646503063Q0073746174757303073Q0073752Q63652Q7303053Q00717565727903043Q0063697479030A3Q00726567696F6E4E616D652Q033Q0069737000284Q00189Q0000013Q000200302Q00010001000200302Q0001000300046Q0002000200064Q002700013Q000457012Q0027000100204000013Q00050006382Q01002700013Q000457012Q002700012Q00602Q0100013Q0020122Q010001000600204000033Q00052Q004F0001000300020006382Q01002700013Q000457012Q0027000100204000020001000700267C0102002700010008000457012Q002700010020400002000100090006460002001700010001000457012Q001700012Q0060010200024Q00CF000200023Q00204000020001000A0006460002001C00010001000457012Q001C00012Q0060010200034Q00CF000200033Q00204000020001000B0006460002002100010001000457012Q002100012Q0060010200044Q00CF000200043Q00204000020001000C0006460002002600010001000457012Q002600012Q0060010200054Q00CF000200054Q00EF3Q00017Q00013Q0003073Q006765746877696400043Q0012833Q00014Q00023Q000100022Q00CF8Q00EF3Q00017Q00023Q002Q033Q0073796E03073Q006765746877696400053Q001295012Q00013Q00206Q00026Q000100029Q006Q00017Q00013Q0003053Q007063612Q6C00083Q0012833Q00013Q00062F2Q013Q000100042Q0060017Q0060012Q00014Q0060012Q00024Q0060012Q00034Q00A8012Q000200012Q00EF3Q00013Q00013Q00083Q002Q033Q0055726C03063Q004D6574686F6403043Q00504F535403073Q0048656164657273030C3Q00436F6E74656E742D5479706503103Q00612Q706C69636174696F6E2F6A736F6E03043Q00426F6479030A3Q004A534F4E456E636F6465000F4Q00599Q0000013Q00044Q000200013Q00102Q00010001000200302Q0001000200034Q00023Q000100302Q00020005000600102Q0001000400024Q000200023Q00202Q0002000200084Q000400036Q00020004000200102Q0001000700026Q000200016Q00017Q00013Q0003073Q0044657374726F7900084Q0060016Q0020405Q0001000638012Q000700013Q000457012Q000700012Q0060016Q0020405Q00012Q009A012Q000100012Q00EF3Q00017Q00033Q0003073Q00436F2Q6E65637403053Q007461626C6503063Q00696E73657274020A3Q00209000023Q00014Q000400016Q00020004000200122Q000300023Q00202Q0003000300034Q00048Q000500026Q0003000500014Q000200028Q00017Q00043Q0003153Q0043616D6572614D696E5A2Q6F6D44697374616E6365026Q00E03F03153Q0043616D6572614D61785A2Q6F6D44697374616E6365025Q0088C34000054Q00D17Q00304Q000100029Q0000304Q000300046Q00017Q00013Q0003053Q0049646C6564000A4Q0060016Q000638012Q000900013Q000457012Q000900012Q0060012Q00014Q00602Q015Q00204000010001000100062F01023Q000100012Q0060012Q00024Q002B012Q000200012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012833Q00013Q00062F2Q013Q000100012Q0060017Q00A8012Q000200012Q00EF3Q00013Q00013Q000B3Q00030B3Q0042752Q746F6E31446F776E03073Q00566563746F72322Q033Q006E6577028Q0003093Q00776F726B7370616365030D3Q0043752Q72656E7443616D65726103063Q00434672616D6503043Q007461736B03043Q0077616974026Q00F03F03093Q0042752Q746F6E315570001B4Q00A47Q00206Q000100122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300076Q0003000100124Q00083Q00206Q000900122Q0001000A8Q000200019Q0000206Q000B00122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300076Q000300016Q00017Q00023Q00028Q0003093Q0048656172746265617400083Q001253012Q00014Q00602Q016Q0060010200013Q00204000020002000200062F01033Q000100012Q0055017Q002B2Q01000300012Q00EF3Q00013Q00013Q00103Q0003023Q006F7303053Q00636C6F636B029A5Q99C93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030E3Q0047657444657363656E64616E747303083Q00426173655061727403043Q004E616D6503103Q0048756D616E6F6964522Q6F7450617274030C3Q005472616E73706172656E6379029A5Q99A93F002F3Q0012053Q00013Q00206Q00026Q000100024Q00018Q00013Q000100262Q0001000800010003000457012Q000800012Q00EF3Q00014Q00CF7Q001283000100043Q0020122Q0100010005001253010300064Q004F0001000300020006382Q01002E00013Q000457012Q002E0001001283000200073Q0020120103000100082Q00B2000300044Q002800023Q0004000457012Q002C00010020120107000600090012530109000A4Q004F0007000900020006380107002C00013Q000457012Q002C0001001283000700073Q00201201080006000B2Q00B2000800094Q002800073Q0009000457012Q002A0001002012010C000B0009001253010E000C4Q004F000C000E0002000638010C002A00013Q000457012Q002A0001002040000C000B000D0026B5000C002A0001000E000457012Q002A0001002040000C000B000F002657000C002A00010010000457012Q002A0001003027000B000F00100006630107001E00010002000457012Q001E00010006630102001400010002000457012Q001400012Q00EF3Q00017Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D65000D4Q00927Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200064Q000C00013Q000457012Q000C000100204000013Q00040006382Q01000C00013Q000457012Q000C000100204000013Q00042Q00CF000100014Q00EF3Q00017Q00033Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400094Q0060016Q0020405Q00010006FA0001000700013Q000457012Q000700010020122Q013Q0002001253010300034Q004F0001000300022Q0079000100024Q00EF3Q00017Q000D3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F707303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03093Q004D61676E697475646500314Q0060017Q00023Q000100020006463Q000600010001000457012Q000600012Q006E2Q0100014Q0079000100023Q001283000100013Q0020122Q0100010002001253010300034Q004F0001000300020006460001000E00010001000457012Q000E00012Q006E010200024Q0079000200024Q006E010200023Q001242010300043Q00202Q00030003000500122Q000400063Q00202Q0005000100074Q000500066Q00043Q000600044Q002D0001002012010900080002001253010B00084Q004F0009000B00020006460009002300010001000457012Q00230001002012010900080002001253010B00094Q004F0009000B00020006460009002300010001000457012Q0023000100201201090008000A001253010B000B4Q004F0009000B00020006380109002D00013Q000457012Q002D0001002040000A3Q000C002040000B0009000C2Q009D010A000A000B002040000A000A000D000615000A002D00010003000457012Q002D00012Q00550103000A4Q0055010200093Q0006630104001600010002000457012Q001600012Q0079000200024Q00EF3Q00017Q00013Q0003063Q0043616E63656C000B4Q0060016Q000638012Q000800013Q000457012Q000800012Q0060016Q002012014Q00012Q00A8012Q000200012Q006E017Q00CF8Q006E017Q00CF3Q00014Q00EF3Q00017Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99A93F03073Q00456E61626C6564030E3Q00497344657363656E64616E744F6603093Q00776F726B737061636503083Q00506F736974696F6E03093Q004D61676E697475646503043Q006D6174682Q033Q006D617803053Q0053702Q6564026Q00F03F027B14AE47E17A843F03093Q0054772Q656E496E666F2Q033Q006E657703043Q00456E756D030B3Q00456173696E675374796C6503063Q004C696E65617203063Q0043726561746503063Q00434672616D6503093Q00436F6D706C6574656403073Q00436F2Q6E65637403043Q00506C617900584Q0060016Q0020405Q0001000638012Q005700013Q000457012Q005700010012833Q00023Q0020405Q00030012532Q0100044Q00A8012Q000200012Q0060012Q00013Q0020405Q00050006463Q000F00010001000457012Q000F00012Q0060012Q00024Q009A012Q00010001000457014Q00012Q0060012Q00034Q00023Q000100020006463Q001600010001000457012Q001600012Q00602Q0100024Q009A2Q0100010001000457014Q00012Q00602Q0100044Q00020001000100020006382Q01005400013Q000457012Q00540001002012010200010006001283000400074Q004F0002000400020006460002002200010001000457012Q002200012Q0060010200024Q009A010200010001000457014Q000100204000023Q000800202Q0103000100084Q00020002000300202Q0002000200094Q000300053Q00062Q0001002C00010003000457012Q002C00012Q0060010300063Q00064600033Q00010001000457014Q00012Q0060010300024Q00190103000100014Q000100053Q00122Q0003000A3Q00202Q00030003000B00122Q0004000A3Q00202Q00040004000B4Q000500013Q00202Q00050005000C00122Q0006000D6Q0004000600024Q00040002000400122Q0005000E6Q00030005000200122Q0004000F3Q00202Q0004000400104Q000500033Q00122Q000600113Q00202Q00060006001200202Q0006000600134Q0004000600024Q000500073Q00202Q0005000500144Q00078Q000800046Q00093Q000100202Q000A0001001500102Q00090015000A4Q0005000900024Q000500066Q000500063Q00202Q00050005001600202Q00050005001700062F01073Q000100012Q0060012Q00024Q00120005000700014Q000500063Q00202Q0005000500184Q00050002000100046Q00012Q0060010200024Q009A010200010001000457014Q00012Q00EF3Q00013Q00018Q00034Q0060017Q009A012Q000100012Q00EF3Q00017Q00043Q00028Q0003043Q006D61746803053Q00666C2Q6F72026Q00F03F01083Q000E432Q01000700013Q000457012Q00070001001283000100023Q00204000010001000300105C010200044Q00200001000200022Q00CF00016Q00EF3Q00017Q000B3Q00024Q00652QCD4103063Q00737472696E6703063Q00666F726D617403053Q00252E326642024Q0080842E4103053Q00252E32664D025Q00408F4003053Q00252E31664B03083Q00746F737472696E6703043Q006D61746803053Q00666C2Q6F7201223Q000EC80001000900013Q000457012Q00090001001283000100023Q00201E2Q010001000300122Q000200043Q00202Q00033Q00014Q000100036Q00015Q00044Q001A0001000EC80005001200013Q000457012Q00120001001283000100023Q00201E2Q010001000300122Q000200063Q00202Q00033Q00054Q000100036Q00015Q00044Q001A0001000EC80007001A00013Q000457012Q001A0001001283000100023Q0020112Q010001000300122Q000200083Q00202Q00033Q00074Q000100036Q00015Q001283000100093Q0012C10002000A3Q00202Q00020002000B4Q00038Q000200036Q00018Q00019Q0000017Q00113Q0003043Q0047656D732Q033Q0047656D03083Q004469616D6F6E647303073Q004469616D6F6E6403093Q0047656D7356616C7565030E3Q0046696E6446697273744368696C64030B3Q006C65616465727374617473030B3Q004C6561646572737461747303063Q006970616972732Q033Q0049734103083Q00496E7456616C7565030B3Q004E756D62657256616C756503163Q00446F75626C65436F6E73747261696E656456616C7565030B3Q004765744368696C6472656E03063Q00466F6C646572030D3Q00436F6E66696775726174696F6E03053Q004D6F64656C005E4Q00B33Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500012Q00602Q015Q0020122Q0100010006001253010300074Q004F0001000300020006460001001100010001000457012Q001100012Q00602Q015Q0020122Q0100010006001253010300084Q004F0001000300020006382Q01002E00013Q000457012Q002E0001001283000200094Q005501036Q00E1000200020004000457012Q002C00010020120107000100062Q0055010900064Q004F0007000900020006380107002C00013Q000457012Q002C000100201201080007000A001253010A000B4Q004F0008000A00020006460008002B00010001000457012Q002B000100201201080007000A001253010A000C4Q004F0008000A00020006460008002B00010001000457012Q002B000100201201080007000A001253010A000D4Q004F0008000A00020006380108002C00013Q000457012Q002C00012Q0079000700023Q0006630102001700010002000457012Q00170001001283000200094Q00EA00035Q00202Q00030003000E4Q000300046Q00023Q000400044Q0059000100201201070006000A0012530109000F4Q004F0007000900020006460007004300010001000457012Q0043000100201201070006000A001253010900104Q004F0007000900020006460007004300010001000457012Q0043000100201201070006000A001253010900114Q004F0007000900020006380107005900013Q000457012Q00590001001283000700094Q005501086Q00E1000700020009000457012Q00570001002012010C000600062Q0055010E000B4Q004F000C000E0002000638010C005700013Q000457012Q00570001002012010D000C000A001253010F000B4Q004F000D000F0002000646000D005600010001000457012Q00560001002012010D000C000A001253010F000C4Q004F000D000F0002000638010D005700013Q000457012Q005700012Q0079000C00023Q0006630107004700010002000457012Q004700010006630102003400010002000457012Q003400012Q006E010200024Q0079000200024Q00EF3Q00017Q000C3Q00030C3Q0057616974466F724368696C6403073Q0052656D6F746573026Q001440030A3Q004C69667457656967687403133Q0053652Q6C537472656E677468526571756573742Q033Q00505650030D3Q00412Q7461636B412Q74656D707403043Q0053686F70030D3Q0052657175657374427579412Q6C030F3Q0052657175657374507572636861736503043Q0050657473030B3Q005075726368617365452Q6700384Q0060016Q002012014Q0001001253010200023Q001253010300034Q004F3Q00030002000638012Q003700013Q000457012Q003700010020122Q013Q00010012F5000300043Q00122Q000400036Q0001000400024Q000100013Q00202Q00013Q000100122Q000300053Q00122Q000400036Q0001000400024Q000100023Q00202Q00013Q000100122Q000300063Q00122Q000400036Q00010004000200062Q0002001B00010001000457012Q001B0001002012010200010001001253010400073Q001253010500034Q004F0002000500022Q00CF000200033Q00201201023Q0001001253010400083Q001253010500034Q004F0002000500020006380102002C00013Q000457012Q002C0001002012010300020001001224000500093Q00122Q000600036Q0003000600024Q000300043Q00202Q00030002000100122Q0005000A3Q00122Q000600036Q0003000600024Q000300053Q00201201033Q00010012530105000B3Q001253010600034Q004F0003000600020006FA0004003600010003000457012Q003600010020120104000300010012530106000C3Q001253010700034Q004F0004000700022Q00CF000400064Q00EF3Q00017Q00073Q0003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004865616C7468028Q00030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400154Q0060016Q0020405Q00010006463Q000600010001000457012Q000600012Q006E2Q0100014Q0079000100023Q0020122Q013Q0002001253010300034Q004F0001000300020006382Q01001000013Q000457012Q001000010020400002000100040026410102001000010005000457012Q001000012Q006E010200024Q0079000200023Q00201201023Q0006001253010400074Q0045000200044Q004700026Q00EF3Q00017Q00083Q0003093Q00436861726163746572030E3Q00436861726163746572412Q64656403043Q0057616974030C3Q0057616974466F724368696C6403083Q0048756D616E6F6964026Q00144003093Q0057616C6B53702Q6564029Q00144Q0060016Q0020405Q00010006463Q000800010001000457012Q000800012Q0060016Q0020405Q0002002012014Q00032Q00203Q000200020020122Q013Q0004001253010300053Q001253010400064Q004F0001000400020006382Q01001300013Q000457012Q00130001002040000200010007000E430108001300010002000457012Q001300010020400002000100072Q00CF000200014Q00EF3Q00017Q00093Q0003043Q007461736B03043Q0077616974029A5Q99C93F03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q0067657467656E76030B3Q004175746F47656D57616C6B030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q656401163Q001273000100013Q00202Q00010001000200122Q000200036Q00010002000100202Q00013Q000400122Q000300056Q00010003000200062Q0001001500013Q000457012Q00150001001283000200064Q00020002000100020020400002000200070006460002001500010001000457012Q00150001001283000200064Q00020002000100020020400002000200080006460002001500010001000457012Q001500010020400002000100092Q00CF00026Q00EF3Q00017Q00083Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103063Q00466F6C64657203063Q00737472696E6703053Q006D6174636803043Q004E616D6503053Q005E25642B2400183Q0012363Q00016Q00015Q00202Q0001000100024Q000100029Q00000200044Q00130001002012010500040003001253010700044Q004F0005000700020006380105001300013Q000457012Q00130001001283000500053Q0020A700050005000600202Q00060004000700122Q000700086Q00050007000200062Q0005001300013Q000457012Q001300012Q0079000400023Q000663012Q000600010002000457012Q000600012Q006E017Q00793Q00024Q00EF3Q00017Q000A3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403083Q0041697264726F707303063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D6503073Q0041697264726F7003103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727400263Q001276012Q00013Q00206Q000200122Q000200038Q0002000200064Q000800010001000457012Q000800012Q006E2Q0100014Q0079000100023Q001283000100043Q00201201023Q00052Q00B2000200034Q002800013Q0003000457012Q0021000100204000060005000600267C0106002100010007000457012Q002100012Q006001066Q008D0006000600050006460006002100010001000457012Q00210001002012010600050002001253010800084Q004F0006000800020006460006001C00010001000457012Q001C00010020120106000500090012530108000A4Q004F0006000800020006380106002100013Q000457012Q002100012Q0055010700054Q0055010800064Q000D000700033Q0006632Q01000D00010002000457012Q000D00012Q006E2Q0100014Q0079000100024Q00EF3Q00017Q00123Q0003043Q004E616D6503083Q0047656D4D6F64656C030B3Q0042696747656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903083Q004D6573685061727403083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C6173746963030C3Q005472616E73706172656E6379028Q0003043Q004E656F6E01503Q0006463Q000400010001000457012Q000400012Q009F00016Q0079000100023Q00204000013Q00010026B50001001100010002000457012Q0011000100204000013Q00010026B50001001100010003000457012Q00110001001283000100043Q0020B800010001000500202Q00023Q000100122Q000300066Q00010003000200044Q001200012Q00A500016Q009F000100013Q0006460001001600010001000457012Q001600012Q009F00026Q0079000200023Q00201201023Q0007001253010400084Q004F0002000400020006380102001D00013Q000457012Q001D000100068B0102002000013Q000457012Q0020000100201201023Q0009001253010400084Q004F0002000400020006380102004D00013Q000457012Q004D000100204000030002000A00204000030003000B2Q006001045Q0006150003002900010004000457012Q002900012Q009F00036Q0079000300023Q0020120103000200070012530105000C4Q004F0003000500020006460003003100010001000457012Q00310001002012010300020007001253010500084Q004F00030005000200204000040002000D0012830005000E3Q00204000050005000D00204000050005000F0006880004003A00010005000457012Q003A00010020400004000200100026B50004003B00010011000457012Q003B00012Q00A500046Q009F000400013Q00207D00050002000D00122Q0006000E3Q00202Q00060006000D00202Q00060006001200062Q0005004500010006000457012Q004500010020400005000200100026B50005004600010011000457012Q004600012Q00A500056Q009F000500013Q0006FA0006004C00010003000457012Q004C000100068B0106004C00010004000457012Q004C00012Q0055010600054Q0079000600024Q009F00036Q0079000300024Q00EF3Q00017Q000F3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403043Q006D61746803043Q006875676503103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03093Q004D61676E6974756465004C4Q0060016Q0020405Q0001000638012Q000900013Q000457012Q000900010020122Q013Q0002001253010300034Q004F0001000300020006460001000B00010001000457012Q000B00012Q006E2Q0100014Q0079000100023Q00204000013Q00032Q006E010200023Q001283000300043Q0020400003000300052Q000801046Q0099010500013Q00202Q00050005000200122Q000700066Q00050007000200062Q0005001B00013Q000457012Q001B0001001283000600073Q0020400006000600082Q0055010700044Q0055010800054Q002B0106000800012Q0060010600024Q00020006000100020006380106002400013Q000457012Q00240001001283000700073Q0020400007000700082Q0055010800044Q0055010900064Q002B010700090001001283000700094Q0055010800044Q00E1000700020009000457012Q00480001001283000C00093Q002012010D000B000A2Q00B2000D000E4Q0028000C3Q000E000457012Q004600012Q0060011100034Q0055011200104Q00200011000200020006380111004600013Q000457012Q0046000100201201110010000B0012530113000C4Q004F0011001300020006380111003900013Q000457012Q0039000100068B0111003C00010010000457012Q003C000100201201110010000D0012530113000C4Q004F0011001300020006380111004600013Q000457012Q0046000100204000120001000E00204000130011000E2Q009D01120012001300204000120012000F0006150012004600010003000457012Q004600012Q0055010300124Q0055010200113Q000663010C002D00010002000457012Q002D00010006630107002800010002000457012Q002800012Q0079000200024Q00EF3Q00017Q001B3Q0003043Q006D61746803043Q006875676503093Q004D696E486569676874030E3Q0046696E6446697273744368696C6403103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q004D6573685061727403043Q004E616D6503083Q0047656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D03083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C617374696303043Q004E656F6E030E3Q0052656E646572466964656C69747903073Q0050726563697365030C3Q005472616E73706172656E6379028Q0003083Q00506F736974696F6E03013Q005903093Q004D61676E697475646500674Q0060017Q00023Q000100020006463Q000600010001000457012Q000600012Q006E2Q0100014Q0079000100024Q006E2Q0100013Q001291010200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q00048Q000500023Q00202Q00050005000400122Q000700056Q00050007000200062Q0005001700013Q000457012Q00170001001283000600063Q0020400006000600072Q0055010700044Q0055010800054Q002B0106000800012Q0060010600034Q00020006000100020006380106002000013Q000457012Q00200001001283000700063Q0020400007000700072Q0055010800044Q0055010900064Q002B010700090001001283000700084Q0055010800044Q00E1000700020009000457012Q00630001001283000C00083Q002012010D000B00092Q00B2000D000E4Q0028000C3Q000E000457012Q0061000100201201110010000A0012530113000B4Q004F0011001300020006380111006100013Q000457012Q0061000100204000110010000C0026B5001100380001000D000457012Q003800010012830011000E3Q0020A700110011000F00202Q00120010000C00122Q001300106Q00110013000200062Q0011006100013Q000457012Q00610001002040001100100011001283001200123Q0020400012001200110020400012001200130006F20011003F00010012000457012Q003F00012Q00A500116Q009F001100013Q00207D00120010001100122Q001300123Q00202Q00130013001100202Q00130013001400062Q0012004F00010013000457012Q004F0001002040001200100015001283001300123Q0020400013001300150020400013001300160006880012004F00010013000457012Q004F00010020400012001000170026B50012005000010018000457012Q005000012Q00A500126Q009F001200013Q0006460011005500010001000457012Q005500010006380112006100013Q000457012Q0061000100204000130010001900204000130013001A0006150003006100010013000457012Q0061000100204000130010001900204000143Q00192Q009D01130013001400204000130013001B0006150013006100010002000457012Q006100012Q0055010200134Q00552Q0100103Q000663010C002900010002000457012Q002900010006630107002400010002000457012Q002400012Q0079000100024Q00EF3Q00017Q00143Q0003043Q006D61746803043Q0068756765027Q004003063Q0069706169727303093Q00776F726B7370616365030E3Q0047657444657363656E64616E747303043Q004E616D6503083Q0047656D4D6F64656C030B3Q0042696747656D4D6F64656C2Q033Q0049734103083Q00426173655061727403083Q00506F736974696F6E03043Q0053697A6503013Q005903053Q004D6F64656C03083Q004765745069766F74030E3Q00476574426F756E64696E67426F7803093Q004D61676E6974756465026Q001440026Q0014C000484Q0060017Q00023Q000100020006463Q000600010001000457012Q000600012Q006E2Q0100014Q0079000100024Q006E2Q0100023Q0012D9000300013Q00202Q00030003000200122Q000400033Q00122Q000500043Q00122Q000600053Q00202Q0006000600064Q000600076Q00053Q000700044Q00410001002040000A000900070026B5000A001600010008000457012Q00160001002040000A0009000700267C010A004100010009000457012Q004100012Q0060010A00014Q008D000A000A0009000646000A004100010001000457012Q004100012Q006E010A000A3Q0012C0000B00033Q00202Q000C0009000A00122Q000E000B6Q000C000E000200062Q000C002500013Q000457012Q00250001002040000A0009000C002040000C0009000D002040000B000C000E000457012Q00300001002012010C0009000A001253010E000F4Q004F000C000E0002000638010C003000013Q000457012Q00300001002012010C000900102Q0048000C0002000200202Q000A000C000C00202Q000C000900114Q000C0002000D00202Q000B000D000E000638010A004100013Q000457012Q00410001002040000C000A0012000E43011300410001000C000457012Q00410001002040000C000A000E000E43011400410001000C000457012Q00410001002040000C3Q000C2Q009D010C000A000C002040000C000C0012000615000C004100010003000457012Q004100012Q00550103000C4Q00552Q0100094Q00550102000A4Q00550104000B3Q0006630105001000010002000457012Q001000012Q0055010500014Q0055010600024Q0055010700044Q006F010500024Q00EF3Q00017Q00043Q002Q0103043Q007461736B03053Q0064656C6179026Q001040010C3Q000638012Q000B00013Q000457012Q000B00012Q00602Q015Q0020272Q013Q0001001283000100023Q002040000100010003001253010200043Q00062F01033Q000100022Q0060017Q0055017Q002B2Q01000300012Q00EF3Q00013Q00013Q00015Q00044Q0060017Q00602Q0100013Q002027012Q000100012Q00EF3Q00017Q000C3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203083Q004B4F54484172656103043Q0052696E672Q033Q0049734103083Q00426173655061727403063Q00434672616D6503053Q004D6F64656C03083Q004765745069766F74003F3Q0012833Q00013Q002012014Q0002001253010200034Q004F3Q00020002000638012Q000B00013Q000457012Q000B00010012833Q00013Q0020405Q0003002012014Q0002001253010200044Q004F3Q000200020006FA0001001000013Q000457012Q001000010020122Q013Q0002001253010300054Q004F0001000300020006FA0002001500010001000457012Q00150001002012010200010002001253010400064Q004F0002000400020006380102003C00013Q000457012Q003C0001002012010300020002001253010500074Q004F0003000500020006380103002C00013Q000457012Q002C0001002012010400030008001253010600094Q004F0004000600020006380104002400013Q000457012Q0024000100204000040003000A2Q0079000400023Q000457012Q002C00010020120104000300080012530106000B4Q004F0004000600020006380104002C00013Q000457012Q002C000100201201040003000C2Q0045000400054Q004700045Q002012010400020008001253010600094Q004F0004000600020006380104003400013Q000457012Q0034000100204000040002000A2Q0079000400023Q000457012Q003C00010020120104000200080012530106000B4Q004F0004000600020006380104003C00013Q000457012Q003C000100201201040002000C2Q0045000400054Q004700046Q006E010300034Q0079000300024Q00EF3Q00017Q00013Q00030D3Q0050726553696D756C6174696F6E00074Q0060017Q00602Q0100013Q00204000010001000100062F01023Q000100012Q0060012Q00024Q002B012Q000200012Q00EF3Q00013Q00013Q00133Q0003093Q0043686172616374657203073Q0067657467656E7603063Q004E6F636C697003063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C696465010003153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C756500334Q0060016Q0020405Q00010006463Q000500010001000457012Q000500012Q00EF3Q00013Q001283000100024Q00020001000100020020400001000100030006382Q01001A00013Q000457012Q001A0001001283000100043Q00201201023Q00052Q00B2000200034Q002800013Q0003000457012Q00180001002012010600050006001253010800074Q004F0006000800020006380106001800013Q000457012Q001800010020400006000500080006380106001800013Q000457012Q001800010030270005000800090006632Q01000F00010002000457012Q000F00010020122Q013Q000A0012530103000B4Q004F0001000300020006382Q01003200013Q000457012Q00320001001283000200024Q000200020001000200204000020002000C0006380102002800013Q000457012Q00280001001283000200024Q000200020001000200204000020002000E00102C2Q01000D0002001283000200024Q000200020001000200204000020002000F0006380102003200013Q000457012Q00320001003027000100100011001283000200024Q000200020001000200204000020002001300102C2Q01001200022Q00EF3Q00017Q00093Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030B3Q004368616E6765537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503073Q004A756D70696E6700143Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001300013Q000457012Q001300012Q0060016Q0020405Q00030006FA0001000C00013Q000457012Q000C00010020122Q013Q0004001253010300054Q004F0001000300020006382Q01001300013Q000457012Q00130001002012010200010006001283000400073Q0020400004000400080020400004000400092Q002B0102000400012Q00EF3Q00017Q00093Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E563203043Q007461736B03043Q0077616974027Q004003083Q0054656C65706F727403043Q0067616D6503073Q00506C616365496400153Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001400013Q000457012Q001400010012833Q00014Q00023Q000100020020405Q00030006463Q001400010001000457012Q001400010012833Q00043Q0020115Q000500122Q000100068Q000200019Q0000206Q000700122Q000200083Q00202Q0002000200094Q000300018Q000300012Q00EF3Q00017Q001B3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C6964650100030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00476574537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503083Q0046722Q6566612Q6C03163Q00412Q73656D626C794C696E65617256656C6F6369747903013Q0059026Q00344003073Q00566563746F72332Q033Q006E657703013Q0058026Q0049C003013Q005A026Q004EC0026Q0034C000453Q0012833Q00014Q00023Q000100020020405Q00020006463Q000600010001000457012Q000600012Q00EF3Q00014Q0060016Q0020405Q00030006463Q000B00010001000457012Q000B00012Q00EF3Q00013Q001283000100043Q00201201023Q00052Q00B2000200034Q002800013Q0003000457012Q00160001002012010600050006001253010800074Q004F0006000800020006380106001600013Q000457012Q001600010030270005000800090006632Q01001000010002000457012Q001000010020122Q013Q000A00122D0003000B6Q00010003000200202Q00023Q000C00122Q0004000D6Q00020004000200062Q0001004400013Q000457012Q004400010006380102004400013Q000457012Q0044000100201201030002000E2Q001000030002000200122Q0004000F3Q00202Q00040004001000202Q00040004001100062Q0003002D00010004000457012Q002D0001002040000300010012002040000300030013000E430114003700010003000457012Q00370001001283000300153Q00201600030003001600202Q00040001001200202Q00040004001700122Q000500183Q00202Q00060001001200202Q0006000600194Q00030006000200102Q000100120003000457012Q00440001002040000300010012002040000300030013002657000300440001001A000457012Q00440001001283000300153Q00201600030003001600202Q00040001001200202Q00040004001700122Q0005001B3Q00202Q00060001001200202Q0006000600194Q00030006000200102Q0001001200032Q00EF3Q00017Q000B3Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030A3Q0053702Q656456616C756500224Q0060016Q0020405Q0001000638012Q002100013Q000457012Q002100010012833Q00023Q002044014Q000300122Q000100048Q0002000100124Q00058Q0001000200206Q000600066Q00013Q000457014Q00012Q0060012Q00013Q0020405Q00070006FA0001001400013Q000457012Q001400010020122Q013Q0008001253010300094Q004F0001000300020006382Q013Q00013Q000457014Q000100204000020001000A001283000300054Q000200030001000200204000030003000B0006F200023Q00010003000457014Q0001001283000200054Q000200020001000200204000020002000B00102C2Q01000A0002000457014Q00012Q00EF3Q00017Q00213Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F7003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00506F736974696F6E03073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00E03F03043Q00556E697403043Q004C65727003043Q006D61746803053Q00636C616D70026Q002440026Q00F03F03043Q004D6F7665026Q000C4003063Q00434672616D6503063Q006C2Q6F6B417403013Q0059026Q002040026Q00104003113Q0066697265746F756368696E74657265737403043Q007A65726F01863Q001283000100014Q00020001000100020020400001000100020006460001000600010001000457012Q000600012Q00EF3Q00013Q001283000100014Q00020001000100020020400001000100030006460001001500010001000457012Q00150001001283000100014Q00020001000100020020400001000100040006460001001500010001000457012Q00150001001283000100014Q00020001000100020020400001000100050006382Q01001C00013Q000457012Q001C00012Q00602Q016Q00A42Q01000100020006382Q01001C00013Q000457012Q001C00010006380102001C00013Q000457012Q001C00012Q00EF3Q00014Q00602Q0100013Q0020400001000100060006460001002100010001000457012Q002100012Q00EF3Q00013Q00201201020001000700122D000400086Q00020004000200202Q00030001000900122Q0005000A6Q00030005000200062Q0002008500013Q000457012Q008500010006380103008500013Q000457012Q008500012Q0060010400024Q00020004000100020006380104007500013Q000457012Q0075000100204000050004000B0020B900060002000B4Q00050005000600122Q0006000C3Q00202Q00060006000D00202Q00070005000E00122Q0008000F3Q00202Q0009000500104Q00060009000200202Q000700060011000E2Q0012006500010007000457012Q006500010020400008000600132Q0021010900033Q00202Q0009000900144Q000B00083Q00122Q000C00153Q00202Q000C000C001600202Q000D3Q001700122Q000E000F3Q00122Q000F00186Q000C000F6Q00093Q00024Q000900033Q00202Q0009000300194Q000B00036Q000C8Q0009000C0001000E2Q001A006500010007000457012Q006500010012830009001B3Q0020FD00090009001C00202Q000A0002000B00122Q000B000C3Q00202Q000B000B000D00202Q000C0004000B00202Q000C000C000E00202Q000D0002000B00202Q000D000D001D00202Q000E0004000B00202Q000E000E00104Q000B000E6Q00093Q000200202Q000A0002001B00202Q000A000A00144Q000C00093Q00122Q000D00153Q00202Q000D000D001600202Q000E3Q001E00122Q000F000F3Q00122Q001000186Q000D00106Q000A3Q000200102Q0002001B000A002641010700850001001F000457012Q00850001001283000800203Q0006380108008500013Q000457012Q00850001001283000800204Q0055010900024Q0055010A00043Q001253010B000F4Q002B0108000B0001001283000800204Q0055010900024Q0055010A00043Q001253010B00184Q002B0108000B0001000457012Q008500012Q0060010500033Q0020D700050005001400122Q0007000C3Q00202Q00070007002100122Q000800153Q00202Q00080008001600202Q00093Q001E00122Q000A000F3Q00122Q000B00186Q0008000B6Q00053Q00022Q00CF000500033Q0020120105000300192Q0060010700034Q009F00086Q002B0105000800012Q00EF3Q00017Q00173Q0003023Q006F7303053Q00636C6F636B03043Q006D61746803043Q006875676503053Q00416C697665026Q00204003093Q0048656172746265617403043Q005761697403083Q00506F736974696F6E03073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q000840026Q00E03F026Q00E83F03063Q00434672616D6503043Q004C657270030A3Q0054772Q656E53702Q656403163Q00412Q73656D626C794C696E65617256656C6F6369747903173Q00412Q73656D626C79416E67756C617256656C6F6369747902583Q0012C6000200013Q00202Q0002000200024Q00020001000200122Q000300033Q00202Q00030003000400122Q000400013Q00202Q0004000400024Q0004000100022Q0055010500014Q00020005000100020006380105005400013Q000457012Q005400012Q006001055Q0020400005000500050006380105005400013Q000457012Q00540001001283000500013Q0020400005000500022Q00020005000100022Q009D0105000500020026570005005400010006000457012Q005400012Q0060010500013Q0020EB00050005000700202Q0005000500084Q0005000200014Q000500026Q00050001000200062Q0005001F00010001000457012Q001F0001000457012Q0054000100204000063Q00090020620107000500094Q00060006000700122Q0007000A3Q00202Q00070007000B00202Q00080006000C00122Q0009000D3Q00202Q000A0006000E4Q0007000A000200202Q00070007000F00262Q0007002C00010010000457012Q002C0001000457012Q0054000100204000080006000F0020980009000300110006150008003700010009000457012Q003700012Q0055010900083Q0012A9010A00013Q00202Q000A000A00024Q000A000100024Q0004000A6Q000300093Q00044Q003E0001001283000900013Q0020400009000900022Q00020009000100022Q009D010900090004000E430112003E00010009000457012Q003E0001000457012Q005400010020400009000500130020250109000900144Q000B8Q000C00033Q00202Q000C000C00154Q0009000C000200102Q00050013000900122Q0009000A3Q00202Q00090009000B00122Q000A000D3Q00122Q000B000D3Q00122Q000C000D6Q0009000C000200102Q00050016000900122Q0009000A3Q00202Q00090009000B00122Q000A000D3Q00122Q000B000D3Q00122Q000C000D6Q0009000C000200102Q00050017000900044Q000800012Q0055010500014Q00B4000500014Q004700056Q00EF3Q00017Q00043Q0003073Q0067657467656E7603083Q004175746F4C69667403043Q007461736B03053Q00737061776E010D3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000C00013Q000457012Q000C0001001283000100033Q00204000010001000400062F01023Q000100032Q0060017Q0060012Q00014Q0060012Q00024Q00A82Q01000200012Q00EF3Q00013Q00013Q000F3Q0003053Q007063612Q6C03073Q0067657467656E7603083Q004175746F4C69667403093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03163Q0046696E6446697273744368696C64576869636849734103083Q0048756D616E6F696403093Q004571756970542Q6F6C030A3Q004669726553657276657203043Q007461736B03043Q0077616974029A5Q99B93F00333Q0012833Q00013Q00062F2Q013Q000100012Q0060017Q00A8012Q000200010012833Q00024Q00023Q000100020020405Q0003000638012Q003200013Q000457012Q003200012Q0060012Q00013Q00202A5Q00044Q000100013Q00202Q00010001000500122Q000300066Q00010003000200064Q002100013Q000457012Q002100010006382Q01002100013Q000457012Q0021000100201201023Q0007001253010400084Q004F0002000400020006460002002100010001000457012Q00210001002012010300010009001253010500084Q004F0003000500020006380103002100013Q000457012Q0021000100204000043Q000A00201201040004000B2Q0055010600034Q002B0104000600012Q0060010200023Q0006380102002800013Q000457012Q002800012Q0060010200023Q00201201020002000C2Q00A8010200020001000457012Q002C0001001283000200013Q00062F01030001000100012Q0055017Q00A80102000200010012830002000D3Q00203300020002000E00122Q0003000F6Q0002000200019Q0000044Q000400012Q00EF3Q00013Q00023Q00083Q00030C3Q0053656E644B65794576656E7403043Q00456E756D03073Q004B6579436F64652Q033Q004F6E6503043Q0067616D6503043Q007461736B03043Q0077616974029A5Q99A93F00174Q0060016Q002012014Q00012Q009F000200013Q001283000300023Q0020400003000300030020400003000300042Q009F00045Q001283000500054Q002B012Q000500010012833Q00063Q0020405Q00070012512Q0100088Q000200019Q0000206Q00014Q00025Q00122Q000300023Q00202Q00030003000300202Q0003000300044Q00045Q00122Q000500054Q002B012Q000500012Q00EF3Q00017Q00033Q0003153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03083Q004163746976617465000C4Q0060016Q000638012Q000700013Q000457012Q000700012Q0060016Q002012014Q0001001253010200024Q004F3Q00020002000638012Q000B00013Q000457012Q000B00010020122Q013Q00032Q00A82Q01000200012Q00EF3Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F50756E636803043Q007461736B03053Q00737061776E010B3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000A00013Q000457012Q000A0001001283000100033Q00204000010001000400062F01023Q000100012Q0060017Q00A82Q01000200012Q00EF3Q00013Q00013Q00083Q0003073Q0067657467656E7603093Q004175746F50756E6368030A3Q004669726553657276657203053Q0050756E6368026Q00F03F03043Q007461736B03043Q0077616974029A5Q99A93F00133Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001200013Q000457012Q001200012Q0060016Q000638012Q000D00013Q000457012Q000D00012Q0060016Q002012014Q0003001253010200043Q001253010300054Q002B012Q000300010012833Q00063Q0020405Q00070012532Q0100084Q00A8012Q00020001000457014Q00012Q00EF3Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F53746F6D7003043Q007461736B03053Q00737061776E010B3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000A00013Q000457012Q000A0001001283000100033Q00204000010001000400062F01023Q000100012Q0060017Q00A82Q01000200012Q00EF3Q00013Q00013Q00073Q0003073Q0067657467656E7603093Q004175746F53746F6D70030A3Q004669726553657276657203053Q0053746F6D7003043Q007461736B03043Q0077616974029A5Q99A93F00123Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001100013Q000457012Q001100012Q0060016Q000638012Q000C00013Q000457012Q000C00012Q0060016Q002012014Q0003001253010200044Q002B012Q000200010012833Q00053Q0020405Q00060012532Q0100074Q00A8012Q00020001000457014Q00012Q00EF3Q00017Q00083Q0003073Q0067657467656E76030B3Q004175746F41697264726F70030F3Q004175746F54652Q7269746F72696573010003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01153Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q001400013Q000457012Q00140001001283000100014Q0002000100010002003027000100030004001283000100053Q0020832Q01000100064Q00028Q00010002000100122Q000100073Q00202Q00010001000800062F01023Q000100042Q0060012Q00014Q0060012Q00024Q0060017Q0060012Q00034Q00A82Q01000200012Q00EF3Q00013Q00013Q000D3Q0003073Q0067657467656E76030B3Q004175746F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F030C3Q004175746F47656D54772Q656E03063Q00434672616D652Q033Q006E6577028Q00026Q000840026Q002E402Q01029A5Q99C93F003D3Q0012833Q00014Q00023Q000100020020405Q0002000638012Q003C00013Q000457012Q003C00010012833Q00033Q0020405Q00040012532Q0100054Q00A8012Q000200012Q0060017Q00023Q000100022Q00602Q0100014Q00A42Q01000100020006382Q013Q00013Q000457014Q000100063801023Q00013Q000457014Q0001000638014Q00013Q000457014Q0001001283000300014Q000200030001000200204000030003000600064600033Q00010001000457014Q00010020400003000200070012E8000400073Q00202Q00040004000800122Q000500093Q00122Q0006000A3Q00122Q000700096Q0004000700024Q00030003000400104Q0007000300122Q000300033Q00202Q00030003000400122Q0004000B6Q0003000200014Q000300023Q00202Q00030001000C4Q000300036Q0003000100024Q00048Q00040001000200062Q00033Q00013Q000457014Q000100063801043Q00013Q000457014Q0001001283000500073Q00207A01050005000800122Q000600093Q00122Q0007000A3Q00122Q000800096Q0005000800024Q00050003000500102Q00040007000500122Q000500033Q00202Q00050005000400122Q0006000D4Q00A8010500020001000457014Q00012Q00EF3Q00017Q00083Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F72696573030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E67030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01163Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q001500013Q000457012Q00150001001283000100014Q00782Q010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q00010001000800062F01023Q000100032Q0060017Q0060012Q00014Q0060012Q00024Q00A82Q01000200012Q00EF3Q00013Q00013Q001B3Q0003023Q00543103023Q00543203023Q00543303023Q00543403023Q00543503093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0054652Q7269746F7269657303063Q0069706169727303073Q0067657467656E76030F3Q004175746F54652Q7269746F726965732Q033Q0049734103083Q00426173655061727403063Q00434672616D6503083Q004765745069766F742Q033Q006E6577028Q00026Q00104003083Q0056656C6F6369747903073Q00566563746F7233026Q004EC003043Q007461736B03043Q0077616974029A5Q99A93F029A5Q99B93F012Q00704Q00B33Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q00050001001283000100063Q0020122Q0100010007001253010300084Q004F0001000300020006382Q01001200013Q000457012Q00120001001283000100063Q0020400001000100080020122Q0100010007001253010300094Q004F0001000300020006382Q01006F00013Q000457012Q006F00010012830002000A4Q005501036Q00E1000200020004000457012Q006A00010012830007000B4Q000200070001000200204000070007000C0006460007001E00010001000457012Q001E0001000457012Q006C00010020120107000100072Q0053000900066Q0007000900024Q00088Q00080001000200062Q0007006A00013Q000457012Q006A00010006380108006A00013Q000457012Q006A000100201201090007000D001253010B000E4Q004F0009000B00020006380109002F00013Q000457012Q002F000100204000090007000F0006460009003100010001000457012Q003100010020120109000700102Q00200009000200022Q0060010A00013Q0012A1000B000F3Q00202Q000B000B001100122Q000C00123Q00122Q000D00133Q00122Q000E00126Q000B000E00024Q000B0009000B0002C3000C6Q0030010A000C000100122Q000A000B6Q000A0001000200202Q000A000A000C00062Q000A004100010001000457012Q00410001000457012Q006C00012Q0060010A6Q0002000A00010002000638010A004C00013Q000457012Q004C0001001283000B00153Q00201A010B000B001100122Q000C00123Q00122Q000D00163Q00122Q000E00126Q000B000E000200102Q000A0014000B001283000B00173Q002040000B000B0018001253010C00194Q00A8010B00020001001253010B00124Q0060010C00023Q000615000B006A0001000C000457012Q006A0001001283000C000B4Q0002000C00010002002040000C000C000C000638010C006A00013Q000457012Q006A0001001283000C00173Q002085010C000C001800122Q000D001A6Q000C0002000100202Q000B000B001A4Q000C8Q000C0001000200062Q000C005100013Q000457012Q00510001001283000D00153Q00206E000D000D001100122Q000E00123Q00122Q000F00123Q00122Q001000126Q000D0010000200102Q000C0014000D00044Q005100010006630102001800010002000457012Q001800010012830002000B4Q00020002000100020030270002000C001B2Q00EF3Q00013Q00013Q00023Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F7269657300053Q0012833Q00014Q00023Q000100020020405Q00022Q00793Q00024Q00EF3Q00017Q00063Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01123Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q001100013Q000457012Q00110001001283000100033Q0020832Q01000100044Q00028Q00010002000100122Q000100053Q00202Q00010001000600062F01023Q000100042Q0060012Q00014Q0060012Q00024Q0060012Q00034Q0060017Q00A82Q01000200012Q00EF3Q00013Q00013Q00133Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00506172656E7403083Q00506F736974696F6E03093Q004D61676E6974756465026Q00144003063Q004D6F7665546F029A5Q99C93F03043Q004D6F766503073Q00566563746F723303043Q007A65726F026Q002E402Q01004C3Q0012833Q00014Q00023Q000100020020405Q0002000638012Q004B00013Q000457012Q004B00010012833Q00033Q00208F014Q000400122Q000100058Q000200019Q0000206Q00064Q000100016Q00010001000200062Q0002001200013Q000457012Q0012000100201201023Q0007001253010400084Q004F0002000400022Q0060010300024Q00A401030001000400063801033Q00013Q000457014Q000100063801043Q00013Q000457014Q00010006382Q013Q00013Q000457014Q000100063801023Q00013Q000457014Q0001001283000500014Q00020005000100020020400005000500020006380105003200013Q000457012Q003200010020400005000300090006380105003200013Q000457012Q0032000100204000050001000A00204000060004000A2Q009D01050005000600204000050005000B000E43010C003200010005000457012Q0032000100201201050002000D00204000070004000A2Q002B01050007000100120F010500033Q00202Q00050005000400122Q0006000E6Q00050002000100044Q001C0001001283000500014Q000200050001000200204000050005000200063801053Q00013Q000457014Q000100204000050003000900063801053Q00013Q000457014Q000100204000050001000A00204000060004000A2Q009D01050005000600204000050005000B00264101053Q0001000C000457014Q000100201201050002000F001283000700103Q0020400007000700112Q002B010500070001001283000500033Q002040000500050004001253010600124Q00A80105000200012Q0060010500033Q002027010500030013000457014Q00012Q00EF3Q00017Q000D3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E6703093Q0057616C6B53702Q6564030A3Q0053702Q656456616C756503043Q004D6F766503073Q00566563746F723303043Q007A65726F01223Q001234000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000457012Q000A0001002012010200010004001253010400054Q004F000200040002000638012Q001900013Q000457012Q00190001001283000300014Q003200030001000200302Q00030006000700122Q000300016Q00030001000200302Q00030008000700062Q0002002100013Q000457012Q00210001001283000300014Q000200030001000200204000030003000A00102C010200090003000457012Q002100010006380102002100013Q000457012Q0021000100201201030002000B0012830005000C3Q00204000050005000D2Q002B0103000500012Q0060010300013Q00102C0102000900032Q00EF3Q00017Q00073Q0003073Q0067657467656E76030A3Q0053702Q656456616C7565030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q001243000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q000457012Q001200012Q00602Q015Q0020400001000100040006FA0002000F00010001000457012Q000F0001002012010200010005001253010400064Q004F0002000400020006380102001200013Q000457012Q0012000100102C010200074Q00EF3Q00017Q00093Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03063Q004E6F636C6970030C3Q004175746F47656D4272696E670100030B3Q004175746F47656D57616C6B030F3Q004175746F54652Q7269746F7269657303043Q007461736B03053Q00737061776E011D3Q0012362Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200102Q000100033Q00064Q001C00013Q000457012Q001C0001001283000100014Q00782Q010001000200302Q00010004000500122Q000100016Q00010001000200302Q00010006000500122Q000100016Q00010001000200302Q00010007000500122Q000100083Q00202Q00010001000900062F01023Q000100072Q0060017Q0060012Q00014Q0060012Q00024Q0060012Q00034Q0060012Q00044Q0060012Q00054Q0060012Q00064Q00A82Q01000200012Q00EF3Q00013Q00013Q00133Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03093Q0048656172746265617403043Q0057616974030B3Q004175746F41697264726F7003063Q00434672616D652Q033Q006E6577028Q00026Q00084003163Q00412Q73656D626C794C696E65617256656C6F6369747903073Q00566563746F723303173Q00412Q73656D626C79416E67756C617256656C6F6369747903043Q007461736B03043Q0077616974026Q002E402Q01029A5Q99C93F03043Q004C657270030A3Q0054772Q656E53702Q656400653Q0012833Q00014Q00023Q000100020020405Q0002000638012Q006400013Q000457012Q006400012Q0060016Q00200B014Q000300206Q00046Q000200016Q00018Q0001000200066Q00013Q000457014Q00012Q00602Q0100024Q00902Q010001000200122Q000300016Q00030001000200202Q00030003000500062Q0003004A00013Q000457012Q004A00010006382Q01004A00013Q000457012Q004A00010006380102004A00013Q000457012Q004A000100204000030002000600124D010400063Q00202Q00040004000700122Q000500083Q00122Q000600093Q00122Q000700086Q0004000700024Q00030003000400104Q0006000300122Q0003000B3Q00202Q00030003000700122Q000400083Q00122Q000500083Q00122Q000600086Q00030006000200104Q000A000300122Q0003000B3Q00202Q00030003000700122Q000400083Q00122Q000500083Q00122Q000600086Q00030006000200104Q000C000300122Q0003000D3Q00202Q00030003000E00122Q0004000F6Q0003000200014Q000300033Q00202Q0003000100104Q000300046Q0003000100024Q000400016Q00040001000200062Q00033Q00013Q000457014Q000100063801043Q00013Q000457014Q0001001283000500063Q00207A01050005000700122Q000600083Q00122Q000700093Q00122Q000800086Q0005000800024Q00050003000500102Q00040006000500122Q0005000D3Q00202Q00050005000E00122Q000600114Q00A8010500020001000457014Q00012Q0060010300054Q000200030001000200063801033Q00013Q000457014Q000100204000043Q00060020E700040004001200202Q0006000300064Q000700063Q00202Q0007000700134Q00040007000200104Q0006000400122Q0004000B3Q00201A01040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200104Q000A00040012830004000B3Q00206E00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200104Q000C000400046Q00012Q00EF3Q00017Q000A3Q0003073Q0067657467656E76030C3Q004175746F47656D4272696E67030C3Q004175746F47656D54772Q656E0100030B3Q004175746F47656D57616C6B030F3Q004175746F54652Q7269746F7269657303053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E011B3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q001A00013Q000457012Q001A0001001283000100014Q00DC00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q0001000100084Q00028Q00010002000100122Q000100093Q00202Q00010001000A00062F01023Q000100042Q0060012Q00014Q0060012Q00024Q0060012Q00034Q0060012Q00044Q00A82Q01000200012Q00EF3Q00013Q00013Q000B3Q0003073Q0067657467656E76030C3Q004175746F47656D4272696E6703043Q007461736B03043Q007761697403063Q00434672616D652Q033Q006E657703073Q00566563746F7233028Q00027Q004002B81E85EB51B89E3F026Q00E03F00333Q0012833Q00014Q00023Q000100020020405Q0002000638012Q003200013Q000457012Q003200010012833Q00033Q0020A95Q00044Q00019Q00000200016Q00018Q000100024Q000100026Q00010001000300062Q0001002D00013Q000457012Q002D00010006380102002D00013Q000457012Q002D0001000638012Q002D00013Q000457012Q002D00012Q0060010400034Q0022010500016Q00040002000100202Q00043Q000500122Q000500053Q00202Q00050005000600122Q000600073Q00202Q00060006000600122Q000700083Q00202Q00080003000900202Q000800080009001253010900084Q008E0106000900024Q0006000200064Q00050002000200104Q0005000500122Q000500033Q00202Q00050005000400122Q0006000A6Q0005000200014Q000500016Q00050001000200063801053Q00013Q000457014Q000100102C010500050004000457014Q0001001283000400033Q0020400004000400040012530105000B4Q00A8010400020001000457014Q00012Q00EF3Q00017Q000E3Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03053Q00737061776E03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F726564010001273Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100012Q0060017Q00A82Q0100020001000457012Q00260001001283000100053Q0020122Q0100010006001253010300074Q004F0001000300020006382Q01002600013Q000457012Q00260001001283000100083Q0012BF000200053Q00202Q00020002000700202Q0002000200094Q000200036Q00013Q000300044Q0024000100201201060005000A0012530108000B4Q004F0006000800020006380106002400013Q000457012Q002400010020120106000500060012530108000C4Q004F0006000800020006380106002400013Q000457012Q0024000100204000060005000C0030270006000D000E0006632Q01001800010002000457012Q001800012Q00EF3Q00013Q00013Q00143Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403063Q00434672616D652Q033Q006E6577028Q00026Q001AC0026Q001EC003083Q00416E63686F7265642Q0100343Q0012833Q00014Q00023Q000100020020405Q0002000638012Q003300013Q000457012Q003300010012833Q00033Q00208C5Q000400122Q000100058Q000200019Q006Q0001000200066Q00013Q000457014Q0001001283000100063Q0020122Q0100010007001253010300084Q004F0001000300020006382Q013Q00013Q000457014Q0001001283000100093Q0012BF000200063Q00202Q00020002000800202Q00020002000A4Q000200036Q00013Q000300044Q0030000100201201060005000B0012530108000C4Q004F0006000800020006380106003000013Q000457012Q003000010020120106000500070012530108000D4Q004F0006000800020006380106003000013Q000457012Q0030000100204000060005000D00204000073Q000E0012A10008000E3Q00202Q00080008000F00122Q000900103Q00122Q000A00113Q00122Q000B00126Q0008000B00024Q00070007000800102C0106000E000700204000060005000D0030270006001300140006632Q01001A00010002000457012Q001A0001000457014Q00012Q00EF3Q00017Q00043Q0003073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303043Q007461736B03053Q00737061776E010C3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100022Q0060017Q0060012Q00014Q00A82Q01000200012Q00EF3Q00013Q00013Q001B3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q0052696768744C6F7765724C656703103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E657703083Q00506F736974696F6E03073Q00566563746F7233026Q002E40027Q0040028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004D6F7665546F00723Q001276012Q00013Q00206Q000200122Q000200038Q0002000200064Q000700010001000457012Q000700012Q00EF3Q00014Q006E2Q0100013Q001256010200043Q00202Q00033Q00054Q000300046Q00023Q000400044Q00230001002012010700060006001253010900074Q004F0007000900020006380107002300013Q000457012Q00230001002012010700060002001253010900084Q004F00070009000200068B2Q01002000010007000457012Q00200001002012010700060002001253010900094Q004F00070009000200068B2Q01002000010007000457012Q0020000100201201070006000A0012530109000B4Q004F0007000900022Q00552Q0100073Q0006382Q01002300013Q000457012Q00230001000457012Q002500010006630102000D00010002000457012Q000D00012Q006001026Q00020002000100020006380102003B00013Q000457012Q003B00010006382Q01003B00013Q000457012Q003B00010012830003000C3Q0020A701030003000D00202Q00040001000E00122Q0005000F3Q00202Q00050005000D00122Q000600103Q00122Q000700113Q00122Q000800126Q0005000800024Q0004000400054Q00030002000200102C0102000C0003001283000300133Q002040000300030014001253010400154Q00A8010300020001001283000300164Q00020003000100020020400003000300170006380103007100013Q000457012Q00710001001283000300133Q00206601030003001400122Q000400156Q0003000200014Q000300013Q00202Q00030003001800062Q0004004B00010003000457012Q004B00010020120104000300190012530106001A4Q004F0004000600022Q006E010500053Q001256010600043Q00202Q00073Q00054Q000700086Q00063Q000800044Q00670001002012010B000A0006001253010D00074Q004F000B000D0002000638010B006700013Q000457012Q00670001002012010B000A0002001253010D00084Q004F000B000D000200068B010500640001000B000457012Q00640001002012010B000A0002001253010D00094Q004F000B000D000200068B010500640001000B000457012Q00640001002012010B000A000A001253010D000B4Q004F000B000D00022Q00550105000B3Q0006380105006700013Q000457012Q00670001000457012Q006900010006630106005100010002000457012Q005100010006380104003B00013Q000457012Q003B00010006380105003B00013Q000457012Q003B000100201201060004001B00204000080005000E2Q002B010600080001000457012Q003B00012Q00EF3Q00017Q00063Q0003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C030A3Q0057616C6B546F426F2Q73010003043Q007461736B03053Q00737061776E010E3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000D00013Q000457012Q000D0001001283000100014Q0002000100010002003027000100030004001283000100053Q00204000010001000600062F01023Q000100012Q0060017Q00A82Q01000200012Q00EF3Q00013Q00013Q00153Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03043Q007461736B03043Q0077616974027B14AE47E17A843F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E6577028Q00026Q000C4003083Q0056656C6F6369747903073Q00566563746F723300413Q001276012Q00013Q00206Q000200122Q000200038Q0002000200064Q000700010001000457012Q000700012Q00EF3Q00013Q001283000100044Q00020001000100020020400001000100050006382Q01004000013Q000457012Q00400001001283000100063Q002040000100010007001253010200084Q00A82Q01000200012Q00602Q016Q00020001000100022Q006E010200023Q001256010300093Q00202Q00043Q000A4Q000400056Q00033Q000500044Q0029000100201201080007000B001253010A000C4Q004F0008000A00020006380108002900013Q000457012Q00290001002012010800070002001253010A000D4Q004F0008000A000200068B0102002600010008000457012Q0026000100201201080007000E001253010A000F4Q004F0008000A00022Q0055010200083Q0006380102002900013Q000457012Q00290001000457012Q002B00010006630103001800010002000457012Q001800010006382Q01000700013Q000457012Q000700010006380102000700013Q000457012Q000700010020400003000200100012A1000400103Q00202Q00040004001100122Q000500123Q00122Q000600123Q00122Q000700136Q0004000700024Q00030003000400102C2Q0100100003001283000300153Q00206E00030003001100122Q000400123Q00122Q000500123Q00122Q000600126Q00030006000200102Q00010014000300044Q000700012Q00EF3Q00017Q00023Q0003073Q0067657467656E7603103Q0053656C6563746564452Q67496E64657801043Q001283000100014Q000200010001000200102C2Q0100024Q00EF3Q00017Q00043Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q6703043Q007461736B03053Q00737061776E010D3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000C00013Q000457012Q000C0001001283000100033Q00204000010001000400062F01023Q000100032Q0060017Q0060012Q00014Q0060012Q00024Q00A82Q01000200012Q00EF3Q00013Q00013Q000B3Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012833Q00014Q00023Q000100020020405Q0002000638012Q003000013Q000457012Q003000012Q0060016Q0006463Q001B00010001000457012Q001B00012Q0060012Q00013Q002012014Q0003001253010200044Q004F3Q00020002000638012Q001B00013Q000457012Q001B00012Q0060012Q00013Q00200A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000457012Q001B00012Q0060012Q00013Q002058014Q000400206Q000500206Q000300122Q000200068Q00020002000638012Q002A00013Q000457012Q002A0001001283000100014Q00020001000100020020400001000100070006460001002300010001000457012Q002300010012532Q0100083Q001283000200093Q00204000020002000A00062F01033Q000100022Q0055017Q0055012Q00014Q00A80102000200012Q008400015Q001283000100093Q00203C2Q010001000B4Q000200026Q0001000200019Q0000046Q00012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012833Q00013Q00062F2Q013Q000100022Q0060017Q0060012Q00014Q00A8012Q000200012Q00EF3Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00084003073Q0049736C616E647300074Q0010016Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q6703043Q007461736B03053Q00737061776E010C3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100022Q0060017Q0060012Q00014Q00A82Q01000200012Q00EF3Q00013Q00013Q00093Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703043Q007461736B03053Q00737061776E03043Q007761697400283Q0012833Q00014Q00023Q000100020020405Q0002000638012Q002700013Q000457012Q002700012Q0060016Q0006463Q001B00010001000457012Q001B00012Q0060012Q00013Q002012014Q0003001253010200044Q004F3Q00020002000638012Q001B00013Q000457012Q001B00012Q0060012Q00013Q00200A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000457012Q001B00012Q0060012Q00013Q002058014Q000400206Q000500206Q000300122Q000200068Q00020002000638012Q002200013Q000457012Q00220001001283000100073Q00204000010001000800062F01023Q000100012Q0055017Q00A82Q0100020001001283000100073Q0020400001000100092Q009A2Q01000100012Q00847Q000457014Q00012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012833Q00013Q00062F2Q013Q000100012Q0060017Q00A8012Q000200012Q00EF3Q00013Q00013Q00043Q00030C3Q00496E766F6B65536572766572026Q00F03F026Q00084003093Q0043756265576F726C6400074Q00707Q00206Q000100122Q000200023Q00122Q000300033Q00122Q000400048Q000400016Q00017Q00163Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001503Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q004A00013Q000457012Q004A0001001283000100033Q0020122Q0100010004001253010300054Q004F0001000300020006382Q01002100013Q000457012Q00210001001283000100033Q00200A2Q010001000500202Q00010001000400122Q000300066Q00010003000200062Q0001002100013Q000457012Q00210001001283000100033Q0020582Q010001000500202Q00010001000600202Q00010001000400122Q000300076Q0001000300020006382Q01002100013Q000457012Q00210001001283000100033Q0020400001000100050020582Q010001000600202Q00010001000700202Q00010001000400122Q000300086Q0001000300022Q006001026Q00020002000100020006380102003F00013Q000457012Q003F00010006382Q01003F00013Q000457012Q003F00010020120103000100090012530105000A4Q004F0003000500020006380103003000013Q000457012Q0030000100201201030001000B2Q00200003000200020006460003003100010001000457012Q0031000100204000030001000C0012830004000C3Q00207C00040004000D00122Q0005000E3Q00122Q0006000F3Q00122Q0007000E6Q0004000700024Q00040003000400102Q0002000C000400122Q000400103Q00202Q00040004001100122Q000500126Q00040002000100302Q00020013001400044Q004200010006380102004200013Q000457012Q00420001003027000200130014001283000300103Q00204000030003001500062F01043Q000100032Q0060012Q00014Q0060012Q00024Q0060012Q00034Q00A8010300020001000457012Q004F00012Q00602Q016Q00020001000100020006382Q01004F00013Q000457012Q004F00010030270001001300162Q00EF3Q00013Q00013Q00083Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001F00013Q000457012Q001F00012Q0060016Q00204F014Q000300206Q00046Q000200016Q00013Q00064Q001700010001000457012Q001700012Q0060012Q00023Q002012014Q0005001253010200064Q004F3Q00020002000638012Q001700013Q000457012Q001700012Q0060012Q00023Q0020405Q0006002012014Q0005001253010200074Q004F3Q00020002000638012Q001D00013Q000457012Q001D0001001283000100083Q00062F01023Q000100012Q0055017Q00A82Q01000200012Q00847Q000457014Q00012Q00EF3Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q0060016Q002012014Q00012Q00A8012Q000200012Q00EF3Q00017Q00043Q0003073Q0067657467656E76030E3Q004175746F4275795765696768747303043Q007461736B03053Q00737061776E010C3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100022Q0060017Q0060012Q00014Q00A82Q01000200012Q00EF3Q00013Q00013Q000A3Q0003073Q0067657467656E76030E3Q004175746F42757957656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012833Q00014Q00023Q000100020020405Q0002000638012Q002800013Q000457012Q002800012Q0060016Q0006463Q001B00010001000457012Q001B00012Q0060012Q00013Q002012014Q0003001253010200044Q004F3Q00020002000638012Q001B00013Q000457012Q001B00012Q0060012Q00013Q00200A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000457012Q001B00012Q0060012Q00013Q002058014Q000400206Q000500206Q000300122Q000200068Q00020002000638012Q002200013Q000457012Q00220001001283000100073Q00204000010001000800062F01023Q000100012Q0055017Q00A82Q0100020001001283000100073Q00203300010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012833Q00013Q00062F2Q013Q000100012Q0060017Q00A8012Q000200012Q00EF3Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q0057656967687403073Q0049736C616E647300064Q00E27Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q00043Q0003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E010C3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100022Q0060017Q0060012Q00014Q00A82Q01000200012Q00EF3Q00013Q00013Q000A3Q0003073Q0067657467656E76030A3Q004175746F427579444E41030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012833Q00014Q00023Q000100020020405Q0002000638012Q002800013Q000457012Q002800012Q0060016Q0006463Q001B00010001000457012Q001B00012Q0060012Q00013Q002012014Q0003001253010200044Q004F3Q00020002000638012Q001B00013Q000457012Q001B00012Q0060012Q00013Q00200A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000457012Q001B00012Q0060012Q00013Q002058014Q000400206Q000500206Q000300122Q000200068Q00020002000638012Q002200013Q000457012Q00220001001283000100073Q00204000010001000800062F01023Q000100012Q0055017Q00A82Q0100020001001283000100073Q00203300010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00EF3Q00013Q00013Q00063Q00026Q00F03F026Q005E4003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E00133Q001253012Q00013Q0012532Q0100023Q001253010200013Q00044B012Q00120001001283000400034Q00020004000100020020400004000400040006460004000A00010001000457012Q000A0001000457012Q00120001001283000400053Q00204000040004000600062F01053Q000100022Q0060017Q0055012Q00034Q00A80104000200012Q008400035Q000415012Q000400012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012833Q00013Q00062F2Q013Q000100022Q0060017Q0060012Q00014Q00A8012Q000200012Q00EF3Q00013Q00013Q00033Q00030C3Q00496E766F6B655365727665722Q033Q00444E4103073Q0049736C616E647300074Q0010016Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E010C3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100022Q0060017Q0060012Q00014Q00A82Q01000200012Q00EF3Q00013Q00013Q000A3Q0003073Q0067657467656E76030D3Q004175746F427579426F64696573030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012833Q00014Q00023Q000100020020405Q0002000638012Q002800013Q000457012Q002800012Q0060016Q0006463Q001B00010001000457012Q001B00012Q0060012Q00013Q002012014Q0003001253010200044Q004F3Q00020002000638012Q001B00013Q000457012Q001B00012Q0060012Q00013Q00200A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000457012Q001B00012Q0060012Q00013Q002058014Q000400206Q000500206Q000300122Q000200068Q00020002000638012Q002200013Q000457012Q00220001001283000100073Q00204000010001000800062F01023Q000100012Q0055017Q00A82Q0100020001001283000100073Q00203300010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00EF3Q00013Q00013Q00073Q00027Q0040025Q00802Q40026Q00F03F03073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E00133Q001253012Q00013Q0012532Q0100023Q001253010200033Q00044B012Q00120001001283000400044Q00020004000100020020400004000400050006460004000A00010001000457012Q000A0001000457012Q00120001001283000400063Q00204000040004000700062F01053Q000100022Q0060017Q0055012Q00034Q00A80104000200012Q008400035Q000415012Q000400012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012833Q00013Q00062F2Q013Q000100022Q0060017Q0060012Q00014Q00A8012Q000200012Q00EF3Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572030B3Q00426F64795570677261646503073Q0049736C616E647300074Q0010016Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B65745765696768747303043Q007461736B03053Q00737061776E010C3Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q000B00013Q000457012Q000B0001001283000100033Q00204000010001000400062F01023Q000100022Q0060017Q0060012Q00014Q00A82Q01000200012Q00EF3Q00013Q00013Q000A3Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B657457656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012833Q00014Q00023Q000100020020405Q0002000638012Q002800013Q000457012Q002800012Q0060016Q0006463Q001B00010001000457012Q001B00012Q0060012Q00013Q002012014Q0003001253010200044Q004F3Q00020002000638012Q001B00013Q000457012Q001B00012Q0060012Q00013Q00200A014Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000457012Q001B00012Q0060012Q00013Q002058014Q000400206Q000500206Q000300122Q000200068Q00020002000638012Q002200013Q000457012Q00220001001283000100073Q00204000010001000800062F01023Q000100012Q0055017Q00A82Q0100020001001283000100073Q00203300010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q00EF3Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012833Q00013Q00062F2Q013Q000100012Q0060017Q00A8012Q000200012Q00EF3Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q00576569676874030B3Q0053757065726D61726B657400064Q00E27Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q001A3Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B657403053Q0053686F707303093Q0052696E67417265617303063Q00536572766572030B3Q0052616E676553797374656D030F3Q0053757065726D61726B657453652Q6C03043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001633Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q005D00013Q000457012Q005D0001001283000100033Q0020122Q0100010004001253010300054Q004F0001000300020006FA0002000E00010001000457012Q000E0001002012010200010004001253010400064Q004F0002000400022Q006E010300033Q0006380102003400013Q000457012Q00340001002012010400020004001253010600074Q004F0004000600020006460004001900010001000457012Q00190001002012010400020004001253010600084Q004F0004000600020006FA0005002900010004000457012Q00290001002012010500040004001253010700094Q004F0005000700020006460005002900010001000457012Q002900010020120105000400040012530107000A4Q004F0005000700020006380105002900013Q000457012Q0029000100204000050004000A002012010500050004001253010700094Q004F0005000700020006380105003400013Q000457012Q003400010020120106000500040012530108000B4Q004F00060008000200068B0103003400010006000457012Q003400010020120106000500040012530108000C4Q004F0006000800022Q0055010300064Q006001046Q00020004000100020006380104005200013Q000457012Q005200010006380103005200013Q000457012Q0052000100201201050003000D0012530107000E4Q004F0005000700020006380105004300013Q000457012Q0043000100201201050003000F2Q00200005000200020006460005004400010001000457012Q00440001002040000500030010001283000600103Q00207C00060006001100122Q000700123Q00122Q000800133Q00122Q000900126Q0006000900024Q00060005000600102Q00040010000600122Q000600143Q00202Q00060006001500122Q000700166Q00060002000100302Q00040017001800044Q005500010006380104005500013Q000457012Q00550001003027000400170018001283000500143Q00204000050005001900062F01063Q000100032Q0060012Q00014Q0060012Q00024Q0060012Q00034Q00A8010500020001000457012Q006200012Q00602Q016Q00020001000100020006382Q01006200013Q000457012Q0062000100302700010017001A2Q00EF3Q00013Q00013Q00083Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001F00013Q000457012Q001F00012Q0060016Q00204F014Q000300206Q00046Q000200016Q00013Q00064Q001700010001000457012Q001700012Q0060012Q00023Q002012014Q0005001253010200064Q004F3Q00020002000638012Q001700013Q000457012Q001700012Q0060012Q00023Q0020405Q0006002012014Q0005001253010200074Q004F3Q00020002000638012Q001D00013Q000457012Q001D0001001283000100083Q00062F01023Q000100012Q0055017Q00A82Q01000200012Q00847Q000457014Q00012Q00EF3Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q0060016Q002012014Q00012Q00A8012Q000200012Q00EF3Q00017Q00083Q0003073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E0100030C3Q004175746F47656D4272696E67030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01163Q001283000100014Q000200010001000200102C2Q0100023Q000638012Q001500013Q000457012Q00150001001283000100014Q00782Q010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100016Q00010001000200302Q00010006000400122Q000100073Q00202Q00010001000800062F01023Q000100032Q0060017Q0060012Q00014Q0060012Q00024Q00A82Q01000200012Q00EF3Q00013Q00013Q001A3Q0003023Q00543103023Q00543203023Q00543303093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B6574030B3Q0054652Q7269746F7269657303063Q0069706169727303073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F726965732Q033Q0049734103083Q00426173655061727403063Q00434672616D6503083Q004765745069766F742Q033Q006E6577028Q00026Q00104003083Q0056656C6F6369747903073Q00566563746F7233026Q004EC003043Q007461736B03043Q0077616974029A5Q99A93F029A5Q99B93F012Q00714Q0008012Q00033Q0012532Q0100013Q001253010200023Q001253010300034Q00133Q00030001001283000100043Q0020122Q0100010005001253010300064Q004F0001000300020006FA0002000E00010001000457012Q000E0001002012010200010005001253010400074Q004F0002000400020006FA0003001300010002000457012Q00130001002012010300020005001253010500084Q004F0003000500020006380103007000013Q000457012Q00700001001283000400094Q005501056Q00E1000400020006000457012Q006B00010012830009000A4Q000200090001000200204000090009000B0006460009001F00010001000457012Q001F0001000457012Q006D00010020120109000300052Q0053000B00086Q0009000B00024Q000A8Q000A0001000200062Q0009006B00013Q000457012Q006B0001000638010A006B00013Q000457012Q006B0001002012010B0009000C001253010D000D4Q004F000B000D0002000638010B003000013Q000457012Q00300001002040000B0009000E000646000B003200010001000457012Q00320001002012010B0009000F2Q0020000B000200022Q0060010C00013Q0012A1000D000E3Q00202Q000D000D001000122Q000E00113Q00122Q000F00123Q00122Q001000116Q000D001000024Q000D000B000D0002C3000E6Q0030010C000E000100122Q000C000A6Q000C0001000200202Q000C000C000B00062Q000C004200010001000457012Q00420001000457012Q006D00012Q0060010C6Q0002000C00010002000638010C004D00013Q000457012Q004D0001001283000D00143Q00201A010D000D001000122Q000E00113Q00122Q000F00153Q00122Q001000116Q000D0010000200102Q000C0013000D001283000D00163Q002040000D000D0017001253010E00184Q00A8010D00020001001253010D00114Q0060010E00023Q000615000D006B0001000E000457012Q006B0001001283000E000A4Q0002000E00010002002040000E000E000B000638010E006B00013Q000457012Q006B0001001283000E00163Q002085010E000E001700122Q000F00196Q000E0002000100202Q000D000D00194Q000E8Q000E0001000200062Q000E005200013Q000457012Q00520001001283000F00143Q00206E000F000F001000122Q001000113Q00122Q001100113Q00122Q001200116Q000F0012000200102Q000E0013000F00044Q005200010006630104001900010002000457012Q001900010012830004000A4Q00020004000100020030270004000B001A2Q00EF3Q00013Q00013Q00023Q0003073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F7269657300053Q0012833Q00014Q00023Q000100020020405Q00022Q00793Q00024Q00EF3Q00017Q00043Q0003073Q00456E61626C656403073Q0067657467656E76030B3Q0057616C6B546F4375626573010001084Q00602Q015Q00102C2Q0100013Q000638012Q000700013Q000457012Q00070001001283000100024Q00020001000100020030270001000300042Q00EF3Q00017Q00013Q0003053Q0053702Q656401034Q00602Q015Q00102C2Q0100014Q00EF3Q00017Q00053Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E7303093Q0043756265576F726C6403063Q0047726F756E6400103Q0012853Q00013Q00206Q000200122Q000200038Q0002000200062Q0001000900013Q000457012Q000900010020122Q013Q0002001253010300044Q004F0001000300020006FA0002000E00010001000457012Q000E0001002012010200010002001253010400054Q004F0002000400022Q0079000200024Q00EF3Q00017Q00083Q00030F3Q0067657447726F756E64466F6C6465722Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903043Q0053697A65027Q0040001E4Q0060016Q0020405Q00012Q00023Q000100020006463Q000700010001000457012Q000700012Q006E2Q0100014Q0079000100023Q0020122Q013Q0002001253010300034Q004F0001000300020006382Q01000E00013Q000457012Q000E000100068B2Q01001200013Q000457012Q001200010020122Q013Q0004001253010300034Q009F000400014Q004F0001000400020006460001001600010001000457012Q001600012Q006E010200024Q0079000200023Q00204000020001000500209100020002000600202Q00030001000700202Q00030003000600202Q0003000300084Q0002000200034Q000200028Q00017Q00133Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F7073030C3Q0067657447726F756E64546F7003063Q00697061697273030B3Q004765744368696C6472656E03093Q00626C61636B6C697374030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03013Q0059027Q00C003093Q006D617848656967687403053Q007461626C6503063Q00696E7365727403043Q006375626503043Q0070617274003B3Q0012713Q00013Q00206Q000200122Q000200038Q000200024Q00015Q00202Q0001000100044Q00010001000200064Q000B00013Q000457012Q000B00010006460001000D00010001000457012Q000D00012Q000801026Q0079000200024Q000801025Q001256010300053Q00202Q00043Q00064Q000400056Q00033Q000500044Q003700012Q006001085Q0020400008000800072Q008D0008000800070006460008003700010001000457012Q00370001002012010800070002001253010A00084Q004F0008000A00020006460008002500010001000457012Q00250001002012010800070002001253010A00094Q004F0008000A00020006460008002500010001000457012Q0025000100201201080007000A001253010A000B4Q004F0008000A00020006380108003700013Q000457012Q0037000100204000090008000C00204000090009000D2Q009D010900090001000EC8000E003700010009000457012Q003700012Q0060010A5Q002040000A000A000F000674000900370001000A000457012Q00370001001283000A00103Q00207B000A000A00114Q000B00026Q000C3Q000200102Q000C0012000700102Q000C001300084Q000A000C00010006630103001300010002000457012Q001300012Q0079000200024Q00EF3Q00017Q00063Q0003073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465020D3Q0012BE000200013Q00202Q00020002000200202Q00033Q000300202Q0004000100034Q00030003000400122Q000400043Q00202Q00053Q000500202Q0006000100054Q0005000500064Q0002000500020020400002000200062Q0079000200024Q00EF3Q00017Q00083Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303073Q0073652Q73696F6E026Q00F03F03073Q00456E61626C6564010003043Q007461736B03053Q00737061776E011A3Q0012FE000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D00010001000457012Q000D00012Q00EF3Q00014Q0060010200013Q003027000200050006001283000200073Q00204000020002000800062F01033Q000100062Q0060012Q00024Q0055012Q00014Q0060017Q0060012Q00034Q0060012Q00044Q0060012Q00054Q00A80102000200012Q00EF3Q00013Q00013Q00203Q0003043Q006D61746803043Q0068756765028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q006375626503063Q00506172656E7403043Q007061727403063Q00697061697273030D3Q00676574466C2Q6F72437562657303083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03073Q0067657467656E76030D3Q0057616C6B437562654465627567030C3Q0067657447726F756E64546F7003053Q007072696E7403353Q005B57616C6B437562655D2074617267657420257320646973743D252E31662068656967687441626F766547726F756E643D252E316603063Q00666F726D617403043Q004E616D6503013Q005903063Q004D6F7665546F027Q0040026Q001840026Q00244003043Q004D6F766503073Q00566563746F723303043Q007A65726F00983Q001283000100013Q002040000100010002001253010200033Q00062F01033Q000100032Q0060017Q0060012Q00014Q0060012Q00023Q00062F01040001000100012Q0060012Q00024Q0055010500034Q00020005000100020006380105008800013Q000457012Q00880001001283000500043Q00206601050005000500122Q000600066Q0005000200014Q000500033Q00202Q00050005000700062Q0006001800010005000457012Q00180001002012010600050008001253010800094Q004F0006000800022Q0060010700044Q00020007000100020006380106001E00013Q000457012Q001E00010006460007002000010001000457012Q002000012Q006E016Q000457012Q00090001000638012Q002B00013Q000457012Q002B000100204000083Q000A00204000080008000B0006380108002A00013Q000457012Q002A000100204000083Q000C00204000080008000B0006460008002B00010001000457012Q002B00012Q006E016Q0006463Q006300010001000457012Q006300012Q006E010800083Q00121D000900013Q00202Q00090009000200122Q000A000D6Q000B00023Q00202Q000B000B000E4Q000B00016Q000A3Q000C00044Q004000012Q0060010F00053Q00206B00100007000F00202Q0011000E000C00202Q00110011000F4Q000F0011000200062Q000F004000010009000457012Q004000012Q00550110000E4Q00550109000F4Q0055010800103Q000663010A003600010002000457012Q003600010006380108006300013Q000457012Q006300012Q0055010A00084Q0005010B00093Q00122Q000C00103Q00202Q000C000C00114Q000C000100024Q0002000C6Q0001000B8Q000A3Q00122Q000A00126Q000A0001000200202Q000A000A001300062Q000A006300013Q000457012Q006300012Q0060010A00023Q002040000A000A00142Q0002000A00010002000646000A005700010001000457012Q00570001001253010A00033Q001283000B00153Q001253010C00163Q002012010C000C0017002040000E0008000A002040000E000E00182Q0055010F00093Q00204000100008000C00204000100010000F0020400010001000192Q009D01100010000A2Q00D4000C00104Q0092010B3Q0001000638012Q008400013Q000457012Q008400012Q0060010800053Q00204000090007000F002040000A3Q000C002040000A000A000F2Q004F0008000A000200201201090006001A002040000B3Q000C002040000B000B000F2Q002B0109000B000100209800090001001B0006150008007800010009000457012Q007800012Q0055010900083Q0012A9010A00103Q00202Q000A000A00114Q000A000100024Q0002000A6Q000100093Q00044Q00090001001283000900103Q0020400009000900112Q00020009000100022Q009D010900090002000E43011C000900010009000457012Q000900012Q0055010900043Q00208A010A3Q000A00122Q000B001D6Q0009000B00019Q0000044Q0009000100201201080006001A002040000A0007000F2Q002B0108000A0001000457012Q000900012Q0060010500033Q0020400005000500070006380105009100013Q000457012Q009100012Q0060010500033Q002040000500050007002012010500050008001253010700094Q004F0005000700020006380105009700013Q000457012Q0097000100201201060005001E0012830008001F3Q0020400008000800202Q002B0106000800012Q00EF3Q00013Q00023Q00043Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303053Q00416C69766503073Q0073652Q73696F6E00123Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001000013Q000457012Q001000012Q0060016Q0020405Q0003000638012Q001000013Q000457012Q001000012Q0060012Q00014Q00602Q0100023Q0020400001000100040006F23Q000F00010001000457012Q000F00012Q00A58Q009F3Q00014Q00793Q00024Q00EF3Q00017Q000B3Q0003073Q0067657467656E76030D3Q0057616C6B43756265446562756703053Q007072696E74031E3Q005B57616C6B437562655D20736B692Q70696E6720737475636B206375626503043Q004E616D652Q033Q00666F7203013Q007303093Q00626C61636B6C6973742Q0103043Q007461736B03053Q0064656C617902173Q001283000200014Q00020002000100020020400002000200020006380102000C00013Q000457012Q000C0001001283000200033Q00123B010300043Q00202Q00043Q000500122Q000500066Q000600013Q00122Q000700076Q0002000700012Q006001025Q00204A01020002000800202Q00023Q000900122Q0002000A3Q00202Q00020002000B4Q000300013Q00062F01043Q000100022Q0060017Q0055017Q002B0102000400012Q00EF3Q00013Q00013Q00023Q0003093Q00626C61636B6C6973742Q00054Q0084016Q00206Q00014Q000100013Q00204Q000100026Q00017Q00033Q0003073Q0067657467656E7603093Q0044697361626C65334403153Q00536574336452656E646572696E67456E61626C656401083Q001283000100014Q000200010001000200102C2Q0100024Q00602Q015Q0020122Q01000100032Q00A800036Q002B2Q01000300012Q00EF3Q00017Q00023Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E01043Q001283000100014Q000200010001000200102C2Q0100024Q00EF3Q00017Q00033Q00034Q0003053Q007063612Q6C2Q0100153Q001253012Q00013Q001283000100023Q00062F01023Q000100022Q0055017Q0060017Q00A82Q01000200010026B53Q000A00010001000457012Q000A00012Q009F000100014Q0079000100023Q001283000100023Q0002C3000200014Q00E10001000200020006FA0003001300010001000457012Q001300010026B50002001200010003000457012Q001200012Q00A500036Q009F000300014Q0079000300024Q00EF3Q00013Q00023Q00013Q00030F3Q00476574452Q726F724D652Q7361676500054Q0040012Q00013Q00206Q00016Q000200029Q006Q00017Q00083Q0003043Q0067616D65030A3Q004765745365727669636503073Q00436F7265477569030E3Q0046696E6446697273744368696C64030F3Q00526F626C6F7850726F6D7074477569030D3Q0070726F6D70744F7665726C617900030B3Q00452Q726F7250726F6D707400173Q00128F3Q00013Q00206Q000200122Q000200038Q0002000200202Q00013Q000400122Q000300056Q00010003000200062Q0002000C00010001000457012Q000C0001002012010200010004001253010400064Q004F0002000400020026B50002001300010007000457012Q00130001002012010300020004001253010500084Q004F00030005000200267C0103001400010007000457012Q001400012Q00A500036Q009F000300014Q0079000300024Q00EF3Q00017Q00083Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203073Q0073652Q73696F6E026Q00F03F03043Q0062757379010003043Q007461736B03053Q00737061776E01193Q001237000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q0001000100034Q00025Q00302Q00020005000600064Q000F00010001000457012Q000F00012Q00EF3Q00013Q001283000200073Q00204000020002000800062F01033Q000100052Q0060012Q00014Q0055012Q00014Q0060017Q0060012Q00024Q0060012Q00034Q00A80102000200012Q00EF3Q00013Q00013Q00173Q0003043Q007461736B03043Q0077616974026Q00F03F030E3Q006973446973636F2Q6E656374656403043Q00627573792Q01028Q0003123Q0054656C65706F7274496E69744661696C656403073Q00436F2Q6E65637403113Q0071756575655F6F6E5F74656C65706F727403073Q0067657467656E7603123Q0056656C6F636974794C6F6164737472696E6703053Q007063612Q6C03023Q006F7303053Q00636C6F636B026Q002E40026Q00E03F030A3Q00446973636F2Q6E65637403043Q006D6174682Q033Q006D696E026Q000840026Q003E40012Q005C3Q00062F014Q000100032Q0060017Q0060012Q00014Q0060012Q00024Q00552Q016Q00020001000100020006382Q01005B00013Q000457012Q005B0001001283000100013Q0020772Q010001000200122Q000200036Q0001000200014Q000100023Q00202Q0001000100044Q00010001000200062Q0001000400013Q000457012Q000400012Q00602Q0100023Q0020400001000100050006460001000400010001000457012Q000400012Q00602Q0100023Q0030270001000500060012532Q0100074Q005501026Q00020002000100020006380102005800013Q000457012Q0058000100209D0001000100032Q009F00026Q0060010300033Q00204000030003000800201201030003000900062F01050001000100012Q0055012Q00024Q004F0003000500020012830004000A3Q0006380104003200013Q000457012Q003200010012830004000B4Q000200040001000200204000040004000C0006380104003200013Q000457012Q003200010012830004000D3Q00124C0105000A3Q00122Q0006000B6Q00060001000200202Q00060006000C4Q0004000600010012830004000D3Q00062F01050002000100032Q0060012Q00034Q0060012Q00024Q0060012Q00044Q00A80104000200010012830004000E3Q00204000040004000F2Q00020004000100022Q005501056Q00020005000100020006380105004C00013Q000457012Q004C00010006460002004C00010001000457012Q004C00010012830005000E3Q00204000050005000F2Q00020005000100022Q009D0105000500040026570005004C00010010000457012Q004C0001001283000500013Q002040000500050002001253010600114Q00A8010500020001000457012Q003B00010020120105000300122Q003B00050002000100122Q000500013Q00202Q00050005000200122Q000600133Q00202Q00060006001400202Q00070001001500122Q000800166Q000600086Q00053Q00014Q00025Q00044Q001800012Q0060010200023Q003027000200050017000457012Q000400012Q00EF3Q00013Q00033Q00043Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203053Q00416C69766503073Q0073652Q73696F6E00123Q0012833Q00014Q00023Q000100020020405Q0002000638012Q001000013Q000457012Q001000012Q0060016Q0020405Q0003000638012Q001000013Q000457012Q001000012Q0060012Q00014Q00602Q0100023Q0020400001000100040006F23Q000F00010001000457012Q000F00012Q00A58Q009F3Q00014Q00793Q00024Q00EF3Q00019Q003Q00034Q009F3Q00014Q00CF8Q00EF3Q00017Q00033Q0003173Q0054656C65706F7274546F506C616365496E7374616E636503073Q00706C616365496403053Q006A6F62496400094Q0060016Q002012014Q00012Q0060010200013Q00205E0002000200024Q000300013Q00202Q0003000300034Q000400028Q000400012Q00EF3Q00017Q00073Q0003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C756501163Q001234000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000457012Q000A0001002012010200010004001253010400054Q004F0002000400020006380102001500013Q000457012Q00150001000638012Q001300013Q000457012Q00130001001283000300014Q000200030001000200204000030003000700102C010200060003000457012Q001500012Q0060010300013Q00102C0102000600032Q00EF3Q00017Q00073Q0003073Q0067657467656E76030E3Q0057616C6B53702Q656456616C7565030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q001243000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q000457012Q001200012Q00602Q015Q0020400001000100040006FA0002000F00010001000457012Q000F0001002012010200010005001253010400064Q004F0002000400020006380102001200013Q000457012Q0012000100102C010200074Q00EF3Q00017Q000A3Q0003073Q0067657467656E76030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C7565026Q00494001163Q001234000100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000457012Q000A0001002012010200010004001253010400054Q004F0002000400020006380102001500013Q000457012Q00150001000638012Q001400013Q000457012Q001400010030270002000600070012F8000300016Q00030001000200202Q00030003000900102Q00020008000300044Q0015000100302700020008000A2Q00EF3Q00017Q00093Q0003073Q0067657467656E76030E3Q004A756D70506F77657256616C7565030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F77657201143Q001243000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001300013Q000457012Q001300012Q00602Q015Q0020400001000100040006FA0002000F00010001000457012Q000F0001002012010200010005001253010400064Q004F0002000400020006380102001300013Q000457012Q0013000100302700020007000800102C010200094Q00EF3Q00017Q00023Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7001043Q001283000100014Q000200010001000200102C2Q0100024Q00EF3Q00017Q00043Q0003083Q00746F737472696E6703043Q0067737562030C3Q005E25732A282E2D2925732A2403023Q00253101093Q00123D000100016Q00028Q00010002000200202Q00010001000200122Q000300033Q00122Q000400046Q0001000400024Q000100028Q00017Q00123Q00028Q0003063Q00737472696E6703053Q00752Q70657203043Q006773756203013Q002C034Q002Q033Q0025732B026Q00F03F03043Q0066696E6403013Q004B025Q00408F4003013Q004D024Q0080842E4103013Q0042024Q00652QCD4103013Q0054023Q00A0941A6D4203083Q00746F6E756D626572014D3Q0006463Q000400010001000457012Q000400010012532Q0100014Q0079000100023Q001283000100023Q0020310001000100034Q00028Q0001000200026Q00013Q00202Q00013Q000400122Q000300053Q00122Q000400066Q00010004000200202Q00010001000400122Q000300073Q001253010400064Q004F0001000400022Q0055012Q00013Q0012C0000100083Q00202Q00023Q000900122Q0004000A6Q00020004000200062Q0002001F00013Q000457012Q001F00010012532Q01000B3Q00202E00023Q000400122Q0004000A3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201201023Q00090012530104000C4Q004F0002000400020006380102002B00013Q000457012Q002B00010012532Q01000D3Q00202E00023Q000400122Q0004000C3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201201023Q00090012530104000E4Q004F0002000400020006380102003700013Q000457012Q003700010012532Q01000F3Q00202E00023Q000400122Q0004000E3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100201201023Q0009001253010400104Q004F0002000400020006380102004200013Q000457012Q004200010012532Q0100113Q00201201023Q0004001253010400103Q001253010500064Q004F0002000500022Q0055012Q00023Q001283000200124Q005501036Q00200002000200020006380102004A00013Q000457012Q004A00012Q00DE0003000200010006460003004B00010001000457012Q004B0001001253010300014Q0079000300024Q00EF3Q00017Q000E3Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D6503063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A65637403053Q007461626C6503063Q00696E7365727403043Q004E616D6503043Q00736F7274003A4Q00A07Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q000457012Q001E00010020122Q013Q0001001253010300034Q004F0001000300020006382Q01001E00013Q000457012Q001E000100204000013Q00030020122Q0100010001001253010300044Q004F0001000300020006382Q01001E00013Q000457012Q001E000100204000013Q000300200A2Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q000457012Q001E000100204000013Q00030020582Q010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006460001002200010001000457012Q002200012Q006E010200024Q0079000200024Q000801025Q001256010300073Q00202Q0004000100084Q000400056Q00033Q000500044Q00320001002012010800070009001253010A000A4Q004F0008000A00020006380108003200013Q000457012Q003200010012830008000B3Q00204000080008000C2Q0055010900023Q002040000A0007000D2Q002B0108000A00010006630103002800010002000457012Q002800010012830003000B3Q00204000030003000E2Q0055010400024Q00A80103000200012Q0079000200024Q00EF3Q00017Q00163Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D65028Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A656374026Q00F03F030E3Q0047657444657363656E64616E747303073Q0056697369626C6503043Q004E616D6503063Q004C6F636B656403043Q004C6F636B03073Q00556E6B6E6F776E030A3Q00496D6167654C6162656C030B3Q00496D616765436F6C6F723303063Q00436F6C6F72332Q033Q006E657700634Q00A07Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q000457012Q001E00010020122Q013Q0001001253010300034Q004F0001000300020006382Q01001E00013Q000457012Q001E000100204000013Q00030020122Q0100010001001253010300044Q004F0001000300020006382Q01001E00013Q000457012Q001E000100204000013Q000300200A2Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q000457012Q001E000100204000013Q00030020582Q010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006460001002200010001000457012Q002200012Q006E010200024Q0079000200023Q001253010200073Q001253010300073Q001256010400083Q00202Q0005000100094Q000500066Q00043Q000600044Q005D000100201201090008000A001253010B000B4Q004F0009000B00020006380109005D00013Q000457012Q005D000100209D00030003000C2Q001900095Q00122Q000A00083Q00202Q000B0008000D4Q000B000C6Q000A3Q000C00044Q00580001002012010F000E000A0012530111000B4Q004F000F00110002000638010F004800013Q000457012Q00480001002040000F000E000E000638010F004800013Q000457012Q00480001002040000F000E000F0026B5000F004600010010000457012Q00460001002040000F000E000F0026B5000F004600010011000457012Q00460001002040000F000E000F00267C010F004800010012000457012Q004800012Q009F000900013Q000457012Q005A0001002012010F000E000A001253011100134Q004F000F00110002000638010F005800013Q000457012Q00580001002040000F000E00140012EC001000153Q00202Q00100010001600122Q001100073Q00122Q001200073Q00122Q001300076Q00100013000200062Q000F005800010010000457012Q005800012Q009F000900013Q000457012Q005A0001000663010A003500010002000457012Q003500010006460009005D00010001000457012Q005D000100209D00020002000C0006630104002900010002000457012Q002900012Q0055010400024Q0055010500034Q000D000400034Q00EF3Q00017Q00043Q00030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657200114Q0099016Q00206Q000100122Q000200028Q0002000200064Q000A00013Q000457012Q000A00010020122Q013Q0001001253010300034Q004F0001000300022Q0055012Q00013Q0006FA0001000F00013Q000457012Q000F00010020122Q013Q0001001253010300044Q004F0001000300022Q0079000100024Q00EF3Q00017Q00193Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q6665727303063Q0069706169727303053Q00546965723103053Q00546965723203053Q0054696572332Q033Q005065742Q033Q0049734103093Q00546578744C6162656C03073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03043Q006E616D6503043Q005465787403053Q00707269636503013Q003F03093Q00617661696C61626C6503073Q0056697369626C65034Q0003023Q00783000674Q00A07Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q000457012Q001E00010020122Q013Q0001001253010300034Q004F0001000300020006382Q01001E00013Q000457012Q001E000100204000013Q00030020122Q0100010001001253010300044Q004F0001000300020006382Q01001E00013Q000457012Q001E000100204000013Q000300200A2Q010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q000457012Q001E000100204000013Q00030020582Q010001000400202Q00010001000500202Q00010001000100122Q000300066Q0001000300020006460001002200010001000457012Q002200012Q006E010200024Q0079000200024Q000801025Q00123F010300076Q000400033Q00122Q000500083Q00122Q000600093Q00122Q0007000A6Q0004000300012Q00E1000300020005000457012Q006300010020120108000100012Q0055010A00074Q004F0008000A00020006FA0009003300010008000457012Q00330001002012010900080001001253010B000B4Q004F0009000B00020006380109006300013Q000457012Q00630001002012010A0009000C001253010C000D4Q004F000A000C0002000638010A006300013Q000457012Q00630001002012010A000800010012D2000C000E6Q000A000C000200202Q000B0008000100122Q000D000F6Q000B000D000200202Q000C0008000100122Q000E00106Q000C000E000200062Q000D00480001000C000457012Q00480001002012010D000C0001001253010F00114Q004F000D000F00022Q0008010E3Q0003002040000F0009001300102C010E0012000F000638010D005000013Q000457012Q00500001002040000F000D0013000646000F005100010001000457012Q00510001001253010F00153Q00102C010E0014000F000638010A005700013Q000457012Q00570001002040000F000A0017000646000F005F00010001000457012Q005F0001000638010B005C00013Q000457012Q005C0001002040000F000B0013000646000F005D00010001000457012Q005D0001001253010F00183Q00267C010F006000010019000457012Q006000012Q00A5000F6Q009F000F00013Q00102C010E0016000F2Q005500020007000E0006630103002B00010002000457012Q002B00012Q0079000200024Q00EF3Q00017Q00033Q0003063Q0069706169727303073Q0067657467656E7603123Q0053656C656374656454617267657450657473010F3Q001206000100013Q00122Q000200026Q00020001000200202Q0002000200034Q00010002000300044Q000A00010006880005000A00013Q000457012Q000A00012Q009F000600014Q0079000600023Q0006632Q01000600010002000457012Q000600012Q009F00016Q0079000100024Q00EF3Q00017Q00093Q0003073Q0067657467656E7603123Q0053656C65637465645461726765745065747303063Q0069706169727303053Q007461626C6503063Q0072656D6F76652Q033Q006D617803043Q004D617820030E3Q00207065747320726561636865642103063Q00696E7365727401263Q001283000100014Q0002000100010002002040000100010002001283000200034Q0055010300014Q00E1000200020004000457012Q001000010006880006001000013Q000457012Q00100001001283000700043Q0020880107000700054Q000800016Q000900056Q0007000900014Q000700016Q000700023Q0006630102000700010002000457012Q000700012Q007A000200014Q006001035Q0020400003000300060006740003001E00010002000457012Q001E00012Q009F00025Q001253010300074Q006001045Q002040000400040006001253010500084Q00310103000300052Q000D000200033Q001283000200043Q0020880102000200094Q000300016Q00048Q0002000400014Q000200016Q000200024Q00EF3Q00017Q00043Q0003053Q007461626C6503053Q00636C65617203073Q0067657467656E7603123Q0053656C65637465645461726765745065747300073Q00122A012Q00013Q00206Q000200122Q000100036Q00010001000200202Q0001000100046Q000200016Q00017Q00063Q0003073Q0067657467656E7603123Q0053656C656374656454617267657450657473028Q0003043Q004E6F6E65026Q00F03F2Q033Q00202B2000143Q0012833Q00014Q00023Q000100020020405Q00022Q007A00015Q00267C2Q01000800010003000457012Q000800010012532Q0100044Q0079000100024Q007A00015Q00267C2Q01000D00010005000457012Q000D000100204000013Q00052Q0079000100023Q00204000013Q0005001209010200066Q00035Q00202Q0003000300054Q0001000100034Q000100028Q00017Q00033Q0003073Q0067657467656E7603143Q004D65726368616E744D6178507269636554657874034Q0001073Q001283000100014Q000200010001000200068B0102000500013Q000457012Q00050001001253010200033Q00102C2Q01000200022Q00EF3Q00017Q00063Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01173Q0012FE000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D00010001000457012Q000D00012Q00EF3Q00013Q001283000200053Q00204000020002000600062F01033Q000100052Q0055012Q00014Q0060017Q0060012Q00014Q0060012Q00024Q0060012Q00034Q00A80102000200012Q00EF3Q00013Q00013Q00283Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657203063Q0069706169727303123Q0053656C65637465645461726765745065747303063Q00737472696E6703053Q00752Q7065722Q0103043Q006E65787403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q66657273030A3Q007061727365507269636503143Q004D65726368616E744D617850726963655465787403053Q00546965723103053Q00546965723203053Q0054696572332Q033Q0050657403073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03073Q0056697369626C6503043Q0054657874034Q002Q033Q0049734103093Q00546578744C6162656C03023Q007830028Q0003053Q007063612Q6C0003043Q007461736B03043Q0077616974026Q33D33F00BF4Q0008017Q006E2Q0100013Q001283000200014Q0002000200010002002040000200020002000638010200BE00013Q000457012Q00BE00012Q006001026Q0060010300013Q002040000300030003000688000200BE00010003000457012Q00BE00010006460001001E00010001000457012Q001E00012Q0060010200023Q002012010200020004001253010400054Q004F0002000400020006380102001800013Q000457012Q00180001002012010300020004001253010500064Q004F0003000500022Q0055010200033Q0006FA0001001E00010002000457012Q001E0001002012010300020004001253010500074Q004F0003000500022Q00552Q0100034Q000801025Q001206000300083Q00122Q000400016Q00040001000200202Q0004000400094Q00030002000500044Q002C00010012830008000A3Q00202600080008000B4Q000900036Q000A00076Q0009000A6Q00083Q000200202Q00020008000C0006630103002500010002000457012Q002500010006382Q0100B900013Q000457012Q00B900010012830003000D4Q0055010400024Q0020000300020002000638010300B900013Q000457012Q00B900012Q0060010300043Q0020120103000300040012530105000E4Q004F0003000500020006FA0004005300010003000457012Q005300010020120104000300040012530106000F4Q004F0004000600020006380104005300013Q000457012Q0053000100204000040003000F002012010400040004001253010600104Q004F0004000600020006380104005300013Q000457012Q0053000100204000040003000F00200A01040004001000202Q00040004000400122Q000600116Q00040006000200062Q0004005300013Q000457012Q0053000100204000040003000F00205801040004001000202Q00040004001100202Q00040004000400122Q000600126Q000400060002000638010400B700013Q000457012Q00B700012Q0060010500013Q002040000500050013001283000600014Q00020006000100020020400006000600142Q002000050002000200123F010600086Q000700033Q00122Q000800153Q00122Q000900163Q00122Q000A00176Q0007000300012Q00E1000600020008000457012Q00B40001002012010B000400042Q0055010D000A4Q004F000B000D00022Q009F000C5Q000638010B00B000013Q000457012Q00B00001002012010D000B000400128A000F00186Q000D000F000200202Q000E000B000400122Q001000196Q000E0010000200202Q000F000B000400122Q0011001A6Q000F0011000200202Q0010000B000400122Q0012001B6Q00100012000200062Q0011007A00010010000457012Q007A00010020120111001000040012530113001C4Q004F0011001300020006FA0012007D0001000E000457012Q007D00010020400012000E001D000638010F008200013Q000457012Q008200010020400013000F001E0006460013008300010001000457012Q008300010012530113001F3Q000638010D00B000013Q000457012Q00B000010020120114000D0020001253011600214Q004F001400160002000638011400B000013Q000457012Q00B00001000646001200B000010001000457012Q00B000010026B5001300B000010022000457012Q00B000010012830014000A3Q00204000140014000B2Q0060011500033Q0020400016000D001E2Q00B2001500164Q009B00143Q00022Q008D001400020014000638011400B000013Q000457012Q00B000010006380111009F00013Q000457012Q009F00012Q0060011400013Q00204000140014001300204000150011001E2Q0020001400020002000646001400A000010001000457012Q00A00001001253011400233Q0026DA000500A400010023000457012Q00A40001000674001400B000010005000457012Q00B000012Q009F000C00014Q008D00153Q000A000646001500B000010001000457012Q00B00001001283001500243Q00062F01163Q000100022Q0055012Q00014Q0055012Q000A4Q0020001500020002000638011500B000013Q000457012Q00B00001002027012Q000A000C000646000C00B300010001000457012Q00B30001002027012Q000A00252Q008400095Q0006630106006300010002000457012Q00630001000457012Q00B900012Q000801056Q0055012Q00053Q001283000300263Q002040000300030027001253010400284Q00A8010300020001000457012Q000200012Q00EF3Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q005F7Q00206Q00014Q000200018Q000200016Q00017Q00053Q0003083Q00496E7374616E63652Q033Q006E657703053Q00706169727303063Q00506172656E7403063Q00697061697273031D3Q0012BA000300013Q00202Q0003000300024Q00048Q00030002000200122Q000400036Q000500016Q00040002000600044Q000B00010026B50007000B00010004000457012Q000B00012Q00550003000700080006630104000800010002000457012Q000800010006380102001600013Q000457012Q00160001001283000400054Q0055010500024Q00E1000400020006000457012Q0014000100102C0108000400030006630104001300010002000457012Q001300010020400004000100040006380104001B00013Q000457012Q001B000100204000040001000400102C0103000400042Q0079000300024Q00EF3Q00017Q00073Q0003083Q005549436F726E6572030C3Q00436F726E657252616469757303043Q005544696D2Q033Q006E6577028Q00026Q00144003063Q00506172656E74020F4Q007201025Q00122Q000300016Q00043Q000200122Q000500033Q00202Q00050005000400122Q000600053Q00062Q0007000900010001000457012Q00090001001253010700064Q004F00050007000200102C01040002000500102C010400074Q0045000200044Q004700026Q00EF3Q00017Q00083Q0003083Q0055495374726F6B6503053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q00E03F03093Q00546869636B6E652Q73026Q00F03F03063Q00506172656E7404144Q006001045Q001253010500014Q000801063Q000400068B0107000700010001000457012Q000700012Q0060010700013Q00204000070007000300102C01060002000700068B0107000B00010002000457012Q000B0001001253010700053Q00102C01060004000700068B0107000F00010003000457012Q000F0001001253010700073Q00102C01060006000700102C010600084Q0045000400064Q004700046Q00EF3Q00017Q00013Q0003053Q007063612Q6C030E3Q001283000300013Q00062F01043Q000100042Q0060017Q0055012Q00014Q0055012Q00024Q0055017Q00E10003000200040006380103000B00013Q000457012Q000B000100068B0105000C00010004000457012Q000C00012Q006E010500054Q0079000500024Q00EF3Q00013Q00013Q00093Q0003083Q005549536861646F77030A3Q00426C757252616469757303043Q005544696D2Q033Q006E6577028Q00026Q002840030C3Q005472616E73706172656E6379026Q00E03F03063Q00506172656E7400164Q0060016Q0012532Q0100014Q000801023Q0003001283000300033Q002040000300030004001253010400054Q0060010500013Q0006460005000A00010001000457012Q000A0001001253010500064Q004F00030005000200102C0102000200032Q0060010300023Q0006460003001000010001000457012Q00100001001253010300083Q00102C0102000700032Q00FC000300033Q00102Q0002000900036Q00029Q008Q00017Q00023Q0003063Q0043726561746503043Q00506C6179030C4Q006001035Q0020120103000300012Q005501055Q00068B0106000600010001000457012Q000600012Q0060010600014Q0055010700024Q005400030007000200202Q0004000300024Q0004000200014Q000300028Q00017Q00133Q0003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0054657874034Q0003083Q00466F6E744661636503043Q00466F6E742Q033Q006E657703043Q00456E756D030A3Q00466F6E7457656967687403043Q00426F6C6403073Q00526567756C6172030A3Q0054657874436F6C6F7233030A3Q00546578745363616C65642Q0103043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574026Q002C40042A4Q00F700045Q00122Q000500016Q00063Q000600302Q00060002000300062Q0007000700013Q000457012Q00070001001253010700053Q00102C010600040007001283000700073Q0020400007000700082Q0060010800013Q0006380102001200013Q000457012Q00120001001283000900093Q00204000090009000A00204000090009000B0006460009001500010001000457012Q00150001001283000900093Q00204000090009000A00204000090009000C2Q004F00070009000200102C01060006000700068B0107001B00010003000457012Q001B00012Q0060010700023Q00204000070007000400102C0106000D00070030270006000E000F001283000700113Q00204000070007001200068B0108002200010001000457012Q00220001001253010800133Q00068B0109002500010001000457012Q00250001001253010900134Q004F00070009000200102C0106001000072Q0045000400064Q004700046Q00EF3Q00017Q00153Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q00284003083Q00506F736974696F6E03063Q00506172656E74026Q0028C003053Q007461626C6503063Q00696E7365727403063Q00697061697273030A3Q00496E707574426567616E03073Q00436F2Q6E656374030C3Q00496E7075744368616E67656402953Q00068B0106000300010001000457012Q000300012Q000801066Q006001075Q001272000800016Q00093Q000600302Q00090002000300302Q00090004000500302Q00090006000700122Q000A00093Q00202Q000A000A000A00122Q000B00073Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000C6Q000A000E000200102Q00090008000A00122Q000A00093Q00202Q000A000A000A00122Q000B000B3Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000B6Q000A000E000200102Q0009000D000A00102Q0009000E6Q0007000900024Q00085Q00122Q000900016Q000A3Q000600302Q000A0002000300302Q000A0004000500302Q000A0006000700122Q000B00093Q00202Q000B000B000A00122Q000C00073Q00122Q000D000B3Q00122Q000E000B3Q00122Q000F000C6Q000B000F000200102Q000A0008000B00122Q000B00093Q00202Q000B000B000A00122Q000C000B3Q00122Q000D000B3Q00122Q000E00073Q00122Q000F000F6Q000B000F000200102Q000A000D000B00102Q000A000E6Q0008000A00024Q00095Q00122Q000A00016Q000B3Q000600302Q000B0002000300302Q000B0004000500302Q000B0006000700122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000C3Q00122Q000F00073Q00122Q0010000B6Q000C0010000200102Q000B0008000C00122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000B3Q00122Q000F000B3Q00122Q0010000B6Q000C0010000200102Q000B000D000C00102Q000B000E6Q0009000B00024Q000A5Q00122Q000B00016Q000C3Q000600302Q000C0002000300302Q000C0004000500302Q000C0006000700122Q000D00093Q00202Q000D000D000A00122Q000E000B3Q001253010F000C3Q0012AA001000073Q00122Q0011000B6Q000D0011000200102Q000C0008000D00122Q000D00093Q002064010D000D000A00122Q000E00073Q00122Q000F000F3Q00122Q0010000B3Q00122Q0011000B6Q000D00110002001048010C000D000D00102Q000C000E6Q000A000C000200122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00076Q000B000D000100122Q000B00103Q00202Q000B000B00112Q0055010C00064Q004C000D00086Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00066Q000D00096Q000B000D000100122Q000B00103Q00202Q000B000B00114Q000C00064Q0055010D000A4Q00C7000B000D000100122Q000B00126Q000C00066Q000B0002000D00044Q008800010020400010000F001300201201100010001400062F01123Q000100042Q0055012Q00024Q0055012Q00044Q0055012Q00054Q0055017Q002B0110001200010020400010000F001500201201100010001400062F01120001000100012Q0055012Q00034Q002B011000120001000663010B007B00010002000457012Q007B00012Q0060010B00014Q0060010C00023Q002040000C000C001500062F010D0002000100052Q0055012Q00034Q0055012Q00024Q0055012Q00044Q0055017Q0055012Q00054Q002B010B000D00012Q00EF3Q00013Q00033Q00073Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636803083Q00506F736974696F6E03073Q004368616E67656403073Q00436F2Q6E656374011A3Q00206A2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000457012Q000C000100204000013Q0001001283000200023Q0020400002000200010020400002000200040006880001001900010002000457012Q001900012Q009F000100014Q001700015Q00202Q00013Q00054Q000100016Q000100033Q00202Q0001000100054Q000100023Q00202Q00013Q000600202Q00010001000700062F01033Q000100022Q0055017Q0060017Q002B2Q01000300012Q00EF3Q00013Q00013Q00033Q00030E3Q0055736572496E707574537461746503043Q00456E756D2Q033Q00456E64000A4Q006D016Q00206Q000100122Q000100023Q00202Q00010001000100202Q00010001000300064Q000900010001000457012Q000900012Q009F8Q00CF3Q00014Q00EF3Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F756368010E3Q00206A2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000457012Q000C000100204000013Q0001001283000200023Q0020400002000200010020400002000200040006880001000D00010002000457012Q000D00012Q00CF8Q00EF3Q00017Q00073Q0003083Q00506F736974696F6E03053Q005544696D322Q033Q006E657703013Q005803053Q005363616C6503063Q004F2Q6673657403013Q0059011F4Q00602Q015Q0006883Q001E00010001000457012Q001E00012Q00602Q0100013Q0006382Q01001E00013Q000457012Q001E000100204000013Q00012Q005F010200026Q0001000100024Q000200033Q00122Q000300023Q00202Q0003000300034Q000400043Q00202Q00040004000400202Q0004000400054Q000500043Q00202Q00050005000400202Q00050005000600202Q0006000100044Q0005000500064Q000600043Q00202Q00060006000700202Q0006000600054Q000700043Q00202Q00070007000700202Q00070007000600202Q0008000100074Q0007000700084Q00030007000200102Q0002000100032Q00EF3Q00017Q00043Q00030A3Q00496E707574426567616E03073Q00436F2Q6E656374030A3Q00496E707574456E646564030C3Q00496E7075744368616E67656402184Q009F00025Q00062F01033Q000100022Q0055017Q0055012Q00013Q00204000043Q000100201201040004000200062F01060001000100022Q0055012Q00024Q0055012Q00034Q002B0104000600012Q006001046Q0060010500013Q00204000050005000300062F01060002000100012Q0055012Q00024Q002B0104000600012Q006001046Q0060010500013Q00204000050005000400062F01060003000100022Q0055012Q00024Q0055012Q00034Q002B0104000600012Q00EF3Q00013Q00043Q00093Q0003103Q004162736F6C757465506F736974696F6E030C3Q004162736F6C75746553697A6503043Q006D61746803053Q00636C616D7003083Q00506F736974696F6E03013Q0058028Q00026Q00F03F03013Q0059011F4Q00E500015Q00202Q0001000100014Q00025Q00202Q00020002000200122Q000300033Q00202Q00030003000400202Q00043Q000500202Q00040004000600202Q0005000100064Q0004000400050020400005000200062Q001D01040004000500122Q000500073Q00122Q000600086Q00030006000200122Q000400033Q00202Q00040004000400202Q00053Q000500202Q00050005000900202Q0006000100094Q0005000500060020400006000200092Q005401050005000600122Q000600073Q00122Q000700086Q0004000700024Q000500016Q000600036Q000700046Q0005000700016Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636801123Q00206A2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000457012Q000C000100204000013Q0001001283000200023Q0020400002000200010020400002000200040006880001001100010002000457012Q001100012Q009F000100014Q00CF00016Q00602Q0100014Q005501026Q00A82Q01000200012Q00EF3Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F756368010F3Q00206A2Q013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000457012Q000C000100204000013Q0001001283000200023Q0020400002000200010020400002000200040006880001000E00010002000457012Q000E00012Q009F00016Q00CF00016Q00EF3Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F75636801134Q00602Q015Q0006382Q01001200013Q000457012Q0012000100204000013Q0001001283000200023Q0020400002000200010020400002000200030006F20001000F00010002000457012Q000F000100204000013Q0001001283000200023Q0020400002000200010020400002000200040006880001001200010002000457012Q001200012Q00602Q0100014Q005501026Q00A82Q01000200012Q00EF3Q00017Q00093Q0003063Q0067657468756903053Q007063612Q6C030B3Q004C6F63616C506C61796572030C3Q0057616974466F724368696C6403093Q00506C61796572477569026Q00144003043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900223Q0012833Q00013Q000638012Q000B00013Q000457012Q000B00010012833Q00023Q001283000100014Q00E13Q00020001000638012Q000B00013Q000457012Q000B00010006382Q01000B00013Q000457012Q000B00012Q0079000100023Q0012833Q00023Q0002C300016Q00E13Q00020001000638012Q001300013Q000457012Q001300010006382Q01001300013Q000457012Q001300012Q0079000100024Q006001025Q0020400002000200030006380102001C00013Q000457012Q001C0001002012010300020004001253010500053Q001253010600064Q0045000300064Q004700035Q001283000300073Q00200100030003000800122Q000500096Q000300056Q00039Q0000013Q00013Q00043Q0003083Q00636C6F6E6572656603043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900113Q0012833Q00013Q000638012Q000B00013Q000457012Q000B00010012833Q00013Q00124D000100023Q00202Q00010001000300122Q000300046Q000100039Q00000200064Q000F00010001000457012Q000F00010012833Q00023Q002012014Q0003001253010200044Q004F3Q000200022Q00793Q00024Q00EF3Q00017Q00023Q002Q033Q0073796E030B3Q0070726F746563745F677569000C3Q0012833Q00013Q000638012Q000B00013Q000457012Q000B00010012833Q00013Q0020405Q0002000638012Q000B00013Q000457012Q000B00010012833Q00013Q0020405Q00022Q00602Q016Q00A8012Q000200012Q00EF3Q00017Q003E3Q0003053Q00416C697665010003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C65030B3Q004175746F47656D57616C6B030F3Q004A756D70506F776572546F2Q676C6503083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E6703093Q0044697361626C65334403063Q0069706169727303083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q6703103Q004175746F486174636843756265452Q6703063Q004E6F636C6970030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030C3Q00496E66696E6974654A756D70030E3Q004D65726368616E74536E69706572030B3Q0057616C6B546F437562657303073Q00456E61626C656403073Q0073652Q73696F6E026Q00F03F03053Q007063612Q6C03053Q007461626C6503053Q00636C65617203093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403093Q004A756D70506F776572026Q004940030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F72656403093Q00776F726B7370616365030A3Q00426F2Q734D6F64656C73030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q004B657953797374656D426C757203073Q0044657374726F7903133Q0056656C6F63697479487562496E7374616E63650001B24Q00602Q015Q0020400001000100010006460001000500010001000457012Q000500012Q00EF3Q00014Q00602Q015Q00307B2Q010001000200122Q000100036Q00010001000200202Q00020001000400062Q0002000D00010001000457012Q000D00010020400002000100050020400003000100060020400004000100070006460004001200010001000457012Q0012000100204000040001000800204000050001000900209C01060001000A00122Q0007000B6Q000800183Q00122Q0009000C3Q00122Q000A000D3Q00122Q000B000E3Q00122Q000C000F3Q00122Q000D00103Q00122Q000E00113Q00122Q000F00123Q00122Q001000133Q00122Q001100143Q00122Q001200153Q00122Q001300053Q00122Q001400073Q00122Q001500083Q00122Q001600093Q00122Q001700163Q00122Q001800173Q00122Q001900183Q00122Q001A00193Q00122Q001B001A3Q00122Q001C001B3Q00122Q001D001C3Q00122Q001E001D3Q00122Q001F001E3Q00122Q0020000A3Q00122Q0021001F3Q00122Q002200203Q00122Q002300043Q00122Q002400063Q00122Q002500213Q00122Q002600223Q00122Q002700236Q0008001F00012Q00E1000700020009000457012Q003900010020272Q01000B00020006630107003800010002000457012Q003800012Q0060010700013Q0030680107002400024Q000700026Q000800023Q00202Q00080008002500202Q00080008002600102Q0007002500084Q000700036Q000800033Q00202Q00080008002500202Q00080008002600102Q0007002500084Q000700046Q000800043Q00202Q00080008002500202Q00080008002600102Q0007002500084Q000700056Q00070001000100122Q0007000B6Q000800066Q00070002000900044Q00570001001283000C00273Q00062F010D3Q000100012Q0055012Q000B4Q00A8010C000200012Q0084000A5Q0006630107005200010002000457012Q00520001001283000700283Q00209E0007000700294Q000800066Q0007000200014Q000700073Q00202Q00070007002A00062Q0008006400010007000457012Q0064000100201201080007002B001253010A002C4Q004F0008000A00020006380108007100013Q000457012Q007100010006380102006E00013Q000457012Q006E000100201201090008002D001283000B002E3Q002040000B000B002F2Q002B0109000B00012Q0060010900083Q00102C0108003000090006380103007100013Q000457012Q007100010030270008003100320006380104007B00013Q000457012Q007B00010006FA0009007800010007000457012Q00780001002012010900070033001253010B00344Q004F0009000B00020006380109007B00013Q000457012Q007B00010030270009003500020006380105009500013Q000457012Q00950001001283000900363Q002012010900090033001253010B00374Q004F0009000B00020006380109009500013Q000457012Q00950001001283000A000B3Q002012010B000900382Q00B2000B000C4Q0028000A3Q000C000457012Q00930001002012010F000E00390012530111003A4Q004F000F00110002000638010F009000013Q000457012Q00900001002012010F000E0033001253011100344Q004F000F00110002000638010F009300013Q000457012Q00930001003027000F00350002000663010A008800010002000457012Q008800010006380106009B00013Q000457012Q009B0001001283000900273Q00062F010A0001000100012Q0060012Q00094Q00A80109000200012Q00600109000A3Q002012010900090033001253010B003B4Q004F0009000B0002000638010900A300013Q000457012Q00A30001002012010A0009003C2Q00A8010A00020001001283000A00034Q0002000A00010002002040000A000A003D2Q0060010B5Q000688000A00AC0001000B000457012Q00AC0001001283000A00034Q0002000A00010002003027000A003D003E0006463Q00B100010001000457012Q00B100012Q0060010A000B3Q002012010A000A003C2Q00A8010A000200012Q00EF3Q00013Q00023Q00013Q00030A3Q00446973636F2Q6E65637400044Q0060016Q002012014Q00012Q00A8012Q000200012Q00EF3Q00017Q00013Q0003153Q00536574336452656E646572696E67456E61626C656400054Q00607Q00206Q00014Q000200018Q000200016Q00019Q003Q00044Q0060017Q009F00016Q00A8012Q000200012Q00EF3Q00019Q003Q00044Q0060017Q009F000100014Q00A8012Q000200012Q00EF3Q00017Q003C3Q0003083Q00496E7374616E63652Q033Q006E6577030A3Q00426C7572452Q6665637403043Q004E616D65030D3Q004B657953797374656D426C757203043Q0053697A65026Q00344003063Q00506172656E74028Q0003053Q004672616D65030E3Q004B657953797374656D4672616D65030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E03053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240025Q00C0624003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03063Q004163746976652Q01026Q00184003063Q005374726F6B65026Q00304003093Q00546578744C6162656C03043Q005465787403173Q004B65792053797374656D20566572696669636174696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40026Q002840026Q00F03F03093Q005365636F6E64617279025Q00804640026Q006E40026Q003E40026Q00104003073Q0054657874426F78034Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q005375625465787403103Q00436C656172546578744F6E466F6375730100026Q0024C0026Q001440030A3Q005465787442752Q746F6E030A3Q005375626D6974204B657903063Q00412Q63656E74025Q00805640030F3Q004175746F42752Q746F6E436F6C6F7203093Q0041637469766174656403073Q00436F2Q6E65637402E23Q001274010200013Q00202Q00020002000200122Q000300036Q00020002000200302Q00020004000500302Q0002000600074Q00035Q00102Q00020008000300122Q000300096Q000400013Q00122Q0005000A6Q00063Q000900302Q00060004000B00122Q0007000D3Q00202Q00070007000200122Q0008000E3Q00122Q0009000E6Q00070009000200102Q0006000C000700122Q000700103Q00202Q00070007000200122Q0008000E3Q00122Q000900093Q00122Q000A000E3Q00122Q000B00096Q0007000B000200102Q0006000F000700122Q000700103Q00202Q00070007001100122Q000800123Q00122Q000900136Q00070009000200102Q0006000600074Q000700023Q00202Q00070007001500102Q00060014000700302Q00060016001700302Q00060018000900302Q00060019001A4Q000700033Q00102Q0006000800074Q0004000600024Q000500046Q000600043Q00122Q0007001B6Q0005000700014Q000500056Q000600046Q000700023Q00202Q00070007001C00122Q0008000E6Q0005000800014Q000500066Q000600043Q00122Q0007001D3Q00122Q0008000E6Q0005000800014Q000500013Q00122Q0006001E6Q00073Q000800302Q0007001F00204Q000800073Q00102Q0007002100084Q000800023Q00202Q00080008001F00102Q00070022000800302Q00070023002400122Q000800103Q00202Q00080008000200122Q000900093Q00122Q000A00093Q00122Q000B00093Q00122Q000C00256Q0008000C000200102Q0007000F000800122Q000800103Q00202Q00080008000200122Q000900263Q00122Q000A00093Q00122Q000B00093Q001253010C00074Q00870008000C000200102Q00070006000800302Q00070016002600102Q0007000800044Q0005000700024Q000600013Q00122Q0007000A6Q00083Q00064Q000900023Q00202Q00090009002700102Q00080014000900122Q0009000D3Q00202Q00090009000200122Q000A000E3Q00122Q000B00096Q0009000B000200102Q0008000C000900122Q000900103Q00202Q00090009000200122Q000A000E3Q00122Q000B00093Q00122Q000C00093Q00122Q000D00286Q0009000D000200102Q0008000F000900122Q000900103Q00202Q00090009000200122Q000A00093Q00122Q000B00293Q00122Q000C00093Q00122Q000D002A6Q0009000D000200102Q00080006000900302Q00080018000900102Q0008000800044Q0006000800024Q000700046Q000800063Q00122Q0009002B6Q0007000900014Q000700056Q000800066Q000900023Q00202Q00090009001C4Q000A00086Q0007000A00024Q000800013Q00122Q0009002C6Q000A3Q000B00302Q000A0016002600302Q000A001F002D00302Q000A002E002F4Q000B00023Q00202Q000B000B003100102Q000A0030000B4Q000B00093Q00102Q000A0021000B4Q000B00023Q00202Q000B000B001F00102Q000A0022000B00302Q000A0023002500302Q000A0032003300122Q000B00103Q00202Q000B000B000200122Q000C00263Q00122Q000D00343Q00122Q000E00263Q00122Q000F00096Q000B000F000200102Q000A0006000B00122Q000B00103Q00202Q000B000B000200122Q000C00093Q00122Q000D00353Q00122Q000E00093Q00122Q000F00096Q000B000F000200102Q000A000F000B00102Q000A000800064Q0008000A00022Q0060010900013Q00128D010A00366Q000B3Q000A00302Q000B001F00374Q000C00073Q00102Q000B0021000C4Q000C00023Q00202Q000C000C001F00102Q000B0022000C00302Q000B002300254Q000C00023Q00202Q000C000C003800102Q000B0014000C00122Q000C000D3Q00202Q000C000C000200122Q000D000E3Q00122Q000E00096Q000C000E000200102Q000B000C000C00122Q000C00103Q00202Q000C000C000200122Q000D000E3Q00122Q000E00093Q00122Q000F00093Q00122Q001000396Q000C0010000200102Q000B000F000C00122Q000C00103Q00202Q000C000C000200122Q000D00093Q00122Q000E00293Q00122Q000F00093Q00122Q0010002A6Q000C0010000200102Q000B0006000C00302Q000B003A003300102Q000B000800044Q0009000B00024Q000A00046Q000B00093Q00122Q000C002B6Q000A000C000100062F010A3Q0001000B2Q0055012Q00084Q0055017Q0055012Q00024Q0060012Q000A4Q0055012Q00044Q0060012Q000B4Q0055012Q00014Q0055012Q00034Q0060012Q000C4Q0055012Q00074Q0060012Q00023Q002089010B0009003B00202Q000B000B003C4Q000D000A6Q000B000D00014Q000B000D6Q000C00046Q000D00016Q000E00056Q000D000100012Q002B010B000D00012Q00EF3Q00013Q00013Q00173Q0003043Q005465787403073Q0044657374726F7903043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240028Q0003043Q007461736B03043Q0077616974029A5Q99C93F026Q00F03F026Q00084003043Q004B69636B030B3Q00496E76616C6964204B6579034Q00030F3Q00506C616365686F6C6465725465787403063Q00737472696E6703063Q00666F726D617403133Q00496E76616C6964204B657921202825642F332903053Q00436F6C6F7203073Q005761726E696E6703053Q0064656C6179026Q00F83F004A4Q0060016Q0020405Q00012Q00602Q0100013Q0006883Q001D00010001000457012Q001D00012Q0060012Q00023Q00200E5Q00026Q000200016Q00036Q000100046Q000200056Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600074Q004F00040006000200102C0103000300042Q002B012Q000300010012833Q00083Q0020405Q00090012532Q01000A4Q00A8012Q000200012Q0060012Q00043Q002012014Q00022Q00A8012Q000200012Q0060012Q00064Q009A012Q00010001000457012Q004900012Q0060012Q00073Q00209D5Q000B2Q00CF3Q00074Q0060012Q00073Q000EC8000C002E00013Q000457012Q002E00012Q0060012Q00023Q000638012Q002900013Q000457012Q002900012Q0060012Q00023Q002012014Q00022Q00A8012Q000200012Q0060012Q00083Q002012014Q000D0012530102000E4Q002B012Q000200012Q00EF3Q00014Q0060016Q003020012Q0001000F9Q0000122Q000100113Q00202Q00010001001200122Q000200136Q000300076Q00010003000200104Q001000016Q00036Q000100096Q000200056Q00033Q00014Q0004000A3Q00202Q00040004001500102Q0003001400046Q0003000100124Q00083Q00206Q001600122Q000100173Q00062F01023Q000100052Q0060017Q0060012Q00034Q0060012Q00094Q0060012Q00054Q0060012Q000A4Q002B012Q000200012Q00EF3Q00013Q00013Q00043Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03053Q00436F6C6F7203063Q005374726F6B65000B4Q0060016Q0030273Q000100022Q0060012Q00014Q00A22Q0100026Q000200036Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300012Q00EF3Q00017Q00613Q0003063Q00412Q63656E7403043Q005461627303083Q005F63752Q72656E740003053Q004672616D6503043Q004E616D6503063Q0057696E646F77030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407A40026Q00744003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03103Q00436C69707344657363656E64616E74732Q0103063Q0041637469766503063Q00506172656E74026Q00184003063Q005374726F6B65026Q00304003063Q00546F7042617203093Q005365636F6E64617279026Q00F03F026Q00414003063Q005A496E646578026Q001440026Q0020400285EB51B81E85EB3F03043Q006D2Q6F6E026Q00324003063Q00507572706C6503093Q00546578744C6162656C03053Q005469746C6503043Q0054657874030C3Q0056656C6F636974792048756203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q002Q40026Q0059C0026Q00344003013Q0078026Q0020C003063Q00436F6C6F723303073Q0066726F6D524742025Q00E06F40025Q0080564003053Q006D696E7573026Q0041C003083Q006B6579626F617264026Q004EC003093Q0041637469766174656403073Q00436F2Q6E65637403043Q00426F6479030E3Q005363726F2Q6C696E674672616D6503073Q005461624C697374029A5Q99C93F025Q00C05C40030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q7303103Q005363726F2Q6C696E67456E61626C6564030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D026Q00084003093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E67526967687403073Q00436F6E74656E74026Q005D40026Q005DC0030B3Q005369646544697669646572027Q0040030A3Q00496E707574426567616E03093Q00437265617465546162029C012Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100023Q0020400002000100010006380102000A00013Q000457012Q000A00012Q006001025Q00204000030001000100102C0102000100032Q000801023Q00022Q000801035Q00102C0102000200030030270002000300042Q0060010300013Q001253010400054Q000801053Q000A003027000500060007001213010600093Q00202Q00060006000A00122Q0007000B3Q00122Q0008000B6Q00060008000200102Q00050008000600122Q0006000D3Q00206401060006000A00122Q0007000B3Q00122Q0008000E3Q00122Q0009000B3Q00122Q000A000E6Q0006000A00020010F40005000C000600122Q0006000D3Q00202Q00060006001000122Q000700113Q00122Q000800126Q00060008000200102Q0005000F00064Q00065Q00202Q00060006001400102Q00050013000600302700050015001600302700050017000E0030270005001800190030270005001A00192Q0060010600023Q00102C0105001B00062Q004F0003000500022Q0060010400034Q0055010500033Q0012530106001C4Q002B0104000600012Q0060010400044Q00E6000500036Q00065Q00202Q00060006001D00122Q0007000B6Q0004000700014Q000400056Q000500033Q00122Q0006001E3Q00122Q0007000B6Q0004000700012Q0060010400013Q001282000500056Q00063Q000800302Q00060006001F4Q00075Q00202Q00070007002000102Q00060013000700302Q00060015001600122Q0007000D3Q00206401070007000A00122Q000800213Q00122Q0009000E3Q00122Q000A000E3Q00122Q000B00226Q0007000B00020010770006000F000700302Q00060017000E00302Q00060023002400302Q0006001A001900102Q0006001B00034Q0004000600024Q000500046Q000600046Q00075Q00202Q00070007001D2Q0060010800064Q002B0105000800012Q0060010500054Q0055010600043Q001253010700253Q001253010800264Q002B0105000800012Q0060010500073Q001253010600273Q001253010700284Q009F000800014Q006001095Q0020400009000900292Q004F000500090002001268000600093Q00202Q00060006000A00122Q0007000E3Q00122Q0008000B6Q00060008000200102Q00050008000600122Q0006000D3Q00202Q00060006000A00122Q0007000E3Q00122Q000800253Q00122Q0009000B3Q00122Q000A000E6Q0006000A000200102Q0005000C000600102Q0005001B00044Q000600013Q00122Q0007002A6Q00083Q000C00302Q00080006002B00202Q00090001002B00062Q0009007D00010001000457012Q007D00010012530109002D3Q00102C0108002C00092Q00C4000900083Q00102Q0008002E00094Q00095Q00202Q00090009002C00102Q0008002F000900302Q00080030003100122Q000900333Q00202Q00090009003200202Q00090009003400102Q000800320009001283000900333Q00204000090009003500204000090009003600102C010800350009003027000800150021001213010900093Q00202Q00090009000A00122Q000A000E3Q00122Q000B000B6Q0009000B000200102Q00080008000900122Q0009000D3Q00206401090009000A00122Q000A000E3Q00122Q000B00373Q00122Q000C000B3Q00122Q000D000E6Q0009000D000200102C0108000C00090012830009000D3Q00206401090009000A00122Q000A00213Q00122Q000B00383Q00122Q000C000E3Q00122Q000D00396Q0009000D000200102C0108000F000900102C0108001B00042Q004F00060008000200062F01073Q000100072Q0060012Q00014Q0060017Q0055012Q00044Q0060012Q00034Q0060012Q00074Q0060012Q00094Q0060012Q000A4Q0076000800073Q00122Q0009003A3Q00122Q000A003B3Q00122Q000B003C3Q00202Q000B000B003D00122Q000C003E3Q00122Q000D003F3Q00122Q000E003F6Q000B000E6Q00083Q00022Q0055010900073Q001253010A00403Q001253010B00414Q0060010C5Q002040000C000C002C2Q004F0009000C00022Q0055010A00073Q001253010B00423Q001253010C00434Q0060010D5Q002040000D000D00292Q004F000A000D00022Q009F000B5Q002040000C000A0044002012010C000C004500062F010E00010001000D2Q0060012Q00024Q0055012Q000B4Q0060012Q00014Q0060017Q0060012Q00034Q0060012Q00044Q0060012Q00054Q0060012Q00084Q0060012Q000B4Q0060012Q000C4Q0060012Q00094Q0060012Q000A4Q0060012Q000D4Q002B010C000E00012Q0060010C00013Q001253010D00054Q0008010E3Q0005003027000E00060046003027000E00150021001283000F000D3Q002064010F000F000A00122Q0010000E3Q00122Q0011000E3Q00122Q0012000E3Q00122Q001300226Q000F0013000200102C010E000C000F001283000F000D3Q002064010F000F000A00122Q001000213Q00122Q0011000E3Q00122Q001200213Q00122Q001300416Q000F00130002001059010E000F000F00102Q000E001B00034Q000C000E00024Q000D00013Q00122Q000E00476Q000F3Q000B00302Q000F000600484Q00105Q00202Q00100010002000102Q000F00130010003027000F00150049003027000F0017000E0012830010000D3Q00206401100010000A00122Q0011000E3Q00122Q0012004A3Q00122Q001300213Q00122Q0014000E6Q001000140002001037010F000F001000122Q0010000D3Q00202Q00100010000A4Q00100001000200102Q000F004B001000122Q001000333Q00202Q00100010004D00202Q00100010004E00102Q000F004C001000302Q000F004F000E003027000F0050001900304E000F001A001900102Q000F001B000C4Q001000016Q001100013Q00122Q001200516Q00133Q000200122Q001400533Q00202Q00140014000A00122Q0015000E3Q00122Q001600544Q002B00140016000200102Q00130052001400122Q001400333Q00202Q00140014005500202Q00140014005600102Q0013005500144Q0011001300024Q001200013Q00122Q001300576Q00143Q0003001213011500533Q00202Q00150015000A00122Q0016000E3Q00122Q0017001C6Q00150017000200102Q00140058001500122Q001500533Q00203901150015000A00122Q0016000E3Q00122Q0017001C6Q00150017000200102Q00140059001500122Q001500533Q00204000150015000A0012530116000E3Q0012530117001C4Q004F00150017000200102C0114005A00152Q00D4001200144Q004601103Q00012Q004F000D001000022Q0098010E00013Q00122Q000F00056Q00103Q000500302Q00100006005B00302Q00100015002100122Q0011000D3Q00202Q00110011000A00122Q0012000E3Q00122Q0013005C3Q00122Q0014000E3Q00122Q0015000E6Q00110015000200102Q0010000C001100122Q0011000D3Q00202Q00110011000A00122Q001200213Q00122Q0013005D3Q00122Q001400213Q00122Q0015000E6Q00110015000200102Q0010000F001100102Q0010001B000C4Q000E001000024Q000F00013Q00122Q001000056Q00113Q000800302Q00110006005E4Q00125Q00202Q00120012001D00102Q0011001300124Q001200063Q00102Q00110015001200302Q00110017000E00122Q0012000D3Q00202Q00120012000A00122Q0013000E3Q00122Q0014004A3Q00122Q0015000E3Q00122Q0016000E6Q00120016000200102Q0011000C001200122Q0012000D3Q00202Q00120012000A00122Q0013000E3Q00122Q001400213Q00122Q001500213Q00122Q0016000E6Q00120016000200102Q0011000F001200302Q00110023005F00102Q0011001B000C4Q000F001100014Q000F000D6Q001000036Q001100036Q001200046Q001300066Q001400056Q0011000300012Q002B010F00110001002040000F00080044002012010F000F004500062F01110002000100052Q0060012Q000E4Q0060012Q00094Q0055012Q00034Q0060012Q000A4Q0060012Q00024Q002B010F001100012Q009F000F5Q00204000100009004400201201100010004500062F01120003000100052Q0055012Q000F4Q0055012Q000C4Q0060012Q00094Q0055012Q00034Q0060012Q000F4Q00ED0010001200014Q00108Q0011000B6Q0012000C3Q00202Q00120012006000062F01130004000100032Q0055012Q000B4Q0055012Q00104Q0055012Q00034Q002B01110013000100062F01110005000100102Q0060012Q00014Q0060017Q0055012Q000D4Q0060012Q00034Q0060012Q00074Q0060012Q00104Q0055012Q000E4Q0055012Q00024Q0060012Q00094Q0060012Q000F4Q0060012Q000A4Q0060012Q00044Q0060012Q00064Q0060012Q00084Q0060012Q00114Q0060012Q00123Q00102C0102006100112Q0079000200024Q00EF3Q00013Q00063Q001C3Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574026Q00364003063Q005A496E646578026Q00184003063Q00506172656E74026Q001440026Q002A4003073Q0053756254657874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503534Q005D00035Q00122Q000400016Q00053Q000900302Q00050002000300302Q0005000400054Q000600013Q00202Q00060006000700102Q00050006000600302Q00050008000900122Q0006000B3Q00202Q00060006000C00122Q000700093Q00122Q0008000D6Q00060008000200102Q0005000A000600122Q0006000F3Q00202Q00060006000C00122Q000700096Q000800013Q00122Q0009000D3Q00122Q000A00106Q0006000A000200102Q0005000E000600122Q0006000F3Q00202Q00060006001200122Q000700133Q00122Q000800136Q00060008000200102Q00050011000600302Q0005001400154Q000600023Q00102Q0005001600064Q0003000500024Q000400036Q000500033Q00122Q000600176Q0004000600014Q000400046Q00055Q00122Q000600186Q00078Q000800013Q00202Q0008000800194Q00040008000200122Q0005000B3Q00202Q00050005000C00122Q0006000D3Q00122Q0007000D6Q00050007000200102Q0004000A000500122Q0005000F3Q00202Q00050005000C00122Q0006000D3Q00122Q000700103Q00122Q0008000D3Q00122Q000900106Q00050009000200102Q0004000E000500302Q00040014001500102Q00040016000300202Q00050003001A00202Q00050005001B00062F01073Q000100062Q0060012Q00054Q0055012Q00034Q0060012Q00064Q0055012Q00044Q0055012Q00024Q0060012Q00014Q002B01050007000100204000050003001C00201201050005001B00062F01070001000100052Q0060012Q00054Q0055012Q00034Q0060012Q00064Q0055012Q00044Q0060012Q00014Q00660005000700014Q000500036Q000600046Q000500038Q00013Q00023Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379028Q00030A3Q0054657874436F6C6F723303043Q005465787400124Q00D39Q00000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00062Q0004000F00010001000457012Q000F00012Q0060010400053Q00204000040004000400102C0103000300042Q002B012Q000300012Q00EF3Q00017Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030A3Q0054657874436F6C6F723303073Q0053756254657874000F4Q0070019Q000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00463Q00030E3Q0046696E6446697273744368696C6403143Q004B657962696E645069636B65724F7665726C617903073Q0044657374726F7903053Q004672616D6503043Q004E616D65030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407040025Q00805B4003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E64030F3Q00426F7264657253697A65506978656C03063Q005A496E646578026Q00594003063Q004163746976652Q0103063Q00506172656E74026Q00204003063Q00507572706C65029A5Q99C93F026Q00F83F026Q003440029A5Q99D93F03093Q00546578744C6162656C03043Q005465787403173Q00437573746F6D204B657962696E642053652Q74696E677303083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40026Q002440026Q00F03F026Q00324003163Q004261636B67726F756E645472616E73706172656E6379025Q0040594003093Q005365636F6E64617279026Q004240025Q00806B40026Q003E40026Q001440030A3Q0043552Q52454E543A205B03063Q00737472696E6703053Q00752Q70657203073Q0067657467656E7603093Q00546F2Q676C654B657903123Q005D0A5052452Q5320414E59204B45593Q2E026Q002640030E3Q005465787458416C69676E6D656E7403043Q00456E756D03063Q0043656E746572025Q00805940030A3Q005465787442752Q746F6E03063Q0043616E63656C03073Q005375625465787403073Q00456C656D656E74025Q00805240026Q003640030F3Q004175746F42752Q746F6E436F6C6F720100026Q00104003093Q0041637469766174656403073Q00436F2Q6E656374030A3Q00496E707574426567616E00F14Q0099016Q00206Q000100122Q000200028Q0002000200064Q000B00013Q000457012Q000B00010020122Q013Q00032Q00A82Q01000200012Q009F00016Q00CF000100014Q00EF3Q00014Q009F000100014Q0049000100016Q000100023Q00122Q000200046Q00033Q000900302Q00030005000200122Q000400073Q00202Q00040004000800122Q000500093Q00122Q000600096Q00040006000200102C0103000600040012830004000B3Q00206401040004000800122Q000500093Q00122Q0006000C3Q00122Q000700093Q00122Q0008000C6Q0004000800020010F40003000A000400122Q0004000B3Q00202Q00040004000E00122Q0005000F3Q00122Q000600106Q00040006000200102Q0003000D00044Q000400033Q00202Q00040004001200102Q00030011000400302700030013000C0030270003001400150030270003001600172Q006001045Q00102C0103001800042Q004F0001000300022Q0060010200044Q0055010300013Q001253010400194Q002B0102000400012Q0060010200054Q0055010300014Q00D0000400033Q00202Q00040004001A00122Q0005001B3Q00122Q0006001C6Q0002000600014Q000200066Q000300013Q00122Q0004001D3Q00122Q0005001E6Q0002000500012Q0060010200023Q0012530103001F4Q000801043Q00090030270004002000212Q0060010500073Q00102C0104002200052Q0060010500033Q00204000050005002000102C0104002300050030270004002400250012830005000B3Q00206401050005000800122Q0006000C3Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900266Q00050009000200102C0104000A00050012830005000B3Q00206401050005000800122Q000600273Q00122Q0007000C3Q00122Q0008000C3Q00122Q000900286Q00050009000200102C0104000D000500302700040029002700302700040014002A00102C0104001800012Q004F0002000400022Q0060010300023Q001253010400044Q000801053Q00072Q0060010600033Q00204000060006002B00102C010500110006001213010600073Q00202Q00060006000800122Q000700093Q00122Q0008000C6Q00060008000200102Q00050006000600122Q0006000B3Q00206401060006000800122Q000700093Q00122Q0008000C3Q00122Q0009000C3Q00122Q000A002C6Q0006000A000200102C0105000A00060012830006000B3Q00206401060006000800122Q0007000C3Q00122Q0008002D3Q00122Q0009000C3Q00122Q000A002E6Q0006000A00020010470105000D000600302Q00050013000C00302Q00050014002A00102Q0005001800014Q0003000500024Q000400046Q000500033Q00122Q0006002F6Q0004000600014Q000400054Q0055010500034Q0014000600033Q00202Q00060006001A00122Q000700096Q0004000700024Q000500023Q00122Q0006001F6Q00073Q000900122Q000800303Q00122Q000900313Q00202Q000900090032001283000A00334Q006A000A0001000200202Q000A000A003400202Q000A000A00054Q00090002000200122Q000A00356Q00080008000A00102Q0007002000084Q000800073Q00102Q0007002200084Q000800033Q00204000080008001A00102C010700230008003027000700240036001283000800383Q00204000080008003700204000080008003900102C0107003700080012830008000B3Q00206401080008000800122Q000900273Q00122Q000A000C3Q00122Q000B00273Q00122Q000C000C6Q0008000C00020010080007000D000800302Q00070029002700302Q00070014003A00102Q0007001800034Q0005000700024Q000600023Q00122Q0007003B6Q00083Q000B00302Q00080020003C2Q00F3000900073Q00102Q0008002200094Q000900033Q00202Q00090009003D00102Q00080023000900302Q0008002400364Q000900033Q00202Q00090009003E00102Q00080011000900122Q000900073Q00202Q00090009000800122Q000A00093Q00122Q000B000C6Q0009000B000200102Q00080006000900122Q0009000B3Q00202Q00090009000800122Q000A00093Q00122Q000B000C3Q00122Q000C000C3Q00122Q000D003F6Q0009000D000200102Q0008000A000900122Q0009000B3Q00202Q00090009000800122Q000A000C3Q00122Q000B00153Q00122Q000C000C3Q00122Q000D00406Q0009000D000200102Q0008000D000900302Q00080041004200302Q00080014002A00102Q0008001800014Q0006000800024Q000700046Q000800063Q00122Q000900436Q0007000900014Q000700073Q00062F01083Q000100032Q0055012Q00074Q0055012Q00014Q0060012Q00013Q00209400090006004400202Q0009000900454Q000B00086Q0009000B00014Q000900086Q000A00093Q00202Q000A000A004600062F010B0001000100062Q0060012Q00014Q0055012Q00084Q0055012Q00054Q0060012Q000A4Q0055012Q00044Q0060012Q000B4Q00210009000B00024Q000700096Q0009000C6Q000A00016Q000B00016Q000C00026Q000B000100012Q002B0109000B00012Q00EF3Q00013Q00023Q00023Q00030A3Q00446973636F2Q6E65637403073Q0044657374726F7900114Q0060016Q000638012Q000800013Q000457012Q000800012Q0060016Q002012014Q00012Q00A8012Q000200012Q006E017Q00CF8Q0060012Q00013Q000638012Q000E00013Q000457012Q000E00012Q0060012Q00013Q002012014Q00022Q00A8012Q000200012Q009F8Q00CF3Q00024Q00EF3Q00017Q00163Q00030D3Q0055736572496E7075745479706503043Q00456E756D03083Q004B6579626F61726403073Q0067657467656E7603093Q00546F2Q676C654B657903073Q004B6579436F646503043Q0054657874030E3Q004E4557204B455942494E443A205B03063Q00737472696E6703053Q00752Q70657203043Q004E616D6503013Q005D030A3Q0054657874436F6C6F723303063Q00436F6C6F723303073Q0066726F6D524742026Q005E40025Q00E06F40026Q00644003053Q00436F6C6F7203043Q007461736B03043Q0077616974026Q00E03F02354Q006001025Q0006460002000600010001000457012Q000600012Q0060010200014Q009A0102000100012Q00EF3Q00013Q00204000023Q0001001283000300023Q0020400003000300010020400003000300030006880002003400010003000457012Q00340001001283000200044Q000200020001000200204000033Q000600102C0102000500032Q0060010200023Q001253010300083Q001283000400093Q00204000040004000A00204000053Q000600204000050005000B2Q00200004000200020012530105000C4Q003101030003000500102C0102000700032Q0060010200023Q0012830003000E3Q00201A01030003000F00122Q000400103Q00122Q000500113Q00122Q000600126Q00030006000200102Q0002000D00032Q0060010200034Q0060010300044Q0060010400054Q000801053Q00010012830006000E3Q00201A01060006000F00122Q000700103Q00122Q000800113Q00122Q000900126Q00060009000200102Q0005001300062Q002B010200050001001283000200143Q002040000200020015001253010300164Q00A80102000200012Q0060010200014Q009A0102000100012Q00EF3Q00017Q000A3Q0003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574030C3Q004162736F6C75746553697A6503013Q0058028Q0003043Q007461736B03043Q0077616974020AD7A3703D0AC73F03073Q0044657374726F7900184Q0071019Q000100018Q000200016Q00016Q000100026Q000200036Q00033Q000100122Q000400023Q00202Q0004000400034Q000500023Q002040000500050004002040000500050005001253010600064Q004F00040006000200102C0103000100042Q002B012Q000300010012833Q00073Q0020405Q00080012532Q0100094Q00A8012Q000200012Q0060012Q00043Q002012014Q000A2Q00A8012Q000200012Q00EF3Q00017Q000C3Q0003073Q0056697369626C65010003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00407A40026Q004140026Q00744003043Q007461736B03043Q007761697402B81E85EB51B8BE3F2Q0100264Q009A9Q009Q009Q007Q00064Q001400013Q000457012Q001400012Q0060012Q00013Q0030783Q000100026Q00026Q000100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300046Q0003000100044Q002500012Q0060012Q00024Q00FF000100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600086Q00040006000200102Q0003000300046Q0003000100124Q00093Q00206Q000A00122Q0001000B8Q000200016Q00013Q00304Q0001000C2Q00EF3Q00017Q00063Q0003073Q004B6579436F646503073Q0067657467656E7603093Q00546F2Q676C654B657903043Q00456E756D03013Q004B03073Q0056697369626C6502193Q0006460001000500010001000457012Q000500012Q006001025Q0006380102000600013Q000457012Q000600012Q00EF3Q00013Q00204000023Q0001001283000300024Q00020003000100020020400003000300030006460003000F00010001000457012Q000F0001001283000300043Q0020400003000300010020400003000300050006880002001800010003000457012Q001800012Q0060010200014Q00D6000200026Q000200016Q000200026Q000300016Q000300033Q00102Q0002000600032Q00EF3Q00017Q00593Q0003063Q005F6F72646572028Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577026Q003C4003073Q0056697369626C6503063Q0048692Q64656E03063Q00506172656E74026Q00144003093Q0049636F6E436F6C6F7203073Q005375625465787403043Q0049636F6E03063Q00636972636C65026Q002E40030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q00184003093Q00546578744C6162656C03043Q004E616D652Q033Q0054616203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q003A40026Q003EC003053Q004672616D6503043Q0050616765030F3Q00426F7264657253697A65506978656C030E3Q005363726F2Q6C696E674672616D65030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q73027Q004003143Q005363726F2Q6C426172496D616765436F6C6F723303063Q005374726F6B65031A3Q005363726F2Q6C426172496D6167655472616E73706172656E6379030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D03093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70026Q002040030D3Q0050612Q64696E67426F2Q746F6D030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E675269676874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403043Q005F62746E03053Q005F69636F6E03053Q005F6E616D6503053Q005F7061676503053Q005F7772617003073Q005F73656C656374030A3Q005F69636F6E436F6C6F7203053Q007461626C6503063Q00696E7365727403043Q0054616273030D3Q0043726561746553656374696F6E030C3Q00437265617465546F2Q676C65030C3Q00437265617465536C69646572030D3Q0043726561746554657874626F78030B3Q004372656174654C6162656C030C3Q0043726561746542752Q746F6E03123Q00437265617465436F726E657242752Q746F6E030C3Q00437265617465427579526F770256012Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100024Q000801023Q00010030270002000100022Q006001035Q001253010400034Q000801053Q00070030270005000400050030270005000600072Q0060010600013Q00204000060006000900102C0105000800060030270005000A000B0012830006000D3Q00206401060006000E00122Q0007000B3Q00122Q000800023Q00122Q000900023Q00122Q000A000F6Q0006000A000200102C0105000C00060020400006000100112Q00A8000600063Q00102C0105001000062Q0060010600023Q00102C0105001200062Q004F0003000500022Q0060010400034Q0055010500033Q001253010600134Q002B0104000600010020400004000100140006460004002600010001000457012Q002600012Q0060010400013Q0020400004000400152Q0060010500043Q0020400006000100160006460006002B00010001000457012Q002B0001001253010600173Q001253010700184Q009F00086Q0055010900044Q004F0005000900020012680006001A3Q00202Q00060006000E00122Q000700023Q00122Q0008001B6Q00060008000200102Q00050019000600122Q0006000D3Q00202Q00060006000E00122Q000700023Q00122Q0008001D3Q00122Q0009001B3Q00122Q000A00026Q0006000A000200102Q0005001C000600102Q0005001200034Q00065Q00122Q0007001E6Q00083Q000B00302Q0008000A000B00202Q00090001001F00062Q0009004600010001000457012Q00460001001253010900203Q00102C0108000400092Q0033010900053Q00102Q0008002100094Q000900013Q00202Q00090009001500102Q00080022000900302Q00080023002400122Q000900263Q00202Q00090009002500202Q00090009002700102Q00080025000900122Q000900263Q00202Q00090009002800202Q00090009002900102Q00080028000900122Q0009001A3Q00202Q00090009000E00122Q000A00023Q00122Q000B001B6Q0009000B000200102Q00080019000900122Q0009000D3Q00202Q00090009000E00122Q000A00023Q00122Q000B002A3Q00122Q000C001B3Q00122Q000D00026Q0009000D000200102Q0008001C000900122Q0009000D3Q00202Q00090009000E00122Q000A000B3Q00122Q000B002B3Q00122Q000C000B3Q00122Q000D00026Q0009000D000200102Q0008000C000900102Q0008001200034Q0006000800024Q00075Q00122Q0008002C6Q00093Q000600302Q0009001F002D00302Q0009000A000B00302Q0009002E000200122Q000A000D3Q00202Q000A000A000E00122Q000B000B3Q00122Q000C00023Q00122Q000D000B3Q00122Q000E00026Q000A000E000200102Q0009000C000A00302Q0009001000074Q000A00063Q00102Q00090012000A4Q0007000900024Q00085Q00122Q0009002F6Q000A3Q000900302Q000A000A000B00302Q000A002E000200122Q000B000D3Q00202Q000B000B000E00122Q000C000B3Q00122Q000D00023Q00122Q000E000B3Q00122Q000F00026Q000B000F000200102Q000A000C000B00122Q000B000D3Q00202Q000B000B000E4Q000B0001000200102Q000A0030000B00122Q000B00263Q00202Q000B000B003200202Q000B000B003300102Q000A0031000B00302Q000A003400354Q000B00013Q00202Q000B000B003700102C010A0036000B00304E000A0038001B00102Q000A001200074Q000B00016Q000C5Q00122Q000D00396Q000E3Q000200122Q000F003B3Q00202Q000F000F000E00122Q001000023Q00122Q0011001D4Q002B000F0011000200102Q000E003A000F00122Q000F00263Q00202Q000F000F003C00202Q000F000F003D00102Q000E003C000F4Q000C000E00024Q000D5Q00122Q000E003E6Q000F3Q00040012130110003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F003F001000122Q0010003B3Q00203901100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F0041001000122Q0010003B3Q00203901100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F0042001000122Q0010003B3Q00204000100010000E001253011100023Q001253011200404Q004F00100012000200102C010F004300102Q00D4000D000F4Q0046010B3Q00012Q004F0008000B000200062F01093Q0001000B2Q0060012Q00074Q0055012Q00024Q0055012Q00014Q0055012Q00074Q0060012Q00084Q0060012Q00094Q0060012Q000A4Q0060012Q00014Q0055012Q00034Q0055012Q00054Q0055012Q00063Q002040000A00030044002012010A000A004500062F010C0001000100042Q0055012Q00074Q0060012Q00084Q0055012Q00034Q0060012Q000A4Q002B010A000C0001002040000A00030046002012010A000A004500062F010C0002000100042Q0055012Q00074Q0060012Q00084Q0055012Q00034Q0060012Q000A4Q009E010A000C000100202Q000A0003004700202Q000A000A00454Q000C00096Q000A000C00014Q000A00036Q000B00056Q000C00066Q000D00086Q000E00073Q00102C0102004D00090010FB0002004C000E00102Q0002004B000D00102Q0002004A000C00102Q00020049000B00102Q00020048000A00102Q0002004E000400122Q000A004F3Q00202Q000A000A00504Q000B00073Q00202Q000B000B00512Q0055010C00024Q002B010A000C00012Q0060010A00073Q002040000A000A00512Q007A000A000A3Q00267C010A2Q002Q01000B000457013Q002Q012Q0055010A00094Q009A010A0001000100062F010A0003000100072Q0055012Q00024Q0060017Q0060012Q00014Q0055012Q00084Q0060012Q00034Q0060012Q000B4Q0060012Q000C3Q00062F010B0004000100062Q0055012Q00024Q0060017Q0055012Q00084Q0060012Q00044Q0060012Q00014Q0060012Q000D3Q00102C01020052000B00062F010B0005000100082Q0055012Q000A4Q0060017Q0060012Q00044Q0060012Q00014Q0060012Q00054Q0060012Q00034Q0060012Q00084Q0060012Q000A3Q00102C01020053000B00062F010B0006000100072Q0055012Q000A4Q0060017Q0060012Q00054Q0060012Q00014Q0060012Q000D4Q0060012Q00034Q0060012Q000E3Q00102C01020054000B00062F010B0007000100092Q0055012Q000A4Q0060017Q0060012Q00054Q0060012Q00014Q0060012Q00034Q0060012Q000B4Q0060012Q000C4Q0060012Q00084Q0060012Q000A3Q00102C01020055000B00062F010B0008000100062Q0055012Q000A4Q0060012Q00044Q0060012Q00014Q0060017Q0060012Q00054Q0060012Q000D3Q00102C01020056000B00062F010B0009000100072Q0055012Q000A4Q0060017Q0060012Q00044Q0060012Q00014Q0060012Q000D4Q0060012Q00084Q0060012Q000A3Q00102C01020057000B00062F010B000A0001000A2Q0055012Q00084Q0060017Q0060012Q00014Q0055012Q00074Q0060012Q00034Q0060012Q000B4Q0060012Q00044Q0060012Q000D4Q0060012Q00084Q0060012Q000A3Q00102C01020058000B00062F010B000B000100072Q0055012Q000A4Q0060017Q0060012Q00054Q0060012Q00014Q0060012Q000D4Q0060012Q00034Q0060012Q000F3Q00102C01020059000B2Q0079000200024Q00EF3Q00013Q000C3Q00193Q0003083Q005F63752Q72656E7403063Q0048692Q64656E03063Q0069706169727303043Q005461627303053Q005F7772617003073Q0056697369626C6501002Q0103083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577027B14AE47E17AA43F028Q0003043Q005F62746E03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03053Q005F69636F6E030A3Q0054657874436F6C6F7233030A3Q005F69636F6E436F6C6F7203073Q005375625465787403053Q005F6E616D6503113Q0053656C656374656449636F6E436F6C6F7203093Q0049636F6E436F6C6F7203063Q00412Q63656E7403043Q005465787400914Q0060016Q0020405Q00012Q00602Q0100013Q0006883Q000600010001000457012Q000600012Q00EF3Q00014Q0060012Q00023Q0020405Q0002000638012Q003200013Q000457012Q003200010012833Q00034Q00602Q015Q0020400001000100042Q00E13Q00020002000457012Q001400012Q0060010500013Q0006F20004001400010005000457012Q00140001002040000500040005003027000500060007000663012Q000F00010002000457012Q000F00012Q0060017Q001C000100013Q00104Q000100016Q00033Q00304Q000600086Q00033Q00122Q0001000A3Q00202Q00010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q00010005000200104Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00202Q00040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q00040008000200102Q0003000900046Q000300016Q00013Q0012833Q00034Q00602Q015Q0020400001000100042Q00E13Q00020002000457012Q005500012Q0060010500013Q0006F20004003C00010005000457012Q003C00010020400005000400050030270005000600072Q0060010500043Q00204000060004000E2Q0060010700064Q000801083Q00010030270008000F00102Q002B0105000800012Q0060010500043Q0020400006000400112Q0060010700064Q000801083Q00010020400009000400130006460009004B00010001000457012Q004B00012Q0060010900073Q00204000090009001400102C0108001200092Q005C0005000800014Q000500043Q00202Q0006000400154Q000700066Q00083Q00014Q000900073Q00202Q00090009001400102Q0008001200094Q000500080001000663012Q003700010002000457012Q003700012Q0060017Q00602Q0100013Q00102C012Q000100012Q0060012Q00033Q0030273Q000600082Q0060012Q00033Q0012830001000A3Q0020642Q010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q000100050002001032012Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00206401040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q00040008000200102C0103000900042Q002B012Q000300012Q0060012Q00044Q00A12Q0100086Q000200066Q00033Q000100302Q0003000F000D6Q000300012Q00933Q00046Q000100096Q000200066Q00033Q00014Q000400023Q00202Q00040004001600062Q0004008600010001000457012Q008600012Q0060010400023Q0020400004000400170006460004008600010001000457012Q008600012Q0060010400073Q00204000040004001800102C0103001200042Q002B012Q000300012Q0060012Q00044Q00A22Q01000A6Q000200066Q00033Q00014Q000400073Q00202Q00040004001900102Q0003001200046Q000300012Q00EF3Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q33E33F000B4Q0060016Q0020405Q00010006463Q000A00010001000457012Q000A00012Q0060012Q00014Q00A12Q0100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q00EF3Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F000B4Q0060016Q0020405Q00010006463Q000A00010001000457012Q000A00012Q0060012Q00014Q00A12Q0100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q00EF3Q00017Q000F3Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003C40030B3Q004C61796F75744F72646572030F3Q00426F7264657253697A65506978656C03063Q00506172656E74026Q00144003063Q005374726F6B6501284Q00812Q018Q00025Q00202Q00020002000100202Q00020002000200102Q0001000100024Q000100013Q00122Q000200036Q00033Q00054Q000400023Q00202Q00040004000500102Q00030004000400122Q000400073Q00202Q00040004000800122Q000500023Q00122Q000600093Q00122Q000700093Q00062Q0008001300013Q000457012Q001300010012530108000A4Q004F00040008000200102C0103000600042Q006001045Q00204000040004000100102C0103000B00040030270003000C00092Q0060010400033Q00102C0103000D00042Q004F0001000300022Q0060010200044Q0055010300013Q0012530104000E4Q00DB0002000400014Q000200056Q000300016Q000400023Q00202Q00040004000F4Q000500066Q0002000500014Q000100028Q00017Q00213Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503163Q004261636B67726F756E645472616E73706172656E637903043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003640030B3Q004C61796F75744F7264657203063Q00506172656E7403043Q0049636F6E030D3Q00637562652D7665727465786573026Q002C4003093Q0049636F6E436F6C6F7203063Q00412Q63656E74030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E027Q004003093Q00546578744C6162656C03043Q005465787403043Q004E616D6503073Q0053656374696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q0036C002643Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100024Q006001026Q007901035Q00202Q00030003000100202Q00030003000200102Q0002000100034Q000200013Q00122Q000300036Q00043Q000400302Q00040004000200122Q000500063Q00202Q00050005000700122Q000600023Q00122Q000700083Q00122Q000800083Q00122Q000900096Q00050009000200102Q0004000500054Q00055Q00202Q00050005000100102Q0004000A00054Q000500023Q00102Q0004000B00054Q0002000400024Q000300033Q00202Q00040001000C00062Q0004002000010001000457012Q002000010012530104000D3Q0012530105000E4Q009F00065Q00204000070001000F0006460007002700010001000457012Q002700012Q0060010700043Q0020400007000700102Q004F000300070002001268000400123Q00202Q00040004000700122Q000500083Q00122Q000600136Q00040006000200102Q00030011000400122Q000400063Q00202Q00040004000700122Q000500083Q00122Q000600153Q00122Q000700133Q00122Q000800086Q00040008000200102Q00030014000400102Q0003000B00024Q000400013Q00122Q000500166Q00063Q000A00302Q00060004000200202Q00070001001800062Q0007003F00010001000457012Q003F0001001253010700193Q00102C0106001700072Q006B010700053Q00102Q0006001A00074Q000700043Q00202Q00070007001700102Q0006001B000700302Q0006001C001D00122Q0007001F3Q00202Q00070007001E00202Q00070007002000102Q0006001E000700122Q000700123Q00202Q00070007000700122Q000800083Q00122Q000900136Q00070009000200102Q00060011000700122Q000700063Q00202Q00070007000700122Q000800083Q00122Q000900093Q00122Q000A00133Q00122Q000B00086Q0007000B000200102Q00060014000700122Q000700063Q00202Q00070007000700122Q000800023Q00122Q000900213Q00122Q000A00023Q00122Q000B00086Q0007000B000200102Q00060005000700102Q0006000B00024Q0004000600014Q000200028Q00017Q00343Q0003073Q0044656661756C74026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00546578744C6162656C03043Q004E616D6503063Q00546F2Q676C6503083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674025Q0080474003053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q66026Q0020C0030A3Q0066726F6D4F2Q66736574026Q002Q40026Q003040030F3Q00426F7264657253697A65506978656C026Q002CC0027Q0040026Q0018402Q033Q005365742Q033Q0047657403093Q0041637469766174656403073Q00436F2Q6E65637403083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03083Q00496E7374616E636502D43Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100023Q0020400002000100010006460002000800010001000457012Q000800012Q009F00026Q006001035Q0012EE000400026Q0003000200024Q000400013Q00122Q000500036Q00063Q000400302Q00060004000500302Q00060006000700122Q000700093Q00202Q00070007000A00122Q000800073Q00122Q0009000B3Q00122Q000A00073Q00122Q000B000B6Q0007000B000200102Q00060008000700102Q0006000C00034Q00040006000200122Q0005000D3Q00202Q00060001000E00062Q0006003500013Q000457012Q003500012Q0060010600023Q0020AB00070001000E00122Q0008000F6Q00098Q000A00033Q00202Q000A000A00104Q0006000A0002001252010700123Q00202Q00070007000A00122Q0008000B3Q00122Q000900136Q00070009000200102Q00060011000700122Q000700093Q00202Q00070007000A00122Q0008000B3Q00122Q0009000D3Q00122Q000A00133Q00122Q000B000B6Q0007000B000200102Q00060014000700102Q0006000C000400122Q000500154Q0060010600013Q00122F000700166Q00083Q000A00302Q00080006000700202Q00090001001700062Q0009003D00010001000457012Q003D0001001253010900183Q00102C0108000400092Q00C4000900043Q00102Q0008001900094Q000900033Q00202Q00090009000400102Q0008001A000900302Q0008001B001C00122Q0009001E3Q00202Q00090009001D00202Q00090009001F00102Q0008001D0009001213010900123Q00202Q00090009000A00122Q000A000B3Q00122Q000B00136Q0009000B000200102Q00080011000900122Q000900093Q00204000090009000A001253010A000B4Q0055010B00053Q0012AA000C00133Q00122Q000D000B6Q0009000D000200102Q00080014000900122Q000900093Q00204000090009000A001253010A00073Q00209D000B000500202Q0082010B000B3Q001253010C00073Q001253010D000B4Q004F0009000D000200102C01080008000900102C0108000C00042Q002B0106000800012Q0060010600013Q001253010700214Q000801083Q00060006380102006A00013Q000457012Q006A00012Q0060010900033Q0020400009000900230006460009006C00010001000457012Q006C00012Q0060010900033Q00204000090009002400102C010800220009001213010900123Q00202Q00090009000A00122Q000A00073Q00122Q000B00136Q0009000B000200102Q00080011000900122Q000900093Q00206401090009000A00122Q000A00073Q00122Q000B00253Q00122Q000C00133Q00122Q000D000B6Q0009000D000200102C010800140009001283000900093Q002040000900090026001253010A00273Q001253010B00284Q004F0009000B000200102C01080008000900302700080029000B00102C0108000C00042Q004F0006000800022Q0060010700054Q0055010800063Q0012530109000D4Q002B0107000900012Q0060010700013Q001253010800214Q000801093Q00062Q0060010A00033Q002040000A000A000400102C01090022000A001283000A00123Q002040000A000A000A001253010B000B3Q001253010C00134Q004F000A000C000200102C01090011000A0006380102009F00013Q000457012Q009F0001001283000A00093Q002064010A000A000A00122Q000B00073Q00122Q000C002A3Q00122Q000D00133Q00122Q000E000B6Q000A000E0002000646000A00A600010001000457012Q00A60001001283000A00093Q002064010A000A000A00122Q000B000B3Q00122Q000C002B3Q00122Q000D00133Q00122Q000E000B6Q000A000E000200102C01090014000A001286000A00093Q00202Q000A000A002600122Q000B001C3Q00122Q000C001C6Q000A000C000200102Q00090008000A00302Q00090029000B00102Q0009000C00064Q0007000900024Q000800056Q000900073Q00122Q000A002C6Q0008000A00014Q00085Q00062F01093Q000100072Q0055012Q00024Q0060012Q00064Q0055012Q00064Q0060012Q00074Q0060012Q00034Q0055012Q00074Q0055012Q00013Q00102C0108002D000900062F01090001000100012Q0055012Q00023Q00102C0108002E000900204000090004002F00201201090009003000062F010B0002000100022Q0055012Q00084Q0055012Q00024Q002B0109000B0001000638010200D100013Q000457012Q00D10001002040000900010031000638010900D100013Q000457012Q00D10001001283000900323Q002040000900090033002040000A000100312Q009F000B00014Q002B0109000B000100102C0108003400032Q0079000800024Q00EF3Q00013Q00033Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02344Q008E00018Q000200016Q000300026Q000400036Q00053Q00014Q00065Q00062Q0006000C00013Q000457012Q000C00012Q0060010600043Q0020400006000600020006460006000E00010001000457012Q000E00012Q0060010600043Q00204000060006000300102C0105000100062Q00500002000500014Q000200016Q000300056Q000400036Q00053Q00014Q00065Q00062Q0006002000013Q000457012Q00200001001283000600053Q00206401060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A00020006460006002700010001000457012Q00270001001283000600053Q00206401060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A000200102C0105000400062Q002B0102000500012Q0060010200063Q00204000020002000C0006380102003300013Q000457012Q003300010012830002000D3Q00205E00020002000E4Q000300063Q00202Q00030003000C4Q00048Q0002000400012Q00EF3Q00019Q002Q0001034Q00602Q016Q0079000100024Q00EF3Q00017Q00013Q002Q033Q0053657400064Q0035016Q00206Q00014Q000200016Q000200028Q000200016Q00017Q003A3Q002Q033Q004D696E028Q002Q033Q004D6178026Q00594003093Q00496E6372656D656E74026Q00F03F03043Q006D61746803053Q00636C616D7003073Q0044656661756C74026Q00454003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E637903043Q005465787403043Q004E616D6503063Q00536C6964657203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q002040026Q00104003043Q0053697A65026Q004EC0026Q002C4003063Q00506172656E7403083Q00746F737472696E6703063Q00412Q63656E7403053Q005269676874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q0020C0026Q004940030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66026Q00E03F026Q0026C0026Q0030C0026Q002440030F3Q00426F7264657253697A65506978656C026Q00084003053Q004672616D65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578027Q0040026Q0018402Q033Q005365742Q033Q0047657403083Q00496E7374616E636502E53Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100023Q0020400002000100010006460002000800010001000457012Q00080001001253010200023Q0020400003000100030006460003000C00010001000457012Q000C0001001253010300043Q0020400004000100050006460004001000010001000457012Q00100001001253010400063Q001283000500073Q0020400005000500080020400006000100090006460006001600010001000457012Q001600012Q0055010600024Q0055010700024Q009B010800036Q0005000800024Q00065Q00122Q0007000A6Q0006000200024Q000700013Q00122Q0008000B6Q00093Q000900302Q0009000C000600202Q000A0001000E00062Q000A002400010001000457012Q00240001001253010A000F3Q00102C0109000D000A2Q00C4000A00023Q00102Q00090010000A4Q000A00033Q00202Q000A000A000D00102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A001600102Q00090014000A001283000A00183Q002064010A000A001900122Q000B00023Q00122Q000C001A3Q00122Q000D00023Q00122Q000E001B6Q000A000E000200102C01090017000A001283000A00183Q002064010A000A001900122Q000B00063Q00122Q000C001D3Q00122Q000D00023Q00122Q000E001E6Q000A000E000200102C0109001C000A00102C0109001F00062Q002B0107000900012Q0060010700013Q0012530108000B4Q000801093Q000A0030270009000C0006001283000A00204Q0055010B00054Q0020000A0002000200102C0109000D000A2Q00C4000A00043Q00102Q00090010000A4Q000A00033Q00202Q000A000A002100102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A002200102Q00090014000A001213010A00243Q00202Q000A000A001900122Q000B00063Q00122Q000C00026Q000A000C000200102Q00090023000A00122Q000A00183Q002064010A000A001900122Q000B00063Q00122Q000C00253Q00122Q000D00023Q00122Q000E001B6Q000A000E000200102C01090017000A001283000A00183Q002064010A000A001900122Q000B00023Q00122Q000C00263Q00122Q000D00023Q00122Q000E001E6Q000A000E00020010650009001C000A00102Q0009001F00064Q0007000900024Q000800013Q00122Q000900276Q000A3Q000800302Q000A000D002800302Q000A0029002A4Q000B00033Q00202Q000B000B002C00102C010A002B000B001283000B00243Q002040000B000B0019001253010C00023Q001203010D002D6Q000B000D000200102Q000A0023000B00122Q000B00183Q00202Q000B000B001900122Q000C00023Q00122Q000D001A3Q00122Q000E00063Q00122Q000F002E6Q000B000F000200102Q000A0017000B00122Q000B00183Q00202Q000B000B001900122Q000C00063Q00122Q000D002F3Q00122Q000E00023Q00122Q000F00306Q000B000F000200102Q000A001C000B00302Q000A0031000200102Q000A001F00064Q0008000A00024Q000900056Q000A00083Q00122Q000B00326Q0009000B00014Q000900013Q00122Q000A00336Q000B3Q00044Q000C00033Q00202Q000C000C002100102Q000B002B000C00122Q000C00183Q00202Q000C000C00194Q000D000500024Q000E000300024Q000D000D000E00122Q000E00023Q00122Q000F00063Q00122Q001000026Q000C0010000200102Q000B001C000C00302Q000B0031000200102Q000B001F00084Q0009000B00024Q000A00056Q000B00093Q00122Q000C00326Q000A000C00014Q000A00013Q00122Q000B00336Q000C3Q00074Q000D00033Q00202Q000D000D000D00102Q000C002B000D00122Q000D00243Q00202Q000D000D001900122Q000E002D3Q00122Q000F002D6Q000D000F000200102Q000C0023000D00122Q000D00183Q00202Q000D000D00194Q000E000500024Q000F000300024Q000E000E000F00122Q000F00023Q00122Q0010002D3Q00122Q001100026Q000D0011000200102Q000C0017000D00122Q000D00183Q00202Q000D000D003400122Q000E00133Q00122Q000F00136Q000D000F000200102Q000C001C000D00302Q000C0031000200302Q000C0035003600102Q000C001F00082Q004F000A000C00022Q0060010B00054Q0055010C000A3Q001253010D00374Q002B010B000D00012Q0008010B5Q00062F010C3Q000100082Q0055012Q00024Q0055012Q00034Q0055012Q00054Q0055012Q00044Q0055012Q00094Q0055012Q000A4Q0055012Q00074Q0055012Q00014Q0060010D00064Q0055010E00083Q00062F010F0001000100012Q0055012Q000C4Q002B010D000F000100062F010D0002000100032Q0055012Q000C4Q0055012Q00024Q0055012Q00033Q00102C010B0038000D00062F010D0003000100012Q0055012Q00053Q00102C010B0039000D00102C010B003A00062Q0079000B00024Q00EF3Q00013Q00043Q000F3Q0003043Q006D61746803053Q00636C616D7003053Q00666C2Q6F72026Q00E03F028Q0003043Q0053697A6503053Q005544696D322Q033Q006E6577026Q00F03F03083Q00506F736974696F6E03043Q005465787403083Q00746F737472696E6703083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02474Q005600028Q000300016Q00048Q0003000300044Q000300036Q00020002000300122Q000300013Q00202Q00030003000200122Q000400013Q00202Q0004000400032Q0060010500034Q009600050002000500202Q0005000500044Q0004000200024Q000500036Q0004000400054Q00058Q000600016Q0003000600024Q000300026Q000300014Q006001046Q009D01030003000400267C0103001C00010005000457012Q001C0001001253010300053Q0006460003002300010001000457012Q002300012Q0060010300024Q005B01048Q0003000300044Q000400016Q00058Q0004000400054Q0003000300042Q0060010400043Q001283000500073Q0020400005000500082Q0055010600033Q001253010700053Q001253010800093Q001253010900054Q004F00050009000200102C0104000600052Q0060010400053Q001283000500073Q0020400005000500082Q0055010600033Q001253010700053Q001253010800043Q001253010900054Q004F00050009000200102C0104000A00052Q0060010400063Q0012830005000C4Q0060010600024Q002000050002000200102C0104000B00050006382Q01004600013Q000457012Q004600012Q0060010400073Q00204000040004000D0006380104004600013Q000457012Q004600010012830004000E3Q00205E00040004000F4Q000500073Q00202Q00050005000D4Q000600026Q0004000600012Q00EF3Q00019Q002Q0001054Q000F00018Q00028Q000300016Q0001000300016Q00017Q00023Q0003043Q006D61746803053Q00636C616D7002104Q00AC00025Q00122Q000300013Q00202Q0003000300024Q000400016Q000500016Q000600026Q0003000600024Q000400016Q0003000300044Q000400026Q000500016Q0004000400054Q0003000300044Q000400016Q0002000400016Q00019Q002Q0001034Q00602Q016Q0079000100024Q00EF3Q00017Q00453Q00026Q003C4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503073Q0054657874626F7803083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65029A5Q99D93F026Q0020C003063Q00506172656E7403053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E64617279026Q0018C0026Q003440030D3Q004175746F6D6174696353697A6503013Q005803103Q00436C69707344657363656E64616E74732Q01030F3Q00426F7264657253697A65506978656C03093Q00554950612Q64696E67030B3Q0050612Q64696E674C65667403043Q005544696D026Q001440030C3Q0050612Q64696E675269676874026Q00104003063Q005374726F6B6503073Q0054657874426F7803073Q0044656661756C74034Q00030F3Q00506C616365686F6C64657254657874030B3Q00506C616365686F6C6465722Q033Q003Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q0053756254657874026Q00264003103Q00436C656172546578744F6E466F637573010003103Q00554953697A65436F6E73747261696E7403073Q004D696E53697A6503083Q004D696E5769647468025Q0080464003073Q004D617853697A6503083Q004D61785769647468025Q0080614003043Q006D61746803043Q006875676503073Q00466F637573656403073Q00436F2Q6E65637403093Q00466F6375734C6F73742Q033Q005365742Q033Q0047657403083Q00496E7374616E636502D23Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100024Q006001025Q001253010300014Q00200002000200022Q0060010300013Q00122F000400026Q00053Q000A00302Q00050003000400202Q00060001000600062Q0006000F00010001000457012Q000F0001001253010600073Q00102C0105000500062Q00C4000600023Q00102Q0005000800064Q000600033Q00202Q00060006000500102Q00050009000600302Q0005000A000B00122Q0006000D3Q00202Q00060006000C00202Q00060006000E00102Q0005000C0006001213010600103Q00202Q00060006001100122Q000700123Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00206401060006001100122Q000700123Q00122Q000800163Q00122Q000900133Q00122Q000A00126Q0006000A000200102C010500140006001283000600153Q00206401060006001100122Q000700183Q00122Q000800193Q00122Q000900043Q00122Q000A00126Q0006000A000200102C01050017000600102C0105001A00022Q002B0103000500012Q0060010300013Q0012530104001B4Q000801053Q00082Q0060010600033Q00204000060006001D00102C0105001C0006001213010600103Q00202Q00060006001100122Q000700043Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00206401060006001100122Q000700043Q00122Q0008001E3Q00122Q000900133Q00122Q000A00126Q0006000A000200102C010500140006001283000600153Q00206401060006001100122Q000700123Q00122Q000800123Q00122Q000900123Q00122Q000A001F6Q0006000A000200104A00050017000600122Q0006000D3Q00202Q00060006002000202Q00060006002100102Q00050020000600302Q00050022002300302Q00050024001200102Q0005001A00024Q00068Q000700013Q001253010800254Q000801093Q0002001213010A00273Q00202Q000A000A001100122Q000B00123Q00122Q000C00286Q000A000C000200102Q00090026000A00122Q000A00273Q002040000A000A0011001253010B00123Q001253010C00284Q004F000A000C000200102C01090029000A2Q00D4000700094Q004601063Q00012Q004F0003000600022Q0044000400046Q000500033Q00122Q0006002A6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006002B4Q000700066Q0004000700024Q000500013Q00122Q0006002C6Q00073Q000C00302Q00070003000400202Q00080001002D00062Q0008007A00010001000457012Q007A00010012530108002E3Q00102C0107000500080020400008000100300006460008007F00010001000457012Q007F0001001253010800313Q00102C0107002F00082Q004E010800033Q00202Q00080008003300102Q0007003200084Q000800023Q00102Q0007000800084Q000800033Q00202Q00080008000500102Q00070009000800302Q0007000A003400302Q0007003500360012830008000D3Q0020A200080008000C00202Q00080008000E00102Q0007000C000800122Q0008000D3Q00202Q00080008002000202Q00080008002100102Q00070020000800122Q000800153Q00206401080008001100122Q000900123Q00122Q000A00123Q00122Q000B00043Q00122Q000C00126Q0008000C000200106200070017000800102Q0007001A00034Q00088Q000900013Q00122Q000A00376Q000B3Q000200122Q000C00103Q00202Q000C000C001100202Q000D0001003900062Q000D00A500010001000457012Q00A50001001253010D003A3Q001253010E00124Q0051000C000E000200102Q000B0038000C00122Q000C00103Q00202Q000C000C001100202Q000D0001003C00062Q000D00AE00010001000457012Q00AE0001001253010D003D3Q001283000E003E3Q0020A6010E000E003F4Q000C000E000200102Q000B003B000C4Q0009000B6Q00083Q00012Q004F00050008000200204000060005004000201201060006004100062F01083Q000100042Q0060012Q00074Q0055012Q00044Q0060012Q00084Q0060012Q00034Q002B01060008000100204000060005004200201201060006004100062F01080001000100072Q0060012Q00074Q0055012Q00044Q0060012Q00084Q0060012Q00034Q0060012Q00064Q0055012Q00014Q0055012Q00054Q002B0106000800012Q000801063Q000300062F01070002000100012Q0055012Q00053Q00102C01060043000700062F01070003000100012Q0055012Q00053Q00102C01060044000700102C0106004500022Q0079000600024Q00EF3Q00013Q00043Q00043Q0003053Q00436F6C6F7203063Q00412Q63656E74030C3Q005472616E73706172656E6379029A5Q99C93F000A4Q0060017Q00602Q0100014Q0060010200024Q000801033Q00022Q0060010400033Q00204000040004000200102C0103000100040030270003000300042Q002B012Q000300012Q00EF3Q00017Q00073Q0003053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E637903083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03043Q005465787400164Q0060017Q00602Q0100014Q0060010200024Q000801033Q00022Q0060010400033Q00204000040004000200102C0103000100042Q0060010400043Q00102C0103000300042Q002B012Q000300012Q0060012Q00053Q0020405Q0004000638012Q001500013Q000457012Q001500010012833Q00053Q0020405Q00062Q00602Q0100053Q0020400001000100042Q0060010200063Q0020400002000200072Q002B012Q000200012Q00EF3Q00017Q00013Q0003043Q005465787402034Q006001025Q00102C0102000100012Q00EF3Q00017Q00013Q0003043Q005465787400044Q0060016Q0020405Q00012Q00793Q00024Q00EF3Q00017Q00253Q00026Q003C40026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D3203063Q00506172656E74026Q003A4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503053Q004C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403043Q0053697A6503053Q0056616C7565034Q0003063Q00412Q63656E7403053Q005269676874026Q0020C02Q033Q005365742Q033Q0047657403083Q00496E7374616E636502853Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100024Q006001025Q00121E000300016Q00020002000200122Q000300023Q00202Q00040001000300062Q0004002500013Q000457012Q002500012Q0060010400013Q002040000500010003001253010600044Q009F00075Q0020400008000100050006460008001400010001000457012Q001400012Q0060010800023Q0020400008000800062Q004F000400080002001252010500083Q00202Q00050005000900122Q0006000A3Q00122Q0007000B6Q00050007000200102Q00040007000500122Q0005000D3Q00202Q00050005000900122Q0006000A3Q00122Q000700023Q00122Q0008000B3Q00122Q0009000A6Q00050009000200102Q0004000C000500102Q0004000E000200122Q0003000F4Q0060010400033Q00122F000500106Q00063Q000A00302Q00060011001200202Q00070001001400062Q0007002D00010001000457012Q002D0001001253010700153Q00102C0106001300072Q00D8000700043Q00102Q0006001600074Q000700023Q00202Q00070007001300102Q00060017000700302Q00060018001900122Q0007001B3Q00202Q00070007001A00202Q00070007001C00102Q0006001A000700122Q000700083Q00202Q00070007000900122Q0008000A3Q00122Q0009000B6Q00070009000200102Q00060007000700122Q0007000D3Q00202Q00070007000900122Q0008000A6Q000900033Q00122Q000A000B3Q00122Q000B000A6Q0007000B000200102Q0006000C000700122Q0007000D3Q00202Q00070007000900122Q0008000B6Q000900033Q00122Q000A00123Q00122Q000B000A6Q0007000B000200102Q0006001D000700102Q0006000E00024Q0004000600024Q000500033Q00122Q000600106Q00073Q000A00302Q00070011001200202Q00080001001E00062Q0008005800010001000457012Q005800010012530108001F3Q00102C0107001300082Q0029010800053Q00102Q0007001600084Q000800023Q00202Q00080008002000102Q00070017000800302Q00070018001900122Q0008001B3Q00202Q00080008001A00202Q00080008002100102Q0007001A000800122Q000800083Q00202Q00080008000900122Q000900123Q00122Q000A000B6Q0008000A000200102Q00070007000800122Q0008000D3Q00202Q00080008000900122Q000900123Q00122Q000A00223Q00122Q000B000B3Q00122Q000C000A6Q0008000C000200102Q0007000C000800122Q0008000D3Q00202Q00080008000900122Q0009000B3Q00122Q000A00223Q00122Q000B00123Q00122Q000C000A6Q0008000C000200102Q0007001D000800102Q0007000E00024Q0005000700024Q00063Q000300062F01073Q000100012Q0055012Q00053Q00102C01060023000700062F01070001000100012Q0055012Q00053Q00102C01060024000700102C0106002500022Q0079000600024Q00EF3Q00013Q00023Q00023Q0003043Q005465787403083Q00746F737472696E6702064Q007F01025Q00122Q000300026Q000400016Q00030002000200102Q0002000100036Q00017Q00013Q0003043Q005465787400044Q0060016Q0020405Q00012Q00793Q00024Q00EF3Q00017Q002C3Q00026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00526967687449636F6E030E3Q00526967687449636F6E436F6C6F72026Q0020C003093Q00546578744C6162656C03043Q004E616D6503063Q0042752Q746F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E6365030C3Q00536574526967687449636F6E02A53Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100024Q006001025Q001217010300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000500302Q00050003000400302Q00050005000600302Q00050007000800122Q0006000A3Q00206401060006000B00122Q000700083Q00122Q0008000C3Q00122Q000900083Q00122Q000A000C6Q0006000A000200100701050009000600102Q0005000D00024Q00030005000200122Q0004000E3Q00202Q00050001000F00062Q0005003500013Q000457012Q003500012Q0060010500023Q00204000060001000F001253010700104Q009F00085Q0020400009000100110006460009002400010001000457012Q002400012Q0060010900033Q0020400009000900122Q004F000500090002001252010600143Q00202Q00060006000B00122Q0007000C3Q00122Q000800156Q00060008000200102Q00050013000600122Q0006000A3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000E3Q00122Q000900153Q00122Q000A000C6Q0006000A000200102Q00050016000600102Q0005000D000300122Q000400174Q006E010500053Q0012530106000C3Q0020400007000100180006380107005500013Q000457012Q005500012Q0060010700023Q002040000800010018001253010900104Q009F000A5Q002040000B00010019000646000B004300010001000457012Q004300012Q0060010B00033Q002040000B000B00122Q004F0007000B00022Q001A000500073Q00122Q000700143Q00202Q00070007000B00122Q000800083Q00122Q000900156Q00070009000200102Q00050013000700122Q0007000A3Q00202Q00070007000B00122Q000800083Q00122Q0009001A3Q00122Q000A00153Q00122Q000B000C6Q0007000B000200102Q00050016000700102Q0005000D000300122Q000600174Q0060010700013Q00122F0008001B6Q00093Q000B00302Q00090007000800202Q000A0001001C00062Q000A005D00010001000457012Q005D0001001253010A001D3Q00102C01090003000A2Q00C4000A00043Q00102Q0009001E000A4Q000A00033Q00202Q000A000A000300102Q0009001F000A00302Q00090020002100122Q000A00233Q00202Q000A000A002200202Q000A000A002400102Q00090022000A001283000A00233Q002061010A000A002500202Q000A000A002600102Q00090025000A00122Q000A00143Q00202Q000A000A000B00122Q000B000C3Q00122Q000C00156Q000A000C000200102Q00090013000A00122Q000A000A3Q002040000A000A000B001299000B000C6Q000C00043Q00122Q000D00153Q00122Q000E000C6Q000A000E000200102Q00090016000A00122Q000A000A3Q00202Q000A000A000B00122Q000B00086Q000C000400062Q0082010C000C3Q0012E9000D00083Q00122Q000E000C6Q000A000E000200102Q00090009000A00102Q0009000D00034Q00070009000200202Q00080003002700202Q00080008002800062F010A3Q000100042Q0060012Q00054Q0055012Q00024Q0060012Q00064Q0060012Q00034Q002B0108000A000100204000080003002900201201080008002800062F010A0001000100042Q0060012Q00054Q0055012Q00024Q0060012Q00064Q0060012Q00034Q002B0108000A000100204000080003002A00201201080008002800062F010A0002000100052Q0060012Q00054Q0055012Q00024Q0060012Q00064Q0060012Q00034Q0055012Q00014Q002B0108000A00012Q000801083Q000200102C0108002B000200062F01090003000100012Q0055012Q00053Q00102C0108002C00092Q0079000800024Q00EF3Q00013Q00043Q00023Q0003103Q004261636B67726F756E64436F6C6F7233030C3Q00456C656D656E74486F76657200094Q0060017Q00A22Q0100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q00EF3Q00017Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q0060017Q00A22Q0100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q00EF3Q00017Q00073Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E7403043Q007461736B03053Q0064656C6179029A5Q99B93F03083Q0043612Q6C6261636B03053Q00737061776E001B4Q00589Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q0003000100124Q00033Q00206Q000400122Q000100053Q00062F01023Q000100042Q0060017Q0060012Q00014Q0060012Q00024Q0060012Q00034Q002B012Q000200012Q0060012Q00043Q0020405Q0006000638012Q001A00013Q000457012Q001A00010012833Q00033Q0020405Q00072Q00602Q0100043Q0020400001000100062Q00A8012Q000200012Q00EF3Q00013Q00013Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q0060017Q00A22Q0100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q00EF3Q00017Q00013Q0003043Q005465787402064Q006001025Q0006380102000500013Q000457012Q000500012Q006001025Q00102C0102000100012Q00EF3Q00017Q00353Q0003043Q0054657874025Q00805240026Q003A4003153Q0046696E6446697273744368696C644F66436C612Q7303093Q00554950612Q64696E67030D3Q0050612Q64696E67426F2Q746F6D03043Q005544696D2Q033Q006E6577028Q00026Q004640030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E6461727903163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00F03F03083Q00506F736974696F6E03053Q005544696D32026Q0028C0026Q0024C003043Q0053697A65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578026Q00244003063Q00506172656E74026Q002A4003063Q005374726F6B65026Q33E33F03043Q0049636F6E03063Q00636972636C6503073Q0053756254657874026Q002640026Q00E03F026Q0020C003093Q00546578744C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q003EC0030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E636502B43Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100023Q0020400002000100010006380102000A00013Q000457012Q000A0001001253010300023Q0006460003000B00010001000457012Q000B0001001253010300034Q006001045Q002012010400040004001253010600054Q004F0004000600020006380104001700013Q000457012Q00170001001283000500073Q002040000500050008001253010600093Q0012530107000A4Q004F00050007000200102C0104000600052Q0060010500013Q0012530106000B4Q000801073Q000900302700070001000C0030270007000D000E2Q0060010800023Q00204000080008001000102C0107000F0008003027000700110012001213010800143Q00202Q00080008000800122Q000900153Q00122Q000A00156Q0008000A000200102Q00070013000800122Q000800173Q00206401080008000800122Q000900153Q00122Q000A00183Q00122Q000B00153Q00122Q000C00196Q0008000C000200103A01070016000800122Q000800173Q00202Q00080008001B4Q000900033Q00122Q000A00036Q0008000A000200102Q0007001A000800302Q0007001C001D4Q000800033Q00102Q0007001E00082Q004F0005000700022Q0069000600046Q000700053Q00122Q0008001F6Q0006000800014Q000600056Q000700056Q000800023Q00202Q00080008002000122Q000900216Q0006000900022Q0060010700063Q0020400008000100220006460008004700010001000457012Q00470001001253010800233Q0012530109001F4Q00B7000A8Q000B00023Q00202Q000B000B00244Q0007000B000200302Q0007001C002500102Q0007001E00054Q000800083Q00062Q0002008900013Q000457012Q00890001001283000900143Q00209C00090009000800122Q000A00153Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00153Q00122Q000B00273Q00122Q000C00263Q00122Q000D00096Q0009000D000200102Q0007001600094Q000900013Q00122Q000A00286Q000B3Q000B00302Q000B0011001500102Q000B000100024Q000C00073Q00102Q000B0029000C4Q000C00023Q00202Q000C000C002400102Q000B002A000C00302Q000B002B002C00122Q000C002E3Q00202Q000C000C002D00202Q000C000C002F00102Q000B002D000C00122Q000C00143Q00202Q000C000C000800122Q000D00093Q00122Q000E00266Q000C000E000200102Q000B0013000C00122Q000C00173Q00202Q000C000C000800122Q000D00093Q00122Q000E00253Q00122Q000F00263Q00122Q001000096Q000C0010000200102Q000B0016000C00122Q000C00173Q00202Q000C000C000800122Q000D00153Q00122Q000E00303Q00122Q000F00153Q00122Q001000096Q000C0010000200102Q000B001A000C00302Q000B001C002500102Q000B001E00054Q0009000B00024Q000800093Q00044Q00970001001283000900143Q00203901090009000800122Q000A00263Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00206401090009000800122Q000A00263Q00122Q000B00093Q00122Q000C00263Q00122Q000D00096Q0009000D000200102C01070016000900204000090005003100201201090009003200062F010B3Q000100062Q0060012Q00084Q0055012Q00074Q0060012Q00094Q0060012Q00024Q0055012Q00064Q0055012Q00084Q002B0109000B000100204000090005003300201201090009003200062F010B0001000100062Q0060012Q00084Q0055012Q00074Q0060012Q00094Q0060012Q00024Q0055012Q00064Q0055012Q00084Q002B0109000B000100204000090005003400201201090009003200062F010B0002000100012Q0055012Q00014Q00260109000B00014Q00093Q000100102Q0009003500054Q000900028Q00013Q00033Q00063Q00030A3Q0054657874436F6C6F723303063Q00412Q63656E7403053Q00436F6C6F72030C3Q005472616E73706172656E6379029A5Q99C93F03043Q0054657874001D4Q0014019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030003000400302Q0003000400056Q000300016Q00053Q00064Q001C00013Q000457012Q001C00012Q0060017Q00A22Q0100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000600102Q0003000100046Q000300012Q00EF3Q00017Q00063Q00030A3Q0054657874436F6C6F723303073Q005375625465787403053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q33E33F001D4Q0014019Q000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000400102Q00030003000400302Q0003000500066Q000300016Q00053Q00064Q001C00013Q000457012Q001C00012Q0060017Q00A22Q0100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q00EF3Q00017Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q0060016Q0020405Q0001000638012Q000900013Q000457012Q000900010012833Q00023Q0020405Q00032Q00602Q015Q0020400001000100012Q00A8012Q000200012Q00EF3Q00017Q002D3Q00026Q003E4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403013Q002D03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65025Q004060C003063Q00506172656E74034Q0003063Q00412Q63656E7403053Q005269676874026Q004CC0026Q005040030A3Q005465787442752Q746F6E2Q033Q0042757903103Q004261636B67726F756E64436F6C6F7233030F3Q004175746F42752Q746F6E436F6C6F720100026Q0018C0030A3Q0066726F6D4F2Q66736574026Q004640026Q003440026Q00104003093Q0041637469766174656403073Q00436F2Q6E65637403083Q00496E7374616E63652Q033Q0053657402913Q0006460001000400010001000457012Q000400012Q000801026Q00552Q0100024Q006001025Q001253010300014Q00200002000200022Q0060010300013Q001253010400024Q000801053Q000B0030270005000300040030270005000500062Q00C4000600023Q00102Q0005000700064Q000600033Q00202Q00060006000500102Q00050008000600302Q00050009000A00122Q0006000C3Q00202Q00060006000B00202Q00060006000D00102Q0005000B00060012830006000C3Q00206101060006000E00202Q00060006000F00102Q0005000E000600122Q000600113Q00202Q00060006001200122Q000700133Q00122Q000800146Q00060008000200102Q00050010000600122Q000600163Q00206401060006001200122Q000700133Q00122Q000800173Q00122Q000900143Q00122Q000A00136Q0006000A000200102C010500150006001283000600163Q00206401060006001200122Q000700043Q00122Q000800193Q00122Q000900043Q00122Q000A00136Q0006000A000200102C01050018000600102C0105001A00022Q004F0003000500022Q0060010400013Q001253010500024Q000801063Q000A00302700060003000400302700060005001B2Q00C4000700043Q00102Q0006000700074Q000700033Q00202Q00070007001C00102Q00060008000700302Q00060009000A00122Q0007000C3Q00202Q00070007000B00202Q00070007001D00102Q0006000B0007001213010700113Q00202Q00070007001200122Q000800043Q00122Q000900146Q00070009000200102Q00060010000700122Q000700163Q00206401070007001200122Q000800043Q00122Q0009001E3Q00122Q000A00143Q00122Q000B00136Q0007000B000200102C010600150007001283000700163Q00206401070007001200122Q000800133Q00122Q0009001F3Q00122Q000A00043Q00122Q000B00136Q0007000B000200100900060018000700102Q0006001A00024Q0004000600024Q000500013Q00122Q000600206Q00073Q000A00302Q0007000500214Q000800043Q00102Q0007000700084Q000800033Q00202Q00080008000500102Q00070008000800302Q00070009000A4Q000800033Q00202Q00080008001C00102Q00070022000800302Q00070023002400122Q000800113Q00202Q00080008001200122Q000900043Q00122Q000A00146Q0008000A000200102Q00070010000800122Q000800163Q00202Q00080008001200122Q000900043Q00122Q000A00253Q00122Q000B00143Q00122Q000C00136Q0008000C000200102Q00070015000800122Q000800163Q00202Q00080008002600122Q000900273Q00122Q000A00286Q0008000A000200102Q00070018000800102Q0007001A00024Q0005000700024Q000600056Q000700053Q00122Q000800296Q00060008000100202Q00060005002A00202Q00060006002B00062F01083Q000100012Q0055012Q00014Q002B0106000800012Q000801063Q000200102C0106002C000200062F01070001000100052Q0055012Q00034Q0055012Q00044Q0060012Q00064Q0055012Q00054Q0060012Q00033Q00102C0106002D00072Q0079000600024Q00EF3Q00013Q00023Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q0060016Q0020405Q0001000638012Q000900013Q000457012Q000900010012833Q00023Q0020405Q00032Q00602Q015Q0020400001000100012Q00A8012Q000200012Q00EF3Q00017Q00113Q0003043Q0054657874034Q002Q033Q00737562027Q00C003023Q002D5303103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66030A3Q0054657874436F6C6F723303043Q00536F6C6403053Q005368696E7903063Q00436F6C6F723303073Q0066726F6D524742026Q004440026Q003E40028Q002Q033Q0042757903063Q00412Q63656E74043A4Q006001045Q00102C0104000100012Q0060010400013Q00068B0105000600010002000457012Q00060001001253010500023Q00102C0104000100052Q0095000400026Q000500016Q00040002000200202Q00040004000300122Q000600046Q00040006000200262Q0004001000010005000457012Q001000012Q00A500046Q009F000400013Q0006460003001E00010001000457012Q001E00012Q0060010500034Q006D000600043Q00202Q00060006000700102Q0005000600064Q000500036Q000600043Q00202Q00060006000100102Q0005000800064Q000500033Q00302Q000500010009000457012Q003900010006380104002F00013Q000457012Q002F00012Q0060010500034Q0060010600043Q00204000060006000A00102C0105000600062Q0060010500033Q0012830006000B3Q00201A01060006000C00122Q0007000D3Q00122Q0008000E3Q00122Q0009000F6Q00060009000200102Q0005000800062Q0060010500033Q003027000500010010000457012Q003900012Q0060010500034Q006D000600043Q00202Q00060006001100102Q0005000600064Q000500036Q000600043Q00202Q00060006000100102Q0005000800064Q000500033Q00302Q0005000100102Q00EF3Q00017Q00BF3Q00030C3Q0043726561746557696E646F7703053Q005469746C6503093Q0043726561746554616203043Q004E616D6503043Q004D61696E03043Q0049636F6E03123Q0033642D637562652D612Q726F772D6C656674030C3Q00437265617465546F2Q676C6503093Q004175746F204C69667403063Q00637572736F7203073Q0044656661756C74010003083Q0043612Q6C6261636B030B3Q005365744175746F4C696674030A3Q004175746F2050756E636803113Q0068616E642D68616C662D652Q6C69707365030C3Q005365744175746F50756E6368030A3Q004175746F2053746F6D702Q033Q006C6567030C3Q005365744175746F53746F6D70030C3Q004175746F2041697264726F70030E3Q0070617065722D616972706C616E65030E3Q005365744175746F41697264726F7003103Q004175746F2054652Q7269746F72696573030C3Q006C6F636174696F6E2D70696E03123Q005365744175746F54652Q7269746F72696573030C3Q00436F2Q6C65637461626C6573030D3Q0062752Q6C65742D666C79696E67030F3Q0057616C6B20746F2041697264726F7003103Q0053657457616C6B546F41697264726F7003103Q0057616C6B20746F207461726765742Q20030E3Q00706572736F6E2D77616C6B696E67030E3Q005365744175746F47656D57616C6B030C3Q00437265617465536C69646572030E3Q0047656D2077616C6B2073702Q65642Q033Q004D696E026Q0034402Q033Q004D6178025Q00408E4003093Q00496E6372656D656E74026Q00F03F030F3Q0053657447656D57616C6B53702Q6564030F3Q004175746F2047656D732054772Q656E030F3Q0062752Q746572666C792D77696E6773030F3Q005365744175746F47656D54772Q656E030A3Q00426C696E6B2047656D73030E3Q006C696768746E696E672D626F6C74030C3Q00536574426C696E6B47656D7303043Q00426F2Q7303063Q0073776F72647303103Q004272696E6720412Q6C20626F2Q73657303163Q0074776F2D612Q726F77732D646F776E2D616E642D7570030C3Q00536574426F2Q734272696E67030C3Q0057616C6B20746F20426F2Q73030D3Q0053657457616C6B546F426F2Q73030A3Q00547020746F20626F2Q73030F3Q005365745470546F426F2Q734B692Q6C030D3Q0043726561746553656374696F6E030C3Q00426F2Q7320747261636B657203053Q0073776F726403093Q0049636F6E436F6C6F7203063Q00507572706C6503043Q007461736B03053Q00737061776E03043Q00452Q67732Q033Q00652Q6703173Q004175746F2068617463682073656C656374656420652Q6703173Q005365744175746F486174636853656C6563746564452Q6703103Q0053656C65637420452Q672028312D3529026Q00144003133Q0053657453656C6563746564452Q67496E646578030A3Q004375626520776F726C64030D3Q00637562652D7665727465786573030E3Q004175746F20486174636820452Q6703133Q005365744175746F486174636843756265452Q6703043Q0053686F70030D3Q0073686F2Q70696E672D6361727403093Q004175746F2053652Q6C03123Q00636972636C652D646F2Q6C61722D7369676E030B3Q005365744175746F53652Q6C03103Q004175746F20427579205765696768747303153Q0073686F2Q70696E672D6261736B65742D636865636B03113Q005365744175746F42757957656967687473030C3Q004175746F2042757920444E412Q033Q0070696E030D3Q005365744175746F427579444E41030F3Q004175746F2042757920426F6469657303103Q00706572736F6E2D63726F756368696E6703103Q005365744175746F427579426F6469657303063Q004D61726B6574030D3Q006275696C64696E672D62616E6B030C3Q004175746F2042757920412Q6C031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F7269657303083Q004D65726368616E74030F3Q004D65726368616E74206F2Q6665727303063Q0048692Q64656E2Q0103133Q004D65726368616E742050657420536E6970657203113Q005365744D65726368616E74536E69706572030D3Q0043726561746554657874626F7803093Q004D6178207072696365030B3Q00506C616365686F6C64657203153Q00652E672E20354D2028626C616E6B203D20616E792903133Q005365744D65726368616E744D61785072696365030B3Q004372656174654C6162656C03063Q0054617267657403053Q0056616C756503043Q004E6F6E6503063Q00537461747573034Q00030A3Q005065747320666F756E64030E3Q004F70656E2050657420496E646578025Q00408F40030A3Q0053656172636820706574030B3Q00706574206E616D653Q2E030C3Q0043726561746542752Q746F6E03103Q005265667265736820706574206C69737403183Q00726F746174652D636F756E7465722D636C6F636B7769736503113Q00436C65617220612Q6C207461726765747303013Q007803273Q005069636B20796F75722070657473202874617020746F2073656C6563742F756E73656C6563742903093Q00526967687449636F6E03103Q00612Q726F772D6C617267652D646F776E03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203123Q00437265617465436F726E657242752Q746F6E03113Q00612Q726F772D6C617267652D726967687403043Q005465787403063Q004F2Q6665727303103Q004261636B20746F204D65726368616E7403103Q00612Q726F772D6C617267652D6C65667403063Q0069706169727303053Q00546965723103053Q00546965723203053Q005469657233030C3Q00437265617465427579526F7703043Q004375626503043Q006375626503153Q0055736520617420796F7572206F776E207269736B2103123Q00637562652D7175657374696F6E2D6D61726B030A3Q004175746F204375626573030C3Q005365744175746F4375626573030B3Q0054772Q656E2053702Q6564026Q006940026Q004940030C3Q005365744375626553702Q6564030D3Q0057616C6B20746F204375626573030E3Q0053657457616C6B546F437562657303043Q004D69736303103Q0063726F2Q7368616972732D736C61736803143Q0044697361626C652033442052656E646572696E6703103Q00636F6D70757465722D6D6F6E69746F72030C3Q0053657444697361626C653344030B3Q004175746F2052656A6F696E03133Q00612Q726F772D6375726C2D746F2D7269676874030D3Q005365744175746F52656A6F696E030E3Q004175746F2052656A6F696E205632030F3Q005365744175746F52656A6F696E563203103Q00437573746F6D2057616C6B53702Q656403063Q00706572736F6E03123Q0053657457616C6B53702Q6564546F2Q676C65030F3Q0057616C6B53702Q65642056616C7565025Q0070974003113Q0053657457616C6B53702Q656456616C756503103Q00437573746F6D204A756D70506F776572030E3Q00706572736F6E2D66612Q6C696E6703123Q005365744A756D70506F776572546F2Q676C65030F3Q004A756D70506F7765722056616C7565025Q00407F4003113Q005365744A756D70506F77657256616C7565030D3Q00496E66696E697465204A756D70030F3Q00536574496E66696E6974654A756D7003053Q005374617473030A3Q00636861696E2D6C696E6B2Q033Q00465053031A3Q0063686576726F6E2D6C617267652D636F6E64656E7365642D7570030A3Q004C6967687447722Q656E03013Q0030030C3Q004E6574776F726B2050696E6703103Q00776966692D7369676E616C2D6661697203043Q0030206D73030C3Q00456C61707365642054696D6503053Q00636C6F636B03083Q002Q303A2Q303A2Q30030A3Q0047656D73202F204D696E03193Q00636C6F636B2D7370696E2D726576657273652D646173686564030B3Q0047656D73204561726E656403073Q006469616D6F6E64030B3Q0052657365742053746174730085023Q00617Q00206Q00014Q00023Q00014Q000300013Q00102Q0002000200036Q0002000200202Q00013Q00034Q00033Q000200302Q00030004000500302Q0003000600072Q002D2Q010003000200202Q0002000100084Q00043Q000400302Q00040004000900302Q00040006000A00302Q0004000B000C4Q000500023Q00202Q00050005000E00103D0104000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004000F00302Q0004000600100030270004000B000C2Q0060010500023Q00204000050005001100103D0104000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001200302Q0004000600130030270004000B000C2Q0060010500023Q00204000050005001400103D0104000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001500302Q0004000600160030270004000B000C2Q0060010500023Q00204000050005001700103D0104000D00054Q00020004000100202Q0002000100084Q00043Q000400302Q00040004001800302Q0004000600190030270004000B000C2Q0060010500023Q00204000050005001A00103D0104000D00054Q00020004000100202Q00023Q00034Q00043Q000200302Q00040004001B00302Q00040006001C2Q002D01020004000200202Q0003000200084Q00053Q000400302Q00050004001D00302Q00050006001600302Q0005000B000C4Q000600023Q00202Q00060006001E00103D0105000D00064Q00030005000100202Q0003000200084Q00053Q000400302Q00050004001F00302Q0005000600200030270005000B000C2Q0060010600023Q00204000060006002100103D0105000D00064Q00030005000100202Q0003000200224Q00053Q000600302Q00050004002300302Q00050024002500301B01050026002700302Q0005000B002500302Q0005002800294Q000600023Q00202Q00060006002A00103D0105000D00064Q00030005000100202Q0003000200084Q00053Q000400302Q00050004002B00302Q00050006002C0030270005000B000C2Q0060010600023Q00204000060006002D00103D0105000D00064Q00030005000100202Q0003000200084Q00053Q000400302Q00050004002E00302Q00050006002F0030270005000B000C2Q0060010600023Q00204000060006003000103D0105000D00064Q00030005000100202Q00033Q00034Q00053Q000200302Q00050004003100302Q0005000600322Q002D01030005000200202Q0004000300084Q00063Q000400302Q00060004003300302Q00060006003400302Q0006000B000C4Q000700023Q00202Q00070007003500103D0106000D00074Q00040006000100202Q0004000300084Q00063Q000400302Q00060004003600302Q0006000600340030270006000B000C2Q0060010700023Q00204000070007003700103D0106000D00074Q00040006000100202Q0004000300084Q00063Q000400302Q00060004003800302Q0006000600340030270006000B000C2Q0060010700023Q00204000070007003900103D0106000D00074Q00040006000100202Q00040003003A4Q00063Q000300302Q00060004003B00302Q00060006003C2Q00B6000700033Q00202Q00070007003E00102Q0006003D00074Q0004000600014Q00045Q00122Q0005003F3Q00202Q00050005004000062F01063Q000100032Q0060012Q00044Q0055012Q00044Q0055012Q00034Q000400050002000100202Q00053Q00034Q00073Q000200302Q00070004004100302Q0007000600424Q00050007000200202Q0006000500084Q00083Q000400302Q00080004004300302Q00080006004200302Q0008000B000C4Q000900023Q00202Q00090009004400102Q0008000D00094Q00060008000100202Q0006000500224Q00083Q000600302Q00080004004500302Q00080024002900302Q00080026004600302Q0008000B002900302Q0008002800294Q000900023Q00202Q00090009004700102Q0008000D00094Q00060008000100202Q00060005003A4Q00083Q000200302Q00080004004800302Q0008000600494Q00060008000100202Q0006000500084Q00083Q000400302Q00080004004A00302Q00080006004200302Q0008000B000C4Q000900023Q00202Q00090009004B00102Q0008000D00094Q00060008000100202Q00063Q00034Q00083Q000200302Q00080004004C00302Q00080006004D4Q00060008000200202Q0007000600084Q00093Q000400302Q00090004004E00302Q00090006004F00302Q0009000B000C4Q000A00023Q00202Q000A000A005000102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005100302Q00090006005200302Q0009000B000C4Q000A00023Q00202Q000A000A005300102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005400302Q00090006005500302Q0009000B000C4Q000A00023Q00202Q000A000A005600102Q0009000D000A4Q00070009000100202Q0007000600084Q00093Q000400302Q00090004005700302Q00090006005800302Q0009000B000C4Q000A00023Q00202Q000A000A005900102Q0009000D000A2Q000A00070009000100202Q00073Q00034Q00093Q000200302Q00090004005A00302Q00090006005B4Q00070009000200202Q0008000700084Q000A3Q000400302Q000A0004005C00302Q000A00060052003027000A000B000C2Q0060010B00023Q002040000B000B005D00103D010A000D000B4Q0008000A000100202Q0008000700084Q000A3Q000400302Q000A0004004E00302Q000A0006004F003027000A000B000C2Q0060010B00023Q002040000B000B005E00103D010A000D000B4Q0008000A000100202Q0008000700084Q000A3Q000400302Q000A0004001800302Q000A00060019003027000A000B000C2Q0060010B00023Q002040000B000B005F00103D010A000D000B4Q0008000A000100202Q00083Q00034Q000A3Q000200302Q000A0004006000302Q000A000600522Q000B0008000A000200202Q00093Q00034Q000B3Q000200302Q000B0004006100302Q000B006200634Q0009000B000200202Q000A000800084Q000C3Q000400302Q000C0004006400302Q000C00060052003027000C000B000C2Q0060010D00023Q002040000D000D006500103D010C000D000D4Q000A000C000100202Q000A000800664Q000C3Q000300302Q000C0004006700302Q000C006800692Q0060010D00023Q002040000D000D006A00103D010C000D000D4Q000A000C000100202Q000A0008006B4Q000C3Q000200302Q000C0004006C00302Q000C006D006E2Q000B000A000C000200202Q000B0008006B4Q000D3Q000200302Q000D0004006F00302Q000D006D00704Q000B000D000200202Q000C0008006B4Q000E3Q000300302Q000E0004007100302Q000E00060042003027000E006D00722Q004F000C000E0002001283000D003F3Q002040000D000D004000062F010E0001000100032Q0060012Q00044Q0060012Q00054Q0055012Q000C4Q00E0000D000200014Q000D8Q000E5Q00122Q000F00736Q001000113Q00062F01120002000100052Q0055012Q000A4Q0060012Q00054Q0055012Q000D4Q0060012Q00034Q0060012Q00063Q00062F01130003000100032Q0055012Q00104Q0055012Q000D4Q0055012Q000E3Q00062F01140004000100032Q0055012Q000E4Q0055012Q00114Q0055012Q00133Q0020120115000800662Q000801173Q000300302700170004007400302700170068007500062F01180005000100022Q0055012Q00144Q0055012Q00133Q00102C0117000D00182Q004F0015001700022Q0055011000153Q00062F01150006000100082Q0055012Q000D4Q0060012Q00054Q0055012Q000B4Q0055012Q00084Q0060012Q00024Q0055012Q00124Q0055012Q000F4Q0055012Q00133Q0020B10016000800764Q00183Q000300302Q00180004007700302Q00180006007800102Q0018000D00154Q00160018000100202Q0016000800764Q00183Q000300302Q00180004007900302Q00180006007A00062F01190007000100042Q0060012Q00024Q0055012Q000B4Q0060012Q00054Q0055012Q00123Q00103D0118000D00194Q00160018000100202Q0016000800764Q00183Q000500302Q00180004007B00302Q0018000600422Q0060011900033Q00204000190019003E00102C0118003D00190030270018007C007D00062F01190008000100022Q0055012Q00144Q0055012Q000E3Q00100D0118000D00194Q0016001800024Q001100163Q00202Q00160011007E00202Q0017000F002900102Q0016007F00174Q001600146Q00178Q0016000200014Q001600154Q009A0116000100010020120116000800802Q000801183Q000300302700180006008100302700180082008300062F01190009000100012Q0055012Q00093Q00103D0118000D00194Q00160018000100202Q0016000900764Q00183Q000300302Q00180004008400302Q00180006008500062F0119000A000100012Q0055012Q00083Q0010810018000D00194Q00160018000100202Q00160009006B4Q00183Q000200302Q00180004006F00302Q0018006D00704Q00160018000200202Q00170009003A4Q00193Q000300302Q00190004006100302Q0019000600524Q001A00033Q00202Q001A001A003E00102Q0019003D001A4Q0017001900014Q00178Q00185Q00122Q001900866Q001A00033Q00122Q001B00873Q00122Q001C00883Q00122Q001D00896Q001A000300012Q00E100190002001B000457012Q00BC2Q01002012011E0009008A2Q000801203Q000100062F0121000B000100042Q0055012Q00184Q0055012Q001D4Q0060012Q00054Q0055012Q00163Q00102C0120000D00212Q004F001E002000022Q00550017001D001E2Q0084001C5Q000663011900B12Q010002000457012Q00B12Q010012830019003F3Q00204000190019004000062F011A000C000100042Q0060012Q00044Q0060012Q00054Q0055012Q00174Q0055012Q00184Q004501190002000100202Q00193Q00034Q001B3Q000200302Q001B0004008B00302Q001B0006008C4Q0019001B000200202Q001A0019003A4Q001C3Q000300302Q001C0004008D00302Q001C0006008E2Q0060011D00033Q002040001D001D003E00103D011C003D001D4Q001A001C000100202Q001A001900084Q001C3Q000400302Q001C0004008F00302Q001C00060007003027001C000B000C2Q0060011D00023Q002040001D001D009000103D011C000D001D4Q001A001C000100202Q001A001900224Q001C3Q000600302Q001C0004009100302Q001C0024002500301B011C0026009200302Q001C000B009300302Q001C002800294Q001D00023Q00202Q001D001D009400103D011C000D001D4Q001A001C000100202Q001A001900084Q001C3Q000400302Q001C0004009500302Q001C00060020003027001C000B000C2Q0060011D00023Q002040001D001D009600103D011C000D001D4Q001A001C000100202Q001A3Q00034Q001C3Q000200302Q001C0004009700302Q001C000600982Q002D011A001C000200202Q001B001A00084Q001D3Q000400302Q001D0004009900302Q001D0006009A00302Q001D000B000C4Q001E00023Q00202Q001E001E009B00103D011D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D0004009C00302Q001D0006009D003027001D000B000C2Q0060011E00023Q002040001E001E009E00103D011D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D0004009F00302Q001D0006009D003027001D000B000C2Q0060011E00023Q002040001E001E00A000103D011D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D000400A100302Q001D000600A2003027001D000B000C2Q0060011E00023Q002040001E001E00A300103D011D000D001E4Q001B001D000100202Q001B001A00224Q001D3Q000600302Q001D000400A400302Q001D0024002500301B011D002600A500302Q001D000B002500302Q001D002800294Q001E00023Q00202Q001E001E00A600103D011D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000400302Q001D000400A700302Q001D000600A8003027001D000B000C2Q0060011E00023Q002040001E001E00A900103D011D000D001E4Q001B001D000100202Q001B001A00224Q001D3Q000600302Q001D000400AA00302Q001D0024009300301B011D002600AB00302Q001D000B009300302Q001D002800294Q001E00023Q00202Q001E001E00AC00103D011D000D001E4Q001B001D000100202Q001B001A00084Q001D3Q000300302Q001D000400AD00302Q001D000B000C2Q0060011E00023Q002040001E001E00AE00103D011D000D001E4Q001B001D000100202Q001B3Q00034Q001D3Q000200302Q001D000400AF00302Q001D000600B02Q00C5001B001D000200202Q001C001B006B4Q001E3Q000400302Q001E000400B100302Q001E000600B24Q001F00033Q00202Q001F001F00B300102Q001E003D001F00302Q001E006D00B44Q001C001E0002002012011D001B006B2Q0008011F3Q0003003027001F000400B5003027001F000600B6003027001F006D00B72Q0087011D001F000200202Q001E001B006B4Q00203Q000300302Q0020000400B800302Q0020000600B90030270020006D00BA2Q0087011E0020000200202Q001F001B006B4Q00213Q000300302Q0021000400BB00302Q0021000600BC0030270021006D00B42Q0087011F0021000200202Q0020001B006B4Q00223Q000300302Q0022000400BD00302Q0022000600BE0030270022006D00B42Q008701200022000200202Q0021001B00764Q00233Q000300302Q0023000400BF00302Q00230006007800062F0124000D000100062Q0060012Q00074Q0060012Q00084Q0060012Q00094Q0055012Q001E4Q0055012Q00204Q0055012Q001F3Q00102C0123000D00242Q002B0121002300010012830021003F3Q00204000210021004000062F0122000E0001000D2Q0060012Q00044Q0055012Q001C4Q0060012Q000A4Q0060012Q000B4Q0055012Q001D4Q0060012Q00074Q0055012Q001E4Q0060012Q000C4Q0060012Q00094Q0060012Q00084Q0055012Q00204Q0060012Q000D4Q0055012Q001F4Q00A80121000200012Q00EF3Q00013Q000F3Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03083Q00486974734C65667403073Q004D61784869747303043Q004E616D652Q0103053Q0056616C7565030B3Q004372656174654C6162656C03083Q00746F737472696E672Q033Q00202F202Q033Q0053657403053Q00706169727303083Q00496E7374616E636503073Q0044657374726F792Q00614Q0060016Q0020405Q0001000638012Q006000013Q000457012Q006000010012833Q00023Q002000014Q000300122Q000100048Q0002000100124Q00053Q00206Q000600122Q000200078Q000200024Q00015Q00064Q004E00013Q000457012Q004E0001001283000200083Q00201201033Q00092Q00B2000300044Q002800023Q0004000457012Q004C000100201201070006000A0012530109000B4Q004F0007000900020006380107004C00013Q000457012Q004C000100201201070006000600122D0009000C6Q00070009000200202Q00080006000600122Q000A000D6Q0008000A000200062Q0007004C00013Q000457012Q004C00010006380108004C00013Q000457012Q004C000100204000090006000E00206C2Q010009000F00202Q00090007001000202Q000A000800104Q000B00013Q00202Q000C0006000E4Q000B000B000C00062Q000B003F00010001000457012Q003F00012Q0060010B00013Q002040000C0006000E2Q0060010D00023Q002012010D000D00112Q0008010F3Q000200204000100006000E00102C010F000E0010001283001000124Q0055011100094Q0020001000020002001253011100133Q001283001200124Q00970113000A6Q0012000200024Q00100010001200102Q000F001000104Q000D000F00024Q000B000C000D00044Q004C00012Q0060010B00013Q002040000C0006000E2Q008D000B000B000C002012010B000B0014001283000D00124Q0055010E00094Q0020000D00020002001253010E00133Q001283000F00124Q00550110000A4Q0020000F000200022Q0031010D000D000F2Q002B010B000D00010006630102001400010002000457012Q00140001001283000200154Q0060010300014Q00E1000200020004000457012Q005D00012Q008D0007000100050006460007005D00010001000457012Q005D00010020400007000600160006380107005B00013Q000457012Q005B00010020400007000600160020120107000700172Q00A80107000200012Q0060010700013Q0020270107000500180006630102005200010002000457012Q00520001000457014Q00012Q00EF3Q00017Q00083Q0003053Q00416C697665030D3Q00676574496E646578436F756E742Q033Q005365742Q033Q00202F2003143Q004F70656E2050657420496E64657820666972737403043Q007461736B03043Q0077616974027Q004000194Q0060016Q0020405Q0001000638012Q001800013Q000457012Q001800012Q0060012Q00013Q0020405Q00022Q00A4012Q000100012Q0060010200023Q002012010200020003000638012Q001100013Q000457012Q001100012Q005501045Q001253010500044Q0055010600014Q00310104000400060006460004001200010001000457012Q00120001001253010400054Q002B01020004000100120F010200063Q00202Q00020002000700122Q000300086Q00020002000100046Q00012Q00EF3Q00017Q00123Q002Q033Q00536574030A3Q007461726765745465787403063Q00697061697273030A3Q006973546172676574656403043Q006E616D652Q033Q00726F7703153Q0046696E6446697273744368696C644F66436C612Q7303083Q0055495374726F6B6503053Q00436F6C6F7203063Q00412Q63656E7403063Q005374726F6B65030C3Q005472616E73706172656E6379029A5Q99B93F03163Q0046696E6446697273744368696C64576869636849734103093Q00546578744C6162656C030A3Q0054657874436F6C6F7233030A3Q004C6967687447722Q656E03043Q005465787400374Q0060016Q002012014Q00012Q0060010200013Q0020400002000200022Q000E010200014Q0092014Q00010012833Q00034Q00602Q0100024Q00E13Q00020002000457012Q003400012Q0060010500013Q0020D500050005000400202Q0006000400054Q00050002000200202Q00060004000600202Q00060006000700122Q000800086Q00060008000200062Q0006002400013Q000457012Q002400010006380105001A00013Q000457012Q001A00012Q0060010700033Q00204000070007000A0006460007001C00010001000457012Q001C00012Q0060010700033Q00204000070007000B00102C0106000900070006380105002200013Q000457012Q002200010012530107000D3Q0006460007002300010001000457012Q002300012Q0060010700043Q00102C0106000C00070020400007000400060020CE00070007000E00122Q0009000F6Q000A00016Q0007000A000200062Q0007003400013Q000457012Q003400010006380105003100013Q000457012Q003100012Q0060010800033Q0020400008000800110006460008003300010001000457012Q003300012Q0060010800033Q00204000080008001200102C010700100008000663012Q000A00010002000457012Q000A00012Q00EF3Q00017Q000B3Q0003063Q00737472696E6703053Q006C6F7765722Q033Q00476574034Q0003063Q006970616972732Q033Q00726F7703073Q0056697369626C6503043Q0066696E6403043Q006E616D65026Q00F03F2Q00283Q0012833Q00013Q0020405Q00022Q00602Q015Q0006382Q01000A00013Q000457012Q000A00012Q00602Q015Q0020400001000100032Q00020001000100020006460001000B00010001000457012Q000B00010012532Q0100044Q00203Q00020002001283000100054Q0060010200014Q00E1000100020003000457012Q002500010020400006000500062Q0060010700023Q0006380107002400013Q000457012Q002400010026B53Q002300010004000457012Q00230001001283000700013Q00207501070007000800122Q000800013Q00202Q00080008000200202Q0009000500094Q0008000200024Q00095Q00122Q000A000A6Q000B00016Q0007000B000200262Q000700230001000B000457012Q002300012Q00A500076Q009F000700013Q00102C0106000700070006632Q01001000010002000457012Q001000012Q00EF3Q00017Q00033Q00030C3Q00536574526967687449636F6E030E3Q00612Q726F772D6C617267652D757003103Q00612Q726F772D6C617267652D646F776E01104Q00CF8Q00602Q0100013Q0006382Q01000D00013Q000457012Q000D00012Q00602Q0100013Q0020122Q0100010001000638012Q000B00013Q000457012Q000B0001001253010300023Q0006460003000C00010001000457012Q000C0001001253010300034Q002B2Q01000300012Q00602Q0100024Q009A2Q01000100012Q00EF3Q00017Q00013Q00034Q0001093Q0026B53Q000600010001000457012Q000600012Q00602Q016Q009F000200014Q00A82Q0100020001000457012Q000800012Q00602Q0100014Q009A2Q01000100012Q00EF3Q00017Q00133Q0003063Q006970616972732Q033Q00726F7703073Q0044657374726F79030B3Q006765745065744E616D65732Q033Q0053657403153Q004F70656E2050657420496E6465782066697273742103073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564030C3Q0043726561746542752Q746F6E03043Q004E616D6503083Q0043612Q6C6261636B03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203053Q007461626C6503063Q00696E7365727403043Q006E616D6500483Q0012833Q00014Q00602Q016Q00E13Q00020002000457012Q000A00010020400005000400020006380105000A00013Q000457012Q000A00010020400005000400020020120105000500032Q00A8010500020001000663012Q000400010002000457012Q000400012Q0008017Q005E019Q002Q00013Q00206Q00046Q0001000200064Q001800010001000457012Q001800012Q00602Q0100023Q0020122Q0100010005001253010300064Q002B2Q01000300012Q00EF3Q00014Q00602Q0100023Q0020242Q010001000500122Q000300076Q00030001000200202Q0003000300084Q000300033Q00122Q000400096Q000500013Q00202Q00050005000A00122Q0006000B6Q0003000300064Q00010003000100122Q000100016Q00028Q00010002000300044Q004100012Q0060010600033Q00201201060006000C2Q000801083Q000200102C0108000D000500062F01093Q000100052Q0060012Q00044Q0055012Q00054Q0060012Q00024Q0060012Q00014Q0060012Q00053Q00107E0108000E00094Q00060008000200202Q00070006000F4Q000800066Q00080008000400102Q00070010000800122Q000700113Q00202Q0007000700124Q00088Q00093Q000200102C010900130005002040000A0006000F00102C01090002000A2Q002B0107000900012Q008400045Q0006632Q01002800010002000457012Q002800012Q00602Q0100074Q009A2Q01000100012Q00602Q0100054Q009A2Q01000100012Q00EF3Q00013Q00013Q00073Q0003143Q00546F2Q676C654D65726368616E745461726765742Q033Q0053657403073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564001A4Q0060016Q0020405Q00012Q00602Q0100014Q00E13Q00020001000638012Q001300013Q000457012Q001300012Q0060010200023Q0020F900020002000200122Q000400036Q00040001000200202Q0004000400044Q000400043Q00122Q000500056Q000600033Q00202Q00060006000600122Q000700076Q0004000400074Q00020004000100044Q001700012Q0060010200023Q0020120102000200022Q0055010400014Q002B0102000400012Q0060010200044Q009A0102000100012Q00EF3Q00017Q00053Q0003143Q00436C6561724D65726368616E74546172676574732Q033Q0053657403023Q00302F2Q033Q006D617803093Q00207461726765746564000E4Q00757Q00206Q00016Q000100016Q00013Q00206Q000200122Q000200036Q000300023Q00202Q00030003000400122Q000400056Q0002000200046Q000200016Q00038Q000100016Q00019Q003Q00054Q007D019Q000100016Q000100018Q000200016Q00017Q00013Q0003073Q005F73656C65637400044Q0060016Q0020405Q00012Q009A012Q000100012Q00EF3Q00017Q00013Q0003073Q005F73656C65637400044Q0060016Q0020405Q00012Q009A012Q000100012Q00EF3Q00017Q00093Q0003093Q0067657452656D6F746503093Q00617661696C61626C652Q033Q00536574030D3Q004E6F7420617661696C61626C6503103Q0052656D6F7465206E6F7420666F756E6403053Q007063612Q6C03073Q00426F756768742003043Q006E616D65030A3Q00427579206661696C656400284Q0028019Q000100019Q0000014Q000100023Q00202Q0001000100014Q00010001000200064Q000B00013Q000457012Q000B000100204000023Q00020006460002001000010001000457012Q001000012Q0060010200033Q002012010200020003001253010400044Q002B010200040001000457012Q002700010006460001001700010001000457012Q001700012Q0060010200033Q002012010200020003001253010400054Q002B010200040001000457012Q00270001001283000200063Q00062F01033Q000100022Q0055012Q00014Q0060012Q00014Q00200002000200022Q0060010300033Q0020120103000300030006380102002500013Q000457012Q00250001001253010500073Q00204000063Q00082Q00310105000500060006460005002600010001000457012Q00260001001253010500094Q002B0103000500012Q00EF3Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q005F7Q00206Q00014Q000200018Q000200016Q00017Q000C3Q0003053Q00416C69766503093Q006765744F2Q6665727303053Q0070616972732Q033Q0053657403043Q006E616D6503053Q00707269636503093Q00617661696C61626C6503203Q004F70656E20746865204D65726368616E7420746F206C6F6164206F2Q66657273034Q0003043Q007461736B03043Q0077616974026Q00E03F00254Q0060016Q0020405Q0001000638012Q002400013Q000457012Q002400012Q0060012Q00013Q00205B5Q00026Q0001000200122Q000100036Q000200026Q00010002000300044Q001D00010006FA0006000E00013Q000457012Q000E00012Q008D00063Q00042Q0060010700034Q00550007000400060006380106001800013Q000457012Q0018000100201201070005000400201C01090006000500202Q000A0006000600202Q000B000600074Q0007000B000100044Q001D0001002012010700050004001253010900083Q001253010A00094Q009F000B6Q002B0107000B00010006632Q01000B00010002000457012Q000B00010012830001000A3Q00204000010001000B0012530102000C4Q00A82Q0100020001000457014Q00012Q00EF3Q00017Q00063Q0003023Q006F7303043Q0074696D65028Q002Q033Q0053657403083Q002Q303A2Q303A2Q3003013Q003000153Q0012A63Q00013Q00206Q00026Q000100029Q0000124Q00038Q00019Q008Q00028Q00033Q00206Q000400122Q000200058Q000200016Q00043Q00206Q000400122Q000200068Q000200016Q00053Q00206Q000400122Q000200068Q000200016Q00017Q00153Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00F03F2Q033Q0053657403083Q00746F737472696E67028Q0003053Q007063612Q6C2Q033Q00206D7303023Q006F7303043Q0074696D6503043Q006D61746803053Q00666C2Q6F72025Q0020AC40026Q004E4003063Q00737472696E6703063Q00666F726D6174030E3Q00253032643A253032643A2530326403053Q0056616C75650003013Q0030005E4Q0060016Q0020405Q0001000638012Q005D00013Q000457012Q005D00010012833Q00023Q0020AF5Q000300122Q000100048Q000200016Q00013Q00206Q000500122Q000200066Q000300026Q000200039Q00000100124Q00073Q001283000100083Q00062F01023Q000100022Q0055017Q0060012Q00034Q009F2Q01000200014Q000100043Q00202Q00010001000500122Q000300066Q00048Q00030002000200122Q000400096Q0003000300044Q00010003000100122Q0001000A3Q00202Q00010001000B4Q0001000100024Q000200056Q00010001000200122Q0002000C3Q00202Q00020002000D00202Q00030001000E4Q00020002000200122Q0003000C3Q00202Q00030003000D00202Q00040001000E00202Q00040004000F4Q00030002000200202Q00040001000F4Q000500063Q00202Q00050005000500122Q000700103Q00202Q00070007001100122Q000800126Q000900026Q000A00036Q000B00046Q0007000B6Q00053Q00014Q000500076Q00050001000200062Q0005005B00013Q000457012Q005B00010020400006000500132Q0060010700083Q0026B50007004400010014000457012Q004400012Q0060010700084Q009D010700060007000E430107004400010007000457012Q004400012Q0060010800094Q00420008000800072Q00CF000800094Q00CF000600084Q00E30007000A3Q00202Q0007000700054Q0009000B6Q000A00096Q0009000A6Q00073Q0001000E2Q0007005700010001000457012Q005700012Q0060010700094Q00AA01070007000100202Q00070007000F4Q0008000C3Q00202Q0008000800054Q000A000B6Q000B00076Q000A000B6Q00083Q000100044Q005B00012Q00600107000C3Q002012010700070005001253010900154Q002B0107000900012Q00847Q000457014Q00012Q00EF3Q00013Q00013Q00043Q0003043Q006D61746803053Q00666C2Q6F72030E3Q004765744E6574776F726B50696E67025Q00408F4000093Q001250012Q00013Q00206Q00024Q000100013Q00202Q0001000100034Q00010002000200202Q0001000100046Q000200029Q006Q00017Q00", GetFEnv(), ...);
