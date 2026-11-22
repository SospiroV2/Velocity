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
											elseif (Enum == 1) then
												local A;
												Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												Stk[Inst[2]]();
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
												Stk[Inst[2]] = #Stk[Inst[3]];
												VIP = VIP + 1;
												Inst = Instr[VIP];
												if (Stk[Inst[2]] == Inst[4]) then
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
											end
										elseif (Enum <= 3) then
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
										elseif (Enum > 4) then
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
											Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											A = Inst[2];
											Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
									elseif (Enum <= 8) then
										if (Enum <= 6) then
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
										elseif (Enum == 7) then
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
									elseif (Enum <= 10) then
										if (Enum > 9) then
											Stk[Inst[2]]();
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
									elseif (Enum == 11) then
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
								elseif (Enum <= 18) then
									if (Enum <= 15) then
										if (Enum <= 13) then
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
										elseif (Enum == 14) then
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
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
										end
									elseif (Enum <= 16) then
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
									elseif (Enum == 17) then
										local A = Inst[2];
										do
											return Unpack(Stk, A, A + Inst[3]);
										end
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
									end
								elseif (Enum <= 21) then
									if (Enum <= 19) then
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
									elseif (Enum == 20) then
										local B;
										local A;
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
									end
								elseif (Enum <= 23) then
									if (Enum > 22) then
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
								elseif (Enum > 24) then
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
							elseif (Enum <= 38) then
								if (Enum <= 31) then
									if (Enum <= 28) then
										if (Enum <= 26) then
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
										elseif (Enum > 27) then
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
											Stk[Inst[2]][Inst[3]] = Inst[4];
											VIP = VIP + 1;
											Inst = Instr[VIP];
											Stk[Inst[2]][Inst[3]] = Inst[4];
										end
									elseif (Enum <= 29) then
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
									elseif (Enum > 30) then
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
								elseif (Enum <= 34) then
									if (Enum <= 32) then
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
									elseif (Enum > 33) then
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
								elseif (Enum <= 36) then
									if (Enum == 35) then
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
								elseif (Enum == 37) then
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
							elseif (Enum <= 45) then
								if (Enum <= 41) then
									if (Enum <= 39) then
										for Idx = Inst[2], Inst[3] do
											Stk[Idx] = nil;
										end
									elseif (Enum > 40) then
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
									else
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
									end
								elseif (Enum <= 43) then
									if (Enum == 42) then
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
								elseif (Enum == 44) then
									Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
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
									Stk[Inst[2]] = Stk[Inst[3]];
								else
									Stk[Inst[2]][Stk[Inst[3]]] = Stk[Inst[4]];
								end
							elseif (Enum <= 48) then
								if (Enum <= 46) then
									if not Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 47) then
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
									Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum <= 50) then
								if (Enum == 49) then
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
								end
							elseif (Enum == 51) then
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
								Upvalues[Inst[3]] = Stk[Inst[2]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								do
									return;
								end
							end
						elseif (Enum <= 79) then
							if (Enum <= 65) then
								if (Enum <= 58) then
									if (Enum <= 55) then
										if (Enum <= 53) then
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
										elseif (Enum > 54) then
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
										end
									elseif (Enum <= 56) then
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
									elseif (Enum == 57) then
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
										local Step;
										local Index;
										local A;
										A = Inst[2];
										Stk[A](Stk[A + 1]);
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Inst[3];
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
									end
								elseif (Enum <= 61) then
									if (Enum <= 59) then
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
									elseif (Enum > 60) then
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
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]] = Upvalues[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
									end
								elseif (Enum <= 63) then
									if (Enum > 62) then
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
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								elseif (Enum > 64) then
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
								end
							elseif (Enum <= 72) then
								if (Enum <= 68) then
									if (Enum <= 66) then
										Stk[Inst[2]] = Stk[Inst[3]] - Stk[Inst[4]];
									elseif (Enum > 67) then
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
								elseif (Enum <= 70) then
									if (Enum == 69) then
										local A = Inst[2];
										do
											return Unpack(Stk, A, Top);
										end
									elseif (Stk[Inst[2]] == Inst[4]) then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								elseif (Enum == 71) then
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
									A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
									if Stk[Inst[2]] then
										VIP = VIP + 1;
									else
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 75) then
								if (Enum <= 73) then
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
								elseif (Enum == 74) then
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
								end
							elseif (Enum <= 77) then
								if (Enum == 76) then
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
								elseif (Stk[Inst[2]] ~= Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum > 78) then
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
							end
						elseif (Enum <= 92) then
							if (Enum <= 85) then
								if (Enum <= 82) then
									if (Enum <= 80) then
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
									elseif (Enum > 81) then
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
								elseif (Enum <= 83) then
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
								elseif (Enum == 84) then
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
							elseif (Enum <= 88) then
								if (Enum <= 86) then
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
									Stk[Inst[2]][Inst[3]] = Inst[4];
								elseif (Enum > 87) then
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
									Stk[Inst[2]] = Inst[3];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Inst[3];
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
								end
							elseif (Enum <= 90) then
								if (Enum == 89) then
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
								end
							elseif (Enum > 91) then
								local A = Inst[2];
								local B = Stk[Inst[3]];
								Stk[A + 1] = B;
								Stk[A] = B[Inst[4]];
							elseif Stk[Inst[2]] then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum <= 99) then
							if (Enum <= 95) then
								if (Enum <= 93) then
									local A;
									Stk[Inst[2]] = {};
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
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Upvalues[Inst[3]];
								elseif (Enum > 94) then
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
							elseif (Enum <= 97) then
								if (Enum > 96) then
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
							elseif (Enum > 98) then
								local B = Inst[3];
								local K = Stk[B];
								for Idx = B + 1, Inst[4] do
									K = K .. Stk[Idx];
								end
								Stk[Inst[2]] = K;
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
								Stk[Inst[2]] = Env[Inst[3]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
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
							end
						elseif (Enum <= 102) then
							if (Enum <= 100) then
								Stk[Inst[2]] = -Stk[Inst[3]];
							elseif (Enum > 101) then
								Stk[Inst[2]] = not Stk[Inst[3]];
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
						elseif (Enum <= 104) then
							if (Enum == 103) then
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
						elseif (Enum == 105) then
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
						end
					elseif (Enum <= 160) then
						if (Enum <= 133) then
							if (Enum <= 119) then
								if (Enum <= 112) then
									if (Enum <= 109) then
										if (Enum <= 107) then
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
										elseif (Enum > 108) then
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
										elseif (Stk[Inst[2]] <= Stk[Inst[4]]) then
											VIP = VIP + 1;
										else
											VIP = Inst[3];
										end
									elseif (Enum <= 110) then
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
									elseif (Enum > 111) then
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
										local B;
										local A;
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
									end
								elseif (Enum <= 115) then
									if (Enum <= 113) then
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
									elseif (Enum == 114) then
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
									end
								elseif (Enum <= 117) then
									if (Enum > 116) then
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
								elseif (Enum == 118) then
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
							elseif (Enum <= 126) then
								if (Enum <= 122) then
									if (Enum <= 120) then
										local A = Inst[2];
										local T = Stk[A];
										for Idx = A + 1, Inst[3] do
											Insert(T, Stk[Idx]);
										end
									elseif (Enum == 121) then
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
								elseif (Enum <= 124) then
									if (Enum > 123) then
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
								elseif (Enum == 125) then
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
										if (Mvm[1] == 194) then
											Indexes[Idx - 1] = {Stk,Mvm[3]};
										else
											Indexes[Idx - 1] = {Upvalues,Mvm[3]};
										end
										Lupvals[#Lupvals + 1] = Indexes;
									end
									Stk[Inst[2]] = Wrap(NewProto, NewUvals, Env);
								end
							elseif (Enum <= 129) then
								if (Enum <= 127) then
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
								elseif (Enum > 128) then
									Stk[Inst[2]][Stk[Inst[3]]] = Inst[4];
								elseif (Stk[Inst[2]] < Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							elseif (Enum <= 131) then
								if (Enum > 130) then
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
							elseif (Enum > 132) then
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
						elseif (Enum <= 146) then
							if (Enum <= 139) then
								if (Enum <= 136) then
									if (Enum <= 134) then
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
									elseif (Enum == 135) then
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
								elseif (Enum <= 137) then
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
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
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
								elseif (Enum == 138) then
									if (Stk[Inst[2]] < Inst[4]) then
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
								end
							elseif (Enum <= 142) then
								if (Enum <= 140) then
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
								elseif (Enum > 141) then
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
								end
							elseif (Enum <= 144) then
								if (Enum == 143) then
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
							elseif (Enum == 145) then
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
							else
								Stk[Inst[2]] = #Stk[Inst[3]];
							end
						elseif (Enum <= 153) then
							if (Enum <= 149) then
								if (Enum <= 147) then
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
								elseif (Enum == 148) then
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
								end
							elseif (Enum <= 151) then
								if (Enum == 150) then
									local Edx;
									local Results;
									local A;
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
							elseif (Enum > 152) then
								Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							else
								Stk[Inst[2]] = Stk[Inst[3]] - Inst[4];
							end
						elseif (Enum <= 156) then
							if (Enum <= 154) then
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
							elseif (Enum == 155) then
								if (Inst[2] <= Stk[Inst[4]]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
								local A;
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
						elseif (Enum <= 158) then
							if (Enum > 157) then
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
							else
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
							end
						elseif (Enum == 159) then
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
					elseif (Enum <= 187) then
						if (Enum <= 173) then
							if (Enum <= 166) then
								if (Enum <= 163) then
									if (Enum <= 161) then
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
									elseif (Enum > 162) then
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
										Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
										A = Inst[2];
										Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
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
								elseif (Enum <= 164) then
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
								elseif (Enum > 165) then
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
								end
							elseif (Enum <= 169) then
								if (Enum <= 167) then
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
								elseif (Enum == 168) then
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
									Stk[Inst[2]] = Inst[3];
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
								end
							elseif (Enum <= 171) then
								if (Enum == 170) then
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
							elseif (Enum > 172) then
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
								local A = Inst[2];
								do
									return Stk[A](Unpack(Stk, A + 1, Top));
								end
							end
						elseif (Enum <= 180) then
							if (Enum <= 176) then
								if (Enum <= 174) then
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
								elseif (Enum > 175) then
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
								if (Enum == 177) then
									local A = Inst[2];
									local Results = {Stk[A](Unpack(Stk, A + 1, Inst[3]))};
									local Edx = 0;
									for Idx = A, Inst[4] do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
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
								end
							elseif (Enum == 179) then
								local A;
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
							elseif (Enum == 182) then
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
								Stk[A] = Stk[A](Stk[A + 1]);
							end
						elseif (Enum <= 185) then
							if (Enum == 184) then
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
						elseif (Enum > 186) then
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
					elseif (Enum <= 200) then
						if (Enum <= 193) then
							if (Enum <= 190) then
								if (Enum <= 188) then
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
								elseif (Enum == 189) then
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
								end
							elseif (Enum <= 191) then
								Stk[Inst[2]] = Env[Inst[3]];
							elseif (Enum == 192) then
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
						elseif (Enum <= 196) then
							if (Enum <= 194) then
								Stk[Inst[2]] = Stk[Inst[3]];
							elseif (Enum == 195) then
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
							end
						elseif (Enum <= 198) then
							if (Enum == 197) then
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
						elseif (Enum > 199) then
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
					elseif (Enum <= 207) then
						if (Enum <= 203) then
							if (Enum <= 201) then
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
							elseif (Enum == 202) then
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
						elseif (Enum <= 205) then
							if (Enum > 204) then
								if (Stk[Inst[2]] ~= Inst[4]) then
									VIP = VIP + 1;
								else
									VIP = Inst[3];
								end
							else
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
							end
						elseif (Enum > 206) then
							Stk[Inst[2]] = Inst[3] ~= 0;
							VIP = VIP + 1;
						else
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
						end
					elseif (Enum <= 210) then
						if (Enum <= 208) then
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
						elseif (Enum > 209) then
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
					elseif (Enum <= 212) then
						if (Enum > 211) then
							local A = Inst[2];
							Stk[A] = Stk[A]();
						else
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
						end
					elseif (Enum > 213) then
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
					end
				elseif (Enum <= 321) then
					if (Enum <= 267) then
						if (Enum <= 240) then
							if (Enum <= 227) then
								if (Enum <= 220) then
									if (Enum <= 217) then
										if (Enum <= 215) then
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
										elseif (Enum > 216) then
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
											Stk[Inst[2]] = Inst[3];
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
										else
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
										end
									elseif (Enum <= 218) then
										Stk[Inst[2]] = Stk[Inst[3]] + Stk[Inst[4]];
									elseif (Enum == 219) then
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
								elseif (Enum <= 223) then
									if (Enum <= 221) then
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
									elseif (Enum > 222) then
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
										Stk[Inst[2]] = Env[Inst[3]];
									end
								elseif (Enum <= 225) then
									if (Enum == 224) then
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
										Stk[Inst[2]] = Env[Inst[3]];
										VIP = VIP + 1;
										Inst = Instr[VIP];
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
								elseif (Enum > 226) then
									Stk[Inst[2]] = Upvalues[Inst[3]];
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
							elseif (Enum <= 233) then
								if (Enum <= 230) then
									if (Enum <= 228) then
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
									elseif (Enum > 229) then
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
								elseif (Enum <= 231) then
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
								elseif (Enum > 232) then
									Stk[Inst[2]] = Wrap(Proto[Inst[3]], nil, Env);
								else
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
								end
							elseif (Enum <= 236) then
								if (Enum <= 234) then
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
								elseif (Enum == 235) then
									local A = Inst[2];
									local Results, Limit = _R(Stk[A](Unpack(Stk, A + 1, Top)));
									Top = (Limit + A) - 1;
									local Edx = 0;
									for Idx = A, Top do
										Edx = Edx + 1;
										Stk[Idx] = Results[Edx];
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
								end
							elseif (Enum <= 238) then
								if (Enum > 237) then
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
								else
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
								end
							elseif (Enum > 239) then
								local A = Inst[2];
								local T = Stk[A];
								for Idx = A + 1, Top do
									Insert(T, Stk[Idx]);
								end
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
								VIP = VIP + 1;
								Inst = Instr[VIP];
								VIP = Inst[3];
							end
						elseif (Enum <= 253) then
							if (Enum <= 246) then
								if (Enum <= 243) then
									if (Enum <= 241) then
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
									elseif (Enum > 242) then
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
									else
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
									end
								elseif (Enum <= 244) then
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
								elseif (Enum == 245) then
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
								end
							elseif (Enum <= 249) then
								if (Enum <= 247) then
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
								elseif (Enum == 248) then
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
									local A = Inst[2];
									local B = Stk[Inst[3]];
									Stk[A + 1] = B;
									Stk[A] = B[Stk[Inst[4]]];
								end
							elseif (Enum <= 251) then
								if (Enum > 250) then
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
									local A;
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
								end
							elseif (Enum > 252) then
								Stk[Inst[2]] = Inst[3] / Stk[Inst[4]];
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
						elseif (Enum <= 260) then
							if (Enum <= 256) then
								if (Enum <= 254) then
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
								elseif (Enum > 255) then
									local A;
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
									Stk[Inst[2]] = Env[Inst[3]];
									VIP = VIP + 1;
									Inst = Instr[VIP];
									Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
								end
							elseif (Enum <= 258) then
								if (Enum == 257) then
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
									local A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
								end
							elseif (Enum > 259) then
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
							end
						elseif (Enum <= 263) then
							if (Enum <= 261) then
								Stk[Inst[2]] = Stk[Inst[3]] / Stk[Inst[4]];
							elseif (Enum > 262) then
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
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								A = Inst[2];
								Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3]));
							else
								Stk[Inst[2]] = Inst[3] ~= 0;
							end
						elseif (Enum <= 265) then
							if (Enum == 264) then
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
							else
								local A;
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
								VIP = Inst[3];
							end
						elseif (Enum == 266) then
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
					elseif (Enum <= 294) then
						if (Enum <= 280) then
							if (Enum <= 273) then
								if (Enum <= 270) then
									if (Enum <= 268) then
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
									elseif (Enum > 269) then
										local A = Inst[2];
										Stk[A](Unpack(Stk, A + 1, Top));
									else
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
									end
								elseif (Enum <= 271) then
									local A = Inst[2];
									Stk[A](Unpack(Stk, A + 1, Inst[3]));
								elseif (Enum == 272) then
									do
										return Stk[Inst[2]];
									end
								else
									local A = Inst[2];
									do
										return Stk[A](Unpack(Stk, A + 1, Inst[3]));
									end
								end
							elseif (Enum <= 276) then
								if (Enum <= 274) then
									local A = Inst[2];
									local T = Stk[A];
									local B = Inst[3];
									for Idx = 1, B do
										T[Idx] = Stk[A + Idx];
									end
								elseif (Enum == 275) then
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
							elseif (Enum <= 278) then
								if (Enum > 277) then
									Stk[Inst[2]] = Stk[Inst[3]] / Inst[4];
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
							elseif (Enum == 279) then
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
						elseif (Enum <= 287) then
							if (Enum <= 283) then
								if (Enum <= 281) then
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
								elseif (Enum > 282) then
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
								end
							elseif (Enum <= 285) then
								if (Enum > 284) then
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
									do
										return Stk[Inst[2]]();
									end
								end
							elseif (Enum == 286) then
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
						elseif (Enum <= 290) then
							if (Enum <= 288) then
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
							elseif (Enum == 289) then
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
							end
						elseif (Enum <= 292) then
							if (Enum > 291) then
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
						elseif (Enum > 293) then
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
							A = Inst[2];
							Stk[A](Unpack(Stk, A + 1, Inst[3]));
							VIP = VIP + 1;
							Inst = Instr[VIP];
							A = Inst[2];
							B = Stk[Inst[3]];
							Stk[A + 1] = B;
							Stk[A] = B[Inst[4]];
						else
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
					elseif (Enum <= 307) then
						if (Enum <= 300) then
							if (Enum <= 297) then
								if (Enum <= 295) then
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
								elseif (Enum == 296) then
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
							elseif (Enum <= 298) then
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
							elseif (Enum > 299) then
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
						elseif (Enum <= 303) then
							if (Enum <= 301) then
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
							elseif (Enum > 302) then
								local A;
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
						elseif (Enum <= 305) then
							if (Enum == 304) then
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
							end
						elseif (Enum == 306) then
							Stk[Inst[2]] = {};
						else
							Stk[Inst[2]][Inst[3]] = Inst[4];
						end
					elseif (Enum <= 314) then
						if (Enum <= 310) then
							if (Enum <= 308) then
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
							elseif (Enum == 309) then
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
							else
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
							end
						elseif (Enum <= 312) then
							if (Enum == 311) then
								local A;
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
						elseif (Enum > 313) then
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
					elseif (Enum <= 317) then
						if (Enum <= 315) then
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
							Stk[Inst[2]] = Env[Inst[3]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
							VIP = VIP + 1;
							Inst = Instr[VIP];
							Stk[Inst[2]] = Stk[Inst[3]];
						elseif (Enum > 316) then
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
					elseif (Enum <= 319) then
						if (Enum > 318) then
							local B;
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
							do
								return;
							end
						else
							local A = Inst[2];
							Stk[A](Stk[A + 1]);
						end
					elseif (Enum > 320) then
						if (Stk[Inst[2]] <= Inst[4]) then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
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
				elseif (Enum <= 375) then
					if (Enum <= 348) then
						if (Enum <= 334) then
							if (Enum <= 327) then
								if (Enum <= 324) then
									if (Enum <= 322) then
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
									elseif (Enum == 323) then
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
									else
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
									end
								elseif (Enum <= 325) then
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
								elseif (Enum > 326) then
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
									VIP = VIP + 1;
									Inst = Instr[VIP];
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
								end
							elseif (Enum <= 330) then
								if (Enum <= 328) then
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
								elseif (Enum > 329) then
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
								else
									Stk[Inst[2]] = Stk[Inst[3]] * Stk[Inst[4]];
								end
							elseif (Enum <= 332) then
								if (Enum > 331) then
									local A = Inst[2];
									Stk[A] = Stk[A](Unpack(Stk, A + 1, Top));
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
							elseif (Enum > 333) then
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
								Upvalues[Inst[3]] = Stk[Inst[2]];
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
						elseif (Enum <= 341) then
							if (Enum <= 337) then
								if (Enum <= 335) then
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
								elseif (Enum == 336) then
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
									local B = Stk[Inst[4]];
									if B then
										VIP = VIP + 1;
									else
										Stk[Inst[2]] = B;
										VIP = Inst[3];
									end
								end
							elseif (Enum <= 339) then
								if (Enum == 338) then
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
							elseif (Enum == 340) then
								local A;
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Upvalues[Inst[3]];
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
								if (Stk[Inst[2]] ~= Inst[4]) then
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
							end
						elseif (Enum <= 344) then
							if (Enum <= 342) then
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
							elseif (Enum == 343) then
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
								Stk[Inst[2]] = Inst[3];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = Inst[3];
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
						elseif (Enum <= 346) then
							if (Enum > 345) then
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
							end
						elseif (Enum == 347) then
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
						end
					elseif (Enum <= 361) then
						if (Enum <= 354) then
							if (Enum <= 351) then
								if (Enum <= 349) then
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
								elseif (Enum > 350) then
									do
										return;
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
							elseif (Enum <= 352) then
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
							elseif (Enum == 353) then
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
						elseif (Enum <= 357) then
							if (Enum <= 355) then
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
							elseif (Enum > 356) then
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
							else
								local A;
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
								VIP = VIP + 1;
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
						elseif (Enum <= 359) then
							if (Enum == 358) then
								local A = Inst[2];
								local Results, Limit = _R(Stk[A]());
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
						elseif (Enum == 360) then
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
					elseif (Enum <= 368) then
						if (Enum <= 364) then
							if (Enum <= 362) then
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
							elseif (Enum > 363) then
								local A;
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
						elseif (Enum <= 366) then
							if (Enum == 365) then
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
						elseif (Enum > 367) then
							Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];
						else
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
						end
					elseif (Enum <= 371) then
						if (Enum <= 369) then
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
						elseif (Enum > 370) then
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
							local A = Inst[2];
							local Results = {Stk[A](Unpack(Stk, A + 1, Top))};
							local Edx = 0;
							for Idx = A, Inst[4] do
								Edx = Edx + 1;
								Stk[Idx] = Results[Edx];
							end
						end
					elseif (Enum <= 373) then
						if (Enum > 372) then
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
					elseif (Enum == 374) then
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
					elseif (Stk[Inst[2]] == Stk[Inst[4]]) then
						VIP = VIP + 1;
					else
						VIP = Inst[3];
					end
				elseif (Enum <= 402) then
					if (Enum <= 388) then
						if (Enum <= 381) then
							if (Enum <= 378) then
								if (Enum <= 376) then
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
								elseif (Enum == 377) then
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
							elseif (Enum == 380) then
								Stk[Inst[2]] = Stk[Inst[3]] * Inst[4];
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
						elseif (Enum <= 384) then
							if (Enum <= 382) then
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
							elseif (Enum > 383) then
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
								local B = Stk[Inst[4]];
								if not B then
									VIP = VIP + 1;
								else
									Stk[Inst[2]] = B;
									VIP = Inst[3];
								end
							end
						elseif (Enum <= 386) then
							if (Enum == 385) then
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
						elseif (Enum > 387) then
							Upvalues[Inst[3]] = Stk[Inst[2]];
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
					elseif (Enum <= 395) then
						if (Enum <= 391) then
							if (Enum <= 389) then
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
							elseif (Enum > 390) then
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
								Stk[Inst[2]][Inst[3]] = Inst[4];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];
								VIP = VIP + 1;
								Inst = Instr[VIP];
								Stk[Inst[2]] = {};
							end
						elseif (Enum <= 393) then
							if (Enum > 392) then
								local A = Inst[2];
								do
									return Stk[A], Stk[A + 1];
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
							end
						elseif (Enum == 394) then
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
						elseif (Enum == 397) then
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
					elseif (Enum <= 400) then
						if (Enum > 399) then
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
					elseif (Enum > 401) then
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
						Stk[Inst[2]] = Stk[Inst[3]][Inst[4]];
						VIP = VIP + 1;
						Inst = Instr[VIP];
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
				elseif (Enum <= 415) then
					if (Enum <= 408) then
						if (Enum <= 405) then
							if (Enum <= 403) then
								VIP = Inst[3];
							elseif (Enum > 404) then
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
								local A = Inst[2];
								local Results, Limit = _R(Stk[A](Stk[A + 1]));
								Top = (Limit + A) - 1;
								local Edx = 0;
								for Idx = A, Top do
									Edx = Edx + 1;
									Stk[Idx] = Results[Edx];
								end
							end
						elseif (Enum <= 406) then
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
						elseif (Enum == 407) then
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
							for Idx = Inst[2], Inst[3] do
								Stk[Idx] = nil;
							end
						end
					elseif (Enum <= 411) then
						if (Enum <= 409) then
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
						elseif (Enum == 410) then
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
						else
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
						end
					elseif (Enum <= 413) then
						if (Enum == 412) then
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
							Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];
						end
					elseif (Enum > 414) then
						if (Stk[Inst[2]] > Inst[4]) then
							VIP = VIP + 1;
						else
							VIP = Inst[3];
						end
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
				elseif (Enum <= 422) then
					if (Enum <= 418) then
						if (Enum <= 416) then
							if (Inst[2] < Stk[Inst[4]]) then
								VIP = VIP + 1;
							else
								VIP = Inst[3];
							end
						elseif (Enum > 417) then
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
					elseif (Enum <= 420) then
						if (Enum == 419) then
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
					elseif (Enum > 421) then
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
				elseif (Enum <= 425) then
					if (Enum <= 423) then
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
					elseif (Enum > 424) then
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
				elseif (Enum <= 427) then
					if (Enum > 426) then
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
					else
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
					end
				elseif (Enum > 428) then
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
					Stk[Inst[2]] = Inst[3];
					VIP = VIP + 1;
					Inst = Instr[VIP];
					Stk[Inst[2]] = Inst[3];
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
return VMCall("LOL!45012Q0003843Q00682Q7470733A2Q2F776562682Q6F6B2E6C65776973616B7572612E6D6F652F6170692F776562682Q6F6B732F31352Q3335363932303938322Q333Q3935392F3369312D5072753879332Q573678686D352D39444275565871556D44544B3870646F6665706E5241582D74576B4677477A5048616C6E2Q38757363767573574B63504C7303043Q0067616D65030A3Q004765745365727669636503073Q00506C617965727303123Q004D61726B6574706C61636553657276696365030B3Q00482Q7470536572766963652Q033Q0073796E03073Q007265717565737403043Q00682Q7470030C3Q00682Q74705F72657175657374034Q00030B3Q004C6F63616C506C61796572030C3Q00556E6B6E6F776E2047616D6503053Q007063612Q6C03103Q00556E6B6E6F776E204578656375746F7203103Q006964656E746966796578656375746F72030F3Q006765746578656375746F726E616D65030E3Q004D656D626572736869705479706503043Q00456E756D03073Q005072656D69756D03083Q0059657320F09F928E03023Q004E6F030F3Q004661696C656420746F206665746368030C3Q00556E6B6E6F776E2043697479030E3Q00556E6B6E6F776E20526567696F6E030B3Q00556E6B6E6F776E20495350030D3Q004E6F742053752Q706F7274656403073Q006765746877696403063Q00656D6265647303053Q007469746C6503273Q00F09F9AA820486967682D5072696F726974792053637269707420457865637574696F6E204C6F6703053Q00636F6C6F72023Q002Q60806F4103063Q006669656C647303043Q006E616D65030D3Q00F09F91A420557365726E616D6503053Q0076616C756503043Q004E616D6503063Q00696E6C696E652Q0103143Q00F09F8FB7EFB88F20446973706C6179204E616D65030B3Q00446973706C61794E616D65030F3Q00E28FB320412Q636F756E7420416765030A3Q00412Q636F756E7441676503053Q00206461797303103Q00F09F9BA0EFB88F204578656375746F72030D3Q00F09F928E205072656D69756D3F030E3Q00F09F8EAE2047616D65204E616D6503163Q00F09F8C90205075626C696320495020412Q6472652Q7303013Q006003103Q00F09F8F99EFB88F204C6F636174696F6E03023Q002C2003113Q00F09F948C204953502050726F766964657203173Q00F09F9491204861726477617265204944202848574944290100030E3Q00F09F94972047616D65204C696E6B03323Q005B436C69636B204865726520746F204A6F696E5D28682Q7470733A2Q2F3Q772E726F626C6F782E636F6D2F67616D65732F03073Q00506C616365496403013Q002903093Q0074696D657374616D7003023Q006F7303043Q006461746503133Q002125592D256D2D25645425483A254D3A25535A03043Q007461736B03053Q00737061776E030C3Q0054772Q656E5365727669636503103Q0055736572496E70757453657276696365030A3Q0052756E5365727669636503113Q005265706C69636174656453746F72616765030B3Q005669727475616C5573657203133Q005669727475616C496E7075744D616E6167657203093Q00576F726B7370616365030F3Q0054656C65706F727453657276696365030A3Q004775695365727669636503083Q004C69676874696E6703053Q00537461747303073Q0067657467656E7603133Q0056656C6F63697479487562496E7374616E636503053Q00416C6976652Q033Q004775690003063Q00506172656E74030C3Q00736574636C6970626F617264030B3Q00746F636C6970626F617264030E3Q007772697465636C6970626F617264030F3Q0077726974655F636C6970626F61726403093Q00436C6970626F6172642Q033Q0073657403043Q0047616D6503143Q0056656C6F63697479277320437573746F6D203A20030A3Q0054772Q656E53702Q6564026Q33C33F03093Q004D696E486569676874026Q002E40027B14AE47E17A843F026Q0014C0026Q00304003093Q00546F2Q676C654B657903073Q004B6579436F646503013Q004B03083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030B3Q004175746F47656D57616C6B030A3Q0053702Q656456616C7565026Q003440030D3Q0047656D54772Q656E53702Q6564026Q004940030C3Q0057616C6B546F5475726B657903103Q005475726B657953702Q656456616C7565030F3Q004175746F4461696C7952657761726403083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E67030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E6503103Q004175746F486174636843756265452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03063Q004E6F636C6970030D3Q0043756265436F2Q6C6563746F7203073Q00456E61626C656403053Q0053702Q6564026Q004440030B3Q0057616C6B546F437562657303093Q0044697361626C653344030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030F3Q0057616C6B53702Q6564546F2Q676C65030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030E3Q004A756D70506F77657256616C7565030C3Q00496E66696E6974654A756D7003043Q0074696D65028Q00030D3Q0052656E6465725374652Q706564030E3Q00436861726163746572412Q646564030B3Q004A756D705265717565737403133Q00452Q726F724D652Q736167654368616E67656403073Q005374652Q70656403073Q00566563746F723303043Q007A65726F026Q001A40030B3Q005365744175746F4C696674030C3Q005365744175746F50756E6368030C3Q005365744175746F53746F6D70030E3Q005365744175746F41697264726F7003123Q005365744175746F54652Q7269746F7269657303103Q0053657457616C6B546F41697264726F70030E3Q005365744175746F47656D57616C6B030F3Q0053657457616C6B546F5475726B657903073Q0073652Q73696F6E03123Q005365744175746F4461696C79526577617264030F3Q0053657447656D57616C6B53702Q656403123Q005365745475726B657957616C6B53702Q656403103Q0053657447656D54772Q656E53702Q6564030F3Q005365744175746F47656D54772Q656E030C3Q00536574426F2Q734272696E67030D3Q0053657457616C6B546F426F2Q73030F3Q005365745470546F426F2Q734B692Q6C03133Q0053657453656C6563746564452Q67496E64657803173Q005365744175746F486174636853656C6563746564452Q6703123Q005365744175746F48617463685065724F6E6503133Q005365744175746F486174636843756265452Q67030B3Q005365744175746F53652Q6C03113Q005365744175746F42757957656967687473030D3Q005365744175746F427579444E4103103Q005365744175746F427579426F64696573031C3Q005365744175746F42757953757065726D61726B65745765696768747303163Q005365744175746F53652Q6C53757065726D61726B6574031D3Q005365744175746F53757065726D61726B657454652Q7269746F72696573030C3Q005365744175746F4375626573030C3Q005365744375626553702Q656403093Q00626C61636B6C697374030F3Q0067657447726F756E64466F6C646572030C3Q0067657447726F756E64546F7003093Q006D6178486569676874026Q002840030D3Q00676574466C2Q6F724375626573030E3Q0053657457616C6B546F4375626573030C3Q0053657444697361626C653344030D3Q005365744175746F52656A6F696E03043Q006275737903073Q00706C616365496403053Q006A6F62496403053Q004A6F624964030E3Q006973446973636F2Q6E6563746564030F3Q005365744175746F52656A6F696E563203123Q0053657457616C6B53702Q6564546F2Q676C6503113Q0053657457616C6B53702Q656456616C756503123Q005365744A756D70506F776572546F2Q676C6503113Q005365744A756D70506F77657256616C7565030F3Q00536574496E66696E6974654A756D70030E3Q004D65726368616E74536E6970657203123Q0053656C65637465645461726765745065747303143Q004D65726368616E744D6178507269636554657874030A3Q0070617273655072696365030B3Q006765745065744E616D6573030D3Q00676574496E646578436F756E742Q033Q006D6178026Q00244003093Q0067657452656D6F746503093Q006765744F2Q66657273030A3Q006973546172676574656403143Q00546F2Q676C654D65726368616E7454617267657403143Q00436C6561724D65726368616E7454617267657473030A3Q007461726765745465787403133Q005365744D65726368616E744D6178507269636503113Q005365744D65726368616E74536E6970657203093Q004175746F5175657374030A3Q005175657374446562756703063Q0061637469766503053Q006F776E656403073Q006F776E6564547003083Q0074704D616E75616C03023Q00756903093Q004175746F437562657303053Q00686174636803053Q00666C61677303043Q00626F2Q7303043Q007469636B030B3Q006C6966745F69736C616E64030B3Q006C6966745F6D61726B657403043Q006C69667403053Q0070756E636803053Q0073746F6D702Q033Q0067656D03043Q0073652Q6C03093Q0074652Q7269746F727903043Q006375626503083Q00636C612Q7369667903073Q006765744C697374030D3Q00706172736550726F6772652Q7303093Q0072656164436172647303043Q006F776E7303123Q0072656C656173655573657253746F2Q70656403043Q0073746F7003053Q007374617274030C3Q005365744175746F5175657374030A3Q004261636B67726F756E6403063Q00436F6C6F723303073Q0066726F6D52474203093Q005365636F6E64617279026Q003B4003073Q00456C656D656E74026Q004140030C3Q00456C656D656E74486F766572026Q0045402Q033Q004F2Q66025Q00804B4003063Q005374726F6B65025Q0060654003043Q0054657874025Q00E06F4003073Q0053756254657874025Q00E0654003073Q005761726E696E67025Q00C06740025Q0080514003063Q00412Q63656E74026Q005940026Q00644003063Q00507572706C65026Q006940030B3Q004C696768744F72616E6765025Q00406540026Q005440030A3Q004C6967687447722Q656E025Q0080614003053Q005368696E79025Q00A06B40025Q0080494003063Q0059652Q6C6F772Q033Q00526564025Q0080564003053Q0042726F776E026Q006640026Q005C40026Q004E4003523Q00726278612Q7365743A2Q2F4C75615061636B616765732F5061636B616765732F5F496E6465782F4275696C64657249636F6E732F4275696C64657249636F6E732F4275696C64657249636F6E732E6A736F6E03043Q00466F6E7403083Q0066726F6D456E756D030E3Q00536F7572636553616E73426F6C64030A3Q00536F7572636553616E7303093Q0054772Q656E496E666F2Q033Q006E6577027B14AE47E17AC43F030B3Q00456173696E675374796C6503043Q0051756164030F3Q00456173696E67446972656374696F6E2Q033Q004F7574026Q00D03F03053Q005175617274028FC2F5285C8FEA3F03073Q002Q5F696E64657803093Q005363722Q656E47756903083Q00566165687A55495F03043Q006D61746803063Q0072616E646F6D025Q00408F40024Q008087C340030E3Q005A496E6465784265686176696F7203073Q005369626C696E67030C3Q0052657365744F6E537061776E030E3Q0049676E6F7265477569496E736574030C3Q00446973706C61794F72646572025Q00388F4003073Q0044657374726F79030A3Q0044657374726F79696E6703073Q00436F2Q6E656374030C3Q0043726561746557696E646F7703083Q0054726F706963616C004E052Q0012833Q00013Q001288000100023Q00202Q00010001000300122Q000300046Q00010003000200122Q000200023Q00202Q00020002000300122Q000400056Q00020004000200122Q000300023Q00202Q000300030003001283000500064Q00020103000500020012BF000400073Q00065B0004001400013Q000493012Q001400010012BF000400073Q00209900040004000800062E0004001F00010001000493012Q001F00010012BF000400093Q00065B0004001B00013Q000493012Q001B00010012BF000400093Q00209900040004000800062E0004001F00010001000493012Q001F00010012BF0004000A3Q00062E0004001F00010001000493012Q001F00010012BF000400083Q00065B000400C100013Q000493012Q00C1000100065B3Q00C100013Q000493012Q00C100010026CD3Q00C10001000B000493012Q00C1000100209900050001000C0012830006000D3Q0012BF0007000E3Q00067E00083Q000100022Q00C23Q00064Q00C23Q00024Q003E0107000200010012830007000F3Q0012BF000800103Q00065B0008003500013Q000493012Q003500010012BF0008000E3Q00067E00090001000100012Q00C23Q00074Q003E010800020001000493012Q003C00010012BF000800113Q00065B0008003C00013Q000493012Q003C00010012BF0008000E3Q00067E00090002000100012Q00C23Q00074Q003E0108000200010020990008000500120012BF000900133Q0020990009000900120020990009000900140006770108004500010009000493012Q00450001001283000800153Q00062E0008004600010001000493012Q00460001001283000800163Q001283000900173Q001283000A00183Q001283000B00193Q001283000C001A3Q0012BF000D000E3Q00067E000E0003000100062Q00C23Q00044Q00C23Q00034Q00C23Q00094Q00C23Q000A4Q00C23Q000B4Q00C23Q000C4Q003E010D00020001001283000D001B3Q0012BF000E001C3Q00065B000E005C00013Q000493012Q005C00010012BF000E000E3Q00067E000F0004000100012Q00C23Q000D4Q003E010E00020001000493012Q006700010012BF000E00073Q00065B000E006700013Q000493012Q006700010012BF000E00073Q002099000E000E001C00065B000E006700013Q000493012Q006700010012BF000E000E3Q00067E000F0005000100012Q00C23Q000D4Q003E010E000200012Q0032010E3Q00012Q0043010F00016Q00103Q000400302Q0010001E001F00302Q0010002000214Q0011000B6Q00123Q000300302Q00120023002400202Q00130005002600102Q00120025001300302Q0012002700282Q003201133Q000300303301130023002900209900140005002A0010630113002500140030330113002700282Q003201143Q000300303301140023002B00209900150005002C0012830016002D4Q00630015001500160010630114002500150030330114002700282Q009A01153Q000300302Q00150023002E00102Q00150025000700302Q0015002700284Q00163Q000300302Q00160023002F00102Q00160025000800302Q0016002700284Q00173Q000300302Q0017002300300010630117002500060030780117002700284Q00183Q000300302Q00180023003100122Q001900326Q001A00093Q00122Q001B00326Q00190019001B00102Q00180025001900302Q0018002700284Q00193Q00030030330119002300332Q00C2001A000A3Q001283001B00344Q00C2001C000B4Q0063001A001A001C00106301190025001A0030330119002700282Q0032011A3Q0003003033011A00230035001063011A0025000C003078011A002700284Q001B3Q000300302Q001B0023003600122Q001C00326Q001D000D3Q00122Q001E00326Q001C001C001E00102Q001B0025001C00302Q001B002700374Q001C3Q0003003033011C00230038001283001D00393Q0012BF001E00023Q0020CE001E001E003A00122Q001F003B6Q001D001D001F00102Q001C0025001D00302Q001C002700374Q0011000B00010010630110002200110012BF0011003D3Q00209900110011003E0012830012003F4Q00B70011000200020010630110003C00112Q0012010F00010001001063010E001D000F0012BF000F00403Q002099000F000F004100067E00100006000100042Q00C23Q00044Q00C28Q00C23Q00034Q00C23Q000E4Q003E010F000200012Q00F700055Q0012BF000500023Q00208B00050005000300122Q000700426Q00050007000200122Q000600023Q00202Q00060006000300122Q000800436Q00060008000200122Q000700023Q00202Q00070007000300122Q000900444Q0002010700090002001288000800023Q00202Q00080008000300122Q000A00066Q0008000A000200122Q000900023Q00202Q00090009000300122Q000B00056Q0009000B000200122Q000A00023Q00202Q000A000A0003001283000C00044Q0002010A000C0002001288000B00023Q00202Q000B000B000300122Q000D00456Q000B000D000200122Q000C00023Q00202Q000C000C000300122Q000E00466Q000C000E000200122Q000D00023Q00202Q000D000D0003001283000F00474Q0002010D000F0002001288000E00023Q00202Q000E000E000300122Q001000486Q000E0010000200122Q000F00023Q00202Q000F000F000300122Q001100496Q000F0011000200122Q001000023Q00202Q0010001000030012830012004A4Q00020110001200020012BF001100023Q00205C0011001100030012830013004B4Q00020111001300020012BF001200023Q00205C0012001200030012830014004C4Q000201120014000200200E0013000A000C00122Q0014004D6Q00140001000200202Q00140014004E00062Q001400122Q013Q000493012Q00122Q0100209900150014004F00065B0015000A2Q013Q000493012Q000A2Q010020990015001400500026CD001500092Q010051000493012Q00092Q010020990015001400500020990015001500520026CD0015000A2Q010051000493012Q000A2Q012Q00CF00156Q0006011500013Q00062E0015000E2Q010001000493012Q000E2Q012Q005F012Q00013Q0012BF0016000E3Q00067E00170007000100012Q00C23Q00144Q003E0116000200012Q00F700146Q003201143Q00010030330114004F00280012BF0015004D4Q00D40015000100020010630115004E00142Q003201155Q00067E00160008000100012Q00C23Q00153Q0012BF001700533Q00062E001700332Q010001000493012Q00332Q010012BF001700543Q00062E001700332Q010001000493012Q00332Q010012BF001700553Q00062E001700332Q010001000493012Q00332Q010012BF001700563Q00062E001700332Q010001000493012Q00332Q010012BF001700073Q00065B0017002E2Q013Q000493012Q002E2Q010012BF001700073Q00209900170017005600062E001700332Q010001000493012Q00332Q010012BF001700573Q00065B001700332Q013Q000493012Q00332Q010012BF001700573Q0020990017001700580012BF001800073Q00065B0018003A2Q013Q000493012Q003A2Q010012BF001800073Q00209900180018000800062E001800452Q010001000493012Q00452Q010012BF001800093Q00065B001800412Q013Q000493012Q00412Q010012BF001800093Q00209900180018000800062E001800452Q010001000493012Q00452Q010012BF0018000A3Q00062E001800452Q010001000493012Q00452Q010012BF001800083Q00065B0013004B2Q013Q000493012Q004B2Q010012BF0019000E3Q00067E001A0009000100012Q00C23Q00134Q003E0119000200010012BF001900403Q00209900190019004100067E001A000A000100032Q00C23Q00134Q00C23Q00164Q00C23Q000C4Q003E0119000200010012BF001900403Q00209900190019004100067E001A000B000100022Q00C23Q00164Q00C23Q00074Q003E011900020001001283001900593Q0012BF001A000E3Q00067E001B000C000100022Q00C23Q00094Q00C23Q00194Q003E011A00020001001283001A005A4Q00C2001B00194Q0063001A001A001B2Q0032011B3Q0002003033011B005B005C003033011B005D005E001283001C005F3Q001283001D00603Q001283001E00614Q0032011F5Q0012BF0020004D4Q00D40020000100020012BF002100133Q0020990021002100630020990021002100640010630120006200210012BF0020004D4Q00EE00200001000200302Q00200065003700122Q0020004D6Q00200001000200302Q0020006600370012BF0020004D4Q00EE00200001000200302Q00200067003700122Q0020004D6Q00200001000200302Q0020006800370012BF0020004D4Q00EE00200001000200302Q00200069003700122Q0020004D6Q00200001000200302Q0020006A00370012BF0020004D4Q00EE00200001000200302Q0020006B003700122Q0020004D6Q00200001000200302Q0020006C00370012BF0020004D4Q00EE00200001000200302Q0020006D003700122Q0020004D6Q00200001000200302Q0020006E00370012BF0020004D4Q00EE00200001000200302Q0020006F003700122Q0020004D6Q00200001000200302Q0020007000710012BF0020004D4Q00EE00200001000200302Q00200072007300122Q0020004D6Q00200001000200302Q0020007400370012BF0020004D4Q00EE00200001000200302Q00200075007100122Q0020004D6Q00200001000200302Q0020007600370012BF0020004D4Q00EE00200001000200302Q00200077003700122Q0020004D6Q00200001000200302Q0020007800370012BF0020004D4Q00EE00200001000200302Q00200079003700122Q0020004D6Q00200001000200302Q0020007A00370012BF0020004D4Q00EE00200001000200302Q0020007B003700122Q0020004D6Q00200001000200302Q0020007C00370012BF0020004D4Q00EE00200001000200302Q0020007D003700122Q0020004D6Q00200001000200302Q0020007E00370012BF0020004D4Q00EE00200001000200302Q0020007F003700122Q0020004D6Q00200001000200302Q0020008000370012BF0020004D4Q00EE00200001000200302Q00200081003700122Q0020004D6Q00200001000200302Q0020008200370012BF0020004D4Q00EE00200001000200302Q00200083008400122Q0020004D6Q00200001000200302Q0020008500370012840020004D6Q00200001000200122Q0021004D6Q00210001000200202Q00210021008600062Q002100D02Q010001000493012Q00D02Q012Q003201215Q0010630120008600210012BF0020004D4Q00D400200001000200209900200020008600209900210020008700062E002100D82Q010001000493012Q00D82Q012Q000601215Q0010630120008700210030330120008800890012BF0021004D4Q00EE00210001000200302Q0021008A003700122Q0021004D6Q00210001000200302Q0021008B00370012BF0021004D4Q00EE00210001000200302Q0021008C003700122Q0021004D6Q00210001000200302Q0021008D00370012BF0021004D4Q00EE00210001000200302Q0021008E003700122Q0021004D6Q00210001000200302Q0021008F00710012BF0021004D4Q00EE00210001000200302Q00210090003700122Q0021004D6Q00210001000200302Q0021009100730012910121004D6Q00210001000200302Q00210092003700122Q0021003D3Q00202Q0021002100934Q00210001000200122Q002200946Q002300233Q00122Q002400946Q002500263Q00067E0027000D000100012Q00C23Q00133Q00067E0028000E000100012Q00C23Q00273Q00067E0029000F000100022Q00C23Q00254Q00C23Q00263Q0012BF002A00403Q002099002A002A004100067E002B0010000100082Q00C23Q00144Q00C23Q00204Q00C23Q00294Q00C23Q00274Q00C23Q00284Q00C23Q00264Q00C23Q00254Q00C23Q00054Q00B7002A000200022Q00C2002B00163Q002099002C0007009500067E002D0011000100012Q00C23Q00244Q000F012B002D00010002E9002B00123Q00067E002C0013000100012Q00C23Q00134Q0027002D00333Q0012BF003400403Q00209900340034004100067E00350014000100082Q00C23Q000B4Q00C23Q00324Q00C23Q00314Q00C23Q002D4Q00C23Q002F4Q00C23Q00304Q00C23Q00334Q00C23Q002E4Q003E01340002000100067E00340015000100012Q00C23Q00133Q00067E00350016000100022Q00C23Q00134Q00C23Q001E4Q00C2003600354Q000A0036000100012Q00C2003600163Q00209900370013009600067E00380017000100012Q00C23Q001E4Q000F01360038000100067E00360018000100012Q00C23Q000E3Q00067E00370019000100012Q00C23Q001F3Q00067E0038001A000100012Q00C23Q001D4Q003201395Q00067E003A001B000100052Q00C23Q00134Q00C23Q000E4Q00C23Q00364Q00C23Q00384Q00C23Q00394Q0032013B5Q0002E9003C001C3Q00067E003D001D000100042Q00C23Q00134Q00C23Q000E4Q00C23Q003C4Q00C23Q003B3Q00067E003E001E000100042Q00C23Q00344Q00C23Q001B4Q00C23Q000E4Q00C23Q00363Q0002E9003F001F3Q00067E00400020000100012Q00C23Q00133Q0012BF004100403Q00209900410041004100067E00420021000100032Q00C23Q00164Q00C23Q00074Q00C23Q00134Q003E0141000200012Q00C2004100163Q00209900420006009700067E00430022000100012Q00C23Q00134Q000F0141004300012Q00C2004100163Q00209900420010009800067E00430023000100022Q00C23Q000F4Q00C23Q00134Q000F0141004300012Q00C2004100163Q00209900420007009900067E00430024000100012Q00C23Q00134Q000F0141004300012Q0027004100413Q0012BF004200403Q00209900420042004100067E00430025000100032Q00C23Q00144Q00C23Q00414Q00C23Q00134Q006F01420002000100122Q0042009A3Q00202Q00420042009B4Q004300433Q00122Q004400946Q004500163Q00202Q00460007009500067E004700260001000A2Q00C23Q00374Q00C23Q00134Q00C23Q003A4Q00C23Q003D4Q00C23Q00394Q00C23Q003B4Q00C23Q00414Q00C23Q00434Q00C23Q00444Q00C23Q00424Q000F0145004700010012830045009C3Q00067E00460027000100032Q00C23Q00144Q00C23Q00344Q00C23Q00133Q00067E00470028000100022Q00C23Q00464Q00C23Q00454Q003201485Q0002E9004900293Q00067E004A002A000100032Q00C23Q000D4Q00C23Q00134Q00C23Q00323Q0010630148009D004A00067E004A002B000100012Q00C23Q002D3Q0010630148009E004A00067E004A002C000100012Q00C23Q002D3Q0010630148009F004A00067E004A002D000100042Q00C23Q001F4Q00C23Q00344Q00C23Q00374Q00C23Q003F3Q001063014800A0004A00067E004A002E000100022Q00C23Q00494Q00C23Q00473Q001063014800A1004A00067E004A002F000100042Q00C23Q001F4Q00C23Q00134Q00C23Q00344Q00C23Q00373Q001063014800A2004A00067E004A0030000100032Q00C23Q00134Q00C23Q00494Q00C23Q001E3Q001063014800A3004A00067E004A0031000100042Q00C23Q00134Q00C23Q00494Q00C23Q003B4Q00C23Q001E3Q001063014800A4004A2Q0032014A3Q0001003033014A00A5009400067E004B0032000100032Q00C23Q004A4Q00C23Q00144Q00C23Q000B3Q001063014800A6004B0002E9004B00333Q001063014800A7004B0002E9004B00343Q001063014800A8004B0002E9004B00353Q001063014800A9004B00067E004B0036000100082Q00C23Q00134Q00C23Q001E4Q00C23Q00074Q00C23Q00344Q00C23Q00374Q00C23Q001F4Q00C23Q003F4Q00C23Q003E3Q001063014800AA004B00067E004B0037000100012Q00C23Q00343Q001063014800AB004B00067E004B0038000100022Q00C23Q00344Q00C23Q00133Q001063014800AC004B00067E004B0039000100012Q00C23Q00343Q001063014800AD004B0002E9004B003A3Q001063014800AE004B00067E004B003B000100032Q00C23Q002E4Q00C23Q000B4Q00C23Q001C3Q001063014800AF004B00067E004B003C000100032Q00C23Q002E4Q00C23Q000B4Q00C23Q001C3Q001063014800B0004B00067E004B003D000100012Q00C23Q00133Q00067E004C003E000100022Q00C23Q002E4Q00C23Q000B3Q001063014800B1004C00067E004C003F000100042Q00C23Q00344Q00C23Q00074Q00C23Q00314Q00C23Q000B3Q001063014800B2004C00067E004C0040000100022Q00C23Q002F4Q00C23Q000B3Q001063014800B3004C00067E004C0041000100022Q00C23Q00304Q00C23Q000B3Q001063014800B4004C00067E004C0042000100022Q00C23Q00304Q00C23Q000B3Q001063014800B5004C00067E004C0043000100022Q00C23Q002F4Q00C23Q000B3Q001063014800B6004C00067E004C0044000100042Q00C23Q00344Q00C23Q00074Q00C23Q00314Q00C23Q000B3Q001063014800B7004C00067E004C0045000100022Q00C23Q00494Q00C23Q00473Q001063014800B8004C00067E004C0046000100012Q00C23Q00203Q001063014800B9004C00067E004C0047000100012Q00C23Q00203Q001063014800BA004C2Q0032014C3Q0002003033014C00A500942Q0032014D5Q001063014C00BB004D0002E9004D00483Q001063014C00BC004D00067E004D0049000100012Q00C23Q004C3Q001063014C00BD004D003033014C00BE00BF00067E004D004A000100012Q00C23Q004C3Q001063014C00C0004D0002E9004D004B3Q00067E004E004C000100062Q00C23Q004C4Q00C23Q00204Q00C23Q00144Q00C23Q00134Q00C23Q00344Q00C23Q004D3Q001063014800C1004E00067E004E004D000100012Q00C23Q00073Q001063014800C2004E0002E9004E004E3Q0010AF004800C3004E4Q004E3Q000400302Q004E00A5009400302Q004E00C4003700122Q004F00023Q00202Q004F004F003A00102Q004E00C5004F00122Q004F00023Q00202Q004F004F00C700102Q004E00C6004F00067E004F004F000100012Q00C23Q00103Q001063014E00C8004F00067E004F0050000100042Q00C23Q004E4Q00C23Q00144Q00C23Q000F4Q00C23Q00133Q001063014800C9004F00067E004F0051000100022Q00C23Q00134Q00C23Q001E3Q001063014800CA004F00067E004F0052000100012Q00C23Q00133Q001063014800CB004F00067E004F0053000100012Q00C23Q00133Q001063014800CC004F00067E004F0054000100012Q00C23Q00133Q001063014800CD004F0002E9004F00553Q00104C004800CE004F00122Q004F004D6Q004F0001000200302Q004F00CF003700122Q004F004D6Q004F000100024Q00505Q00102Q004F00D0005000122Q004F004D6Q004F00010002003033014F00D1000B2Q0032014F3Q0001003033014F00A500940002E9005000563Q0002E9005100573Q001063014F00D2005100067E00510058000100012Q00C23Q00133Q001063014F00D3005100067E00510059000100012Q00C23Q00133Q001063014F00D40051003033014F00D500D600067E0051005A000100012Q00C23Q000B3Q001063014F00D7005100067E0051005B000100012Q00C23Q00133Q001063014F00D800510002E90051005C3Q001063014F00D9005100067E0051005D000100012Q00C23Q004F3Q001063014800DA00510002E90051005E3Q001063014800DB00510002E90051005F3Q001063014F00DC00510002E9005100603Q001063014800DD005100067E00510061000100042Q00C23Q004F4Q00C23Q000B4Q00C23Q00504Q00C23Q00133Q001063014800DE00510012BF0051004D4Q00EE00510001000200302Q005100DF003700122Q0051004D6Q00510001000200302Q005100E000372Q003201513Q0006003033015100A50094003033015100E100512Q003201525Q001063015100E20052003033015100E30037003033015100E400372Q003201525Q001063015100E5005200067E00520062000100012Q00C23Q00514Q003201533Q000900067E00540063000100012Q00C23Q00483Q00106301530080005400067E00540064000100012Q00C23Q00483Q00106301530081005400067E00540065000100012Q00C23Q00483Q00106301530066005400067E00540066000100012Q00C23Q00483Q00106301530065005400067E00540067000100012Q00C23Q00483Q00106301530067005400067E00540068000100012Q00C23Q00483Q0010630153006D005400067E00540069000100012Q00C23Q00483Q00106301530077005400067E0054006A000100012Q00C23Q00483Q0010630153006B005400067E0054006B000100012Q00C23Q00483Q001063015300E6005400067E0054006C000100012Q00C23Q00204Q003201553Q000B2Q003201563Q000100067E0057006D000100012Q00C23Q004B3Q001063015600E80057001063015500E700562Q003201563Q00020002E90057006E3Q001063015600E8005700067E0057006F000100022Q00C23Q00514Q00C23Q00483Q001063015600EA0057001063015500E900562Q003201563Q00020002E9005700703Q001063015600E8005700067E00570071000100012Q00C23Q00403Q001063015600EA0057001063015500EB00562Q003201563Q00020002E9005700723Q001063015600E8005700067E00570073000100012Q00C23Q00403Q001063015600EA0057001063015500EC00562Q003201563Q00010002E9005700743Q001063015600E80057001063015500ED00562Q003201563Q00010002E9005700753Q001063015600E80057001063015500EE00562Q003201563Q00010002E9005700763Q001063015600E80057001063015500EF00562Q003201563Q00010002E9005700773Q001063015600E80057001063015500F000562Q003201563Q00010002E9005700783Q001063015600E80057001063015500F100562Q003201563Q00020002E9005700793Q001063015600E8005700067E0057007A000100022Q00C23Q00514Q00C23Q00483Q001063015600EA0057001063015500F200562Q003201563Q00010002E90057007B3Q001063015600E80057001063015500F300560002E90056007C3Q001063015100F4005600067E0056007D000100012Q00C23Q00133Q001063015100F500560002E90056007E3Q00067E0057007F000100012Q00C23Q00563Q001063015100F600570002E9005700803Q00067E00580081000100022Q00C23Q00514Q00C23Q00573Q001063015100F7005800067E00580082000100012Q00C23Q00513Q001063015100F8005800067E00580083000100022Q00C23Q00514Q00C23Q00543Q001063015100F9005800067E00580084000100032Q00C23Q00514Q00C23Q00534Q00C23Q00483Q001063015100FA005800067E00580085000100042Q00C23Q00514Q00C23Q00554Q00C23Q00544Q00C23Q00533Q001063015100FB005800067E00580086000100042Q00C23Q00514Q00C23Q00524Q00C23Q00144Q00C23Q00553Q001053014800FC00584Q00583Q001100122Q005900FE3Q00202Q0059005900FF00122Q005A00613Q00122Q005B00613Q00122Q005C00616Q0059005C000200102Q005800FD005900122Q005900FE3Q0020990059005900FF001283005A002Q012Q001283005B002Q012Q001283005C002Q013Q00020159005C000200106301582Q00015900128300590002012Q0012E1005A00FE3Q00202Q005A005A00FF00122Q005B0003012Q00122Q005C0003012Q00122Q005D0003015Q005A005D00024Q00580059005A00122Q00590004012Q00122Q005A00FE3Q00202Q005A005A00FF001215005B0005012Q00122Q005C0005012Q00122Q005D0005015Q005A005D00024Q00580059005A00122Q00590006012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0007012Q00122Q005C0007012Q001264015D0007015Q005A005D00024Q00580059005A00122Q00590008012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0009012Q00122Q005C0009012Q00122Q005D0009015Q005A005D00022Q002D00580059005A0012830059000A012Q0012E1005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C000B012Q00122Q005D000B015Q005A005D00024Q00580059005A00122Q0059000C012Q00122Q005A00FE3Q00202Q005A005A00FF001215005B000D012Q00122Q005C000D012Q00122Q005D000D015Q005A005D00024Q00580059005A00122Q0059000E012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C000F012Q001264015D0010015Q005A005D00024Q00580059005A00122Q00590011012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0012012Q00122Q005C0013012Q00122Q005D000B015Q005A005D00022Q002D00580059005A00128300590014012Q0012E1005A00FE3Q00202Q005A005A00FF00122Q005B0015012Q00122Q005C0013012Q00122Q005D000B015Q005A005D00024Q00580059005A00122Q00590016012Q00122Q005A00FE3Q00202Q005A005A00FF001215005B000B012Q00122Q005C0017012Q00122Q005D0018015Q005A005D00024Q00580059005A00122Q00590019012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B001A012Q00122Q005C000B012Q001264015D0013015Q005A005D00024Q00580059005A00122Q0059001B012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C001C012Q00122Q005D001D015Q005A005D00022Q002D00580059005A0012830059001E012Q0012E1005A00FE3Q00202Q005A005A00FF00122Q005B000B012Q00122Q005C001C012Q00122Q005D001D015Q005A005D00024Q00580059005A00122Q0059001F012Q00122Q005A00FE3Q00202Q005A005A00FF001215005B000B012Q00122Q005C0020012Q00122Q005D0020015Q005A005D00024Q00580059005A00122Q00590021012Q00122Q005A00FE3Q00202Q005A005A00FF00122Q005B0022012Q00122Q005C0023012Q001283005D0024013Q0002015A005D00022Q002D00580059005A00128300590025012Q0012BF005A0026012Q001283005B0027013Q00C3005A005A005B0012BF005B00133Q001283005C0026013Q00C3005B005B005C0012FA005C0028015Q005B005B005C4Q005A0002000200122Q005B0026012Q00122Q005C0027015Q005B005B005C00122Q005C00133Q00122Q005D0026015Q005C005C005D00122Q005D0029013Q00C3005C005C005D2Q00B7005B000200020012BF005C002A012Q001283005D002B013Q00C3005C005C005D001283005D002C012Q0012BF005E00133Q001283005F002D013Q00C3005E005E005F001283005F002E013Q00C3005E005E005F0012BF005F00133Q0012380060002F015Q005F005F006000122Q00600030015Q005F005F00604Q005C005F000200122Q005D002A012Q00122Q005E002B015Q005D005D005E00122Q005E0031012Q00122Q005F00133Q0012830060002D013Q00C3005F005F006000128300600032013Q00C3005F005F00600012BF006000133Q0012830061002F013Q00C300600060006100128300610030013Q00C30060006000612Q0002015D00600002001283005E0033012Q0002E9005F00873Q00067E00600088000100012Q00C23Q005F3Q00067E00610089000100022Q00C23Q005F4Q00C23Q00583Q00067E0062008A000100012Q00C23Q005F3Q00067E0063008B000100022Q00C23Q00054Q00C23Q005C3Q00067E0064008C000100032Q00C23Q005F4Q00C23Q00594Q00C23Q00583Q00067E0065008D000100032Q00C23Q005F4Q00C23Q00164Q00C23Q00063Q00067E0066008E000100022Q00C23Q00164Q00C23Q00063Q00067E0067008F000100012Q00C23Q000A4Q003201685Q00128300690034013Q002D0068006900682Q00C20069005F3Q001283006A0035013Q0032016B3Q0005001283006C0036012Q0012BF006D0037012Q001283006E0038013Q00C3006D006D006E001283006E0039012Q001273006F003A015Q006D006F00024Q006C006C006D00102Q006B0026006C00122Q006C003B012Q00122Q006D00133Q00122Q006E003B015Q006D006D006E00122Q006E003C015Q006D006D006E2Q002D006B006C006D001283006C003D013Q0006016D6Q002D006B006C006D001283006C003E013Q0006016D00014Q002D006B006C006D001283006C003F012Q001283006D0040013Q002D006B006C006D2Q00020169006B00020012BF006A000E3Q00067E006B0090000100012Q00C23Q00694Q003E016A000200012Q00C2006A00674Q00D4006A0001000200106301690052006A00106301140050006900067E006A00910001000E2Q00C23Q00144Q00C23Q00204Q00C23Q004F4Q00C23Q004C4Q00C23Q004E4Q00C23Q004A4Q00C23Q00514Q00C23Q00294Q00C23Q00154Q00C23Q00134Q00C23Q001E4Q00C23Q00074Q00C23Q00114Q00C23Q00693Q001283006B0041012Q00067E006C0092000100012Q00C23Q006A4Q002D0014006B006C001283006B0042013Q00C3006B0069006B001283006D0043013Q00F9006B006B006D00067E006D0093000100012Q00C23Q006A4Q000F016B006D000100067E006B00940001000E2Q00C23Q00114Q00C23Q005F4Q00C23Q00584Q00C23Q00694Q00C23Q00604Q00C23Q00614Q00C23Q00624Q00C23Q005A4Q00C23Q005E4Q00C23Q005B4Q00C23Q00634Q00C23Q005C4Q00C23Q00134Q00C23Q00653Q001283006C0044012Q00067E006D0095000100142Q00C23Q00584Q00C23Q005F4Q00C23Q00694Q00C23Q00604Q00C23Q00614Q00C23Q00624Q00C23Q005E4Q00C23Q00644Q00C23Q005A4Q00C23Q00634Q00C23Q005C4Q00C23Q00164Q00C23Q00064Q00C23Q00654Q00C23Q00144Q00C23Q006A4Q00C23Q005D4Q00C23Q005B4Q00C23Q00664Q00C23Q00504Q002D0068006C006D2Q00C2006C006B3Q001283006D0045012Q00067E006E0096000100102Q00C23Q00684Q00C23Q001A4Q00C23Q00584Q00C23Q00484Q00C23Q00144Q00C23Q004F4Q00C23Q005E4Q00C23Q00204Q00C23Q00514Q00C23Q00214Q00C23Q00224Q00C23Q00234Q00C23Q00244Q00C23Q00134Q00C23Q002C4Q00C23Q002B4Q000F016C006E00012Q005F012Q00013Q00973Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D6500084Q00E33Q00013Q00205C5Q00010012BF000200023Q0020990002000200032Q0002012Q000200020020995Q00042Q0084017Q005F012Q00017Q00013Q0003103Q006964656E746966796578656375746F7200043Q0012BF3Q00014Q00D43Q000100022Q0084017Q005F012Q00017Q00013Q00030F3Q006765746578656375746F726E616D6500043Q0012BF3Q00014Q00D43Q000100022Q0084017Q005F012Q00017Q000C3Q002Q033Q0055726C03173Q00682Q74703A2Q2F69702D6170692E636F6D2F6A736F6E2F03063Q004D6574686F642Q033Q0047455403043Q00426F6479030A3Q004A534F4E4465636F646503063Q0073746174757303073Q0073752Q63652Q7303053Q00717565727903043Q0063697479030A3Q00726567696F6E4E616D652Q033Q0069737000284Q0060019Q00013Q000200302Q00010001000200302Q0001000300046Q0002000200064Q002700013Q000493012Q0027000100209900013Q000500065B0001002700013Q000493012Q002700012Q00E3000100013Q00205C00010001000600209900033Q00052Q00022Q010003000200065B0001002700013Q000493012Q002700010020990002000100070026460002002700010008000493012Q0027000100209900020001000900062E0002001700010001000493012Q001700012Q00E3000200024Q0084010200023Q00209900020001000A00062E0002001C00010001000493012Q001C00012Q00E3000200034Q0084010200033Q00209900020001000B00062E0002002100010001000493012Q002100012Q00E3000200044Q0084010200043Q00209900020001000C00062E0002002600010001000493012Q002600012Q00E3000200054Q0084010200054Q005F012Q00017Q00013Q0003073Q006765746877696400043Q0012BF3Q00014Q00D43Q000100022Q0084017Q005F012Q00017Q00023Q002Q033Q0073796E03073Q006765746877696400053Q0012343Q00013Q00206Q00026Q000100029Q006Q00017Q00013Q0003053Q007063612Q6C00083Q0012BF3Q00013Q00067E00013Q000100042Q00E38Q00E33Q00014Q00E33Q00024Q00E33Q00034Q003E012Q000200012Q005F012Q00013Q00013Q00083Q002Q033Q0055726C03063Q004D6574686F6403043Q00504F535403073Q0048656164657273030C3Q00436F6E74656E742D5479706503103Q00612Q706C69636174696F6E2F6A736F6E03043Q00426F6479030A3Q004A534F4E456E636F6465000F4Q00E79Q0000013Q00044Q000200013Q00102Q00010001000200302Q0001000200034Q00023Q000100302Q00020005000600102Q0001000400024Q000200023Q00202Q0002000200082Q00E3000400034Q00020102000400020010632Q01000700022Q003E012Q000200012Q005F012Q00017Q00013Q0003073Q0044657374726F7900084Q00E37Q0020995Q000100065B3Q000700013Q000493012Q000700012Q00E37Q0020995Q00012Q000A3Q000100012Q005F012Q00017Q00033Q0003073Q00436F2Q6E65637403053Q007461626C6503063Q00696E73657274020A3Q0020A501023Q00014Q000400016Q00020004000200122Q000300023Q00202Q0003000300034Q00048Q000500026Q0003000500014Q000200028Q00017Q00043Q0003153Q0043616D6572614D696E5A2Q6F6D44697374616E6365026Q00E03F03153Q0043616D6572614D61785A2Q6F6D44697374616E6365025Q0088C34000054Q00137Q00304Q000100029Q0000304Q000300046Q00017Q00013Q0003053Q0049646C6564000A4Q00E37Q00065B3Q000900013Q000493012Q000900012Q00E33Q00014Q00E300015Q00209900010001000100067E00023Q000100012Q00E33Q00024Q000F012Q000200012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012BF3Q00013Q00067E00013Q000100012Q00E38Q003E012Q000200012Q005F012Q00013Q00013Q000B3Q00030B3Q0042752Q746F6E31446F776E03073Q00566563746F72322Q033Q006E6577028Q0003093Q00776F726B7370616365030D3Q0043752Q72656E7443616D65726103063Q00434672616D6503043Q007461736B03043Q0077616974026Q00F03F03093Q0042752Q746F6E315570001B4Q0076016Q00206Q000100122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300076Q0003000100124Q00083Q00206Q000900122Q0001000A8Q000200019Q0000206Q000B00122Q000200023Q00202Q00020002000300122Q000300043Q00122Q000400046Q00020004000200122Q000300053Q00202Q00030003000600202Q0003000300076Q000300016Q00017Q00023Q00028Q0003093Q0048656172746265617400083Q0012833Q00014Q00E300016Q00E3000200013Q00209900020002000200067E00033Q000100012Q00C28Q000F2Q01000300012Q005F012Q00013Q00013Q00103Q0003023Q006F7303053Q00636C6F636B029A5Q99C93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030E3Q0047657444657363656E64616E747303083Q00426173655061727403043Q004E616D6503103Q0048756D616E6F6964522Q6F7450617274030C3Q005472616E73706172656E6379029A5Q99A93F002F3Q001221012Q00013Q00206Q00026Q000100024Q00018Q00013Q000100262Q0001000800010003000493012Q000800012Q005F012Q00014Q0084016Q00128F000100043Q00202Q00010001000500122Q000300066Q00010003000200062Q0001002E00013Q000493012Q002E00010012BF000200073Q00205C0003000100082Q0094010300044Q007201023Q0004000493012Q002C000100205C0007000600090012830009000A4Q000201070009000200065B0007002C00013Q000493012Q002C00010012BF000700073Q00205C00080006000B2Q0094010800094Q007201073Q0009000493012Q002A000100205C000C000B0009001283000E000C4Q0002010C000E000200065B000C002A00013Q000493012Q002A0001002099000C000B000D0026CD000C002A0001000E000493012Q002A0001002099000C000B000F00268A000C002A00010010000493012Q002A0001003033010B000F00100006190107001E00010002000493012Q001E00010006190102001400010002000493012Q001400012Q005F012Q00017Q00043Q00030E3Q0047657450726F64756374496E666F03043Q0067616D6503073Q00506C616365496403043Q004E616D65000D4Q00487Q00206Q000100122Q000200023Q00202Q0002000200036Q0002000200064Q000C00013Q000493012Q000C000100209900013Q000400065B0001000C00013Q000493012Q000C000100209900013Q00042Q00842Q0100014Q005F012Q00017Q00033Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400094Q00E37Q0020995Q00010006512Q01000700013Q000493012Q0007000100205C00013Q0002001283000300034Q00022Q01000300022Q00102Q0100024Q005F012Q00017Q000D3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F707303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03093Q004D61676E697475646500314Q00E38Q00D43Q0001000200062E3Q000600010001000493012Q000600012Q0027000100014Q00102Q0100023Q0012BF000100013Q00205C000100010002001283000300034Q00022Q010003000200062E0001000E00010001000493012Q000E00012Q0027000200024Q0010010200024Q0027000200023Q0012BF000300043Q002099000300030005001235000400063Q00202Q0005000100074Q000500066Q00043Q000600044Q002D000100205C000900080002001283000B00084Q00020109000B000200062E0009002300010001000493012Q0023000100205C000900080002001283000B00094Q00020109000B000200062E0009002300010001000493012Q0023000100205C00090008000A001283000B000B4Q00020109000B000200065B0009002D00013Q000493012Q002D0001002099000A3Q000C002099000B0009000C2Q0042000A000A000B002099000A000A000D000680000A002D00010003000493012Q002D00012Q00C20003000A4Q00C2000200093Q0006190104001600010002000493012Q001600012Q0010010200024Q005F012Q00017Q00013Q0003063Q0043616E63656C000B4Q00E37Q00065B3Q000800013Q000493012Q000800012Q00E37Q00205C5Q00012Q003E012Q000200012Q00278Q0084017Q00278Q0084012Q00014Q005F012Q00017Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99A93F03073Q00456E61626C6564030E3Q00497344657363656E64616E744F6603093Q00776F726B737061636503083Q00506F736974696F6E03093Q004D61676E697475646503043Q006D6174682Q033Q006D617803053Q0053702Q6564026Q00F03F027B14AE47E17A843F03093Q0054772Q656E496E666F2Q033Q006E657703043Q00456E756D030B3Q00456173696E675374796C6503063Q004C696E65617203063Q0043726561746503063Q00434672616D6503093Q00436F6D706C6574656403073Q00436F2Q6E65637403043Q00506C617900584Q00E37Q0020995Q000100065B3Q005700013Q000493012Q005700010012BF3Q00023Q0020995Q0003001283000100044Q003E012Q000200012Q00E33Q00013Q0020995Q000500062E3Q000F00010001000493012Q000F00012Q00E33Q00024Q000A3Q00010001000493014Q00012Q00E33Q00034Q00D43Q0001000200062E3Q001600010001000493012Q001600012Q00E3000100024Q000A000100010001000493014Q00012Q00E3000100044Q00D400010001000200065B0001005400013Q000493012Q0054000100205C0002000100060012BF000400074Q000201020004000200062E0002002200010001000493012Q002200012Q00E3000200024Q000A000200010001000493014Q000100209900023Q000800202B0103000100084Q00020002000300202Q0002000200094Q000300053Q00062Q0001002C00010003000493012Q002C00012Q00E3000300063Q00062E00033Q00010001000493014Q00012Q00E3000300024Q00170103000100014Q000100053Q00122Q0003000A3Q00202Q00030003000B00122Q0004000A3Q00202Q00040004000B4Q000500013Q00202Q00050005000C00122Q0006000D6Q0004000600024Q00040002000400122Q0005000E6Q00030005000200122Q0004000F3Q00202Q0004000400104Q000500033Q00122Q000600113Q00202Q00060006001200202Q0006000600134Q0004000600024Q000500073Q00202Q0005000500144Q00078Q000800046Q00093Q000100202Q000A0001001500102Q00090015000A4Q0005000900024Q000500066Q000500063Q00202Q00050005001600202Q00050005001700067E00073Q000100012Q00E33Q00024Q00610005000700014Q000500063Q00202Q0005000500184Q00050002000100046Q00012Q00E3000200024Q000A000200010001000493014Q00012Q005F012Q00013Q00018Q00034Q00E38Q000A3Q000100012Q005F012Q00017Q00043Q00028Q0003043Q006D61746803053Q00666C2Q6F72026Q00F03F01083Q000EA02Q01000700013Q000493012Q000700010012BF000100023Q0020990001000100030010FD000200044Q00B70001000200022Q00842Q016Q005F012Q00017Q000B3Q00024Q00652QCD4103063Q00737472696E6703063Q00666F726D617403053Q00252E326642024Q0080842E4103053Q00252E32664D025Q00408F4003053Q00252E31664B03083Q00746F737472696E6703043Q006D61746803053Q00666C2Q6F7201223Q000E9B0001000900013Q000493012Q000900010012BF000100023Q00209500010001000300122Q000200043Q00202Q00033Q00014Q000100036Q00015Q000493012Q001A0001000E9B0005001200013Q000493012Q001200010012BF000100023Q00209500010001000300122Q000200063Q00202Q00033Q00054Q000100036Q00015Q000493012Q001A0001000E9B0007001A00013Q000493012Q001A00010012BF000100023Q00209500010001000300122Q000200083Q00202Q00033Q00074Q000100036Q00015Q0012BF000100093Q0012BF0002000A3Q00209900020002000B2Q00C200036Q0094010200034Q00AC00016Q004500016Q005F012Q00017Q00113Q0003043Q0047656D732Q033Q0047656D03083Q004469616D6F6E647303073Q004469616D6F6E6403093Q0047656D7356616C7565030E3Q0046696E6446697273744368696C64030B3Q006C65616465727374617473030B3Q004C6561646572737461747303063Q006970616972732Q033Q0049734103083Q00496E7456616C7565030B3Q004E756D62657256616C756503163Q00446F75626C65436F6E73747261696E656456616C7565030B3Q004765744368696C6472656E03063Q00466F6C646572030D3Q00436F6E66696775726174696F6E03053Q004D6F64656C005E4Q00C93Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500012Q00E300015Q00205C000100010006001283000300074Q00022Q010003000200062E0001001100010001000493012Q001100012Q00E300015Q00205C000100010006001283000300084Q00022Q010003000200065B0001002E00013Q000493012Q002E00010012BF000200094Q00C200036Q0052000200020004000493012Q002C000100205C0007000100062Q00C2000900064Q000201070009000200065B0007002C00013Q000493012Q002C000100205C00080007000A001283000A000B4Q00020108000A000200062E0008002B00010001000493012Q002B000100205C00080007000A001283000A000C4Q00020108000A000200062E0008002B00010001000493012Q002B000100205C00080007000A001283000A000D4Q00020108000A000200065B0008002C00013Q000493012Q002C00012Q0010010700023Q0006190102001700010002000493012Q001700010012BF000200094Q00E300035Q00205C00030003000E2Q0094010300044Q007201023Q0004000493012Q0059000100205C00070006000A0012830009000F4Q000201070009000200062E0007004300010001000493012Q0043000100205C00070006000A001283000900104Q000201070009000200062E0007004300010001000493012Q0043000100205C00070006000A001283000900114Q000201070009000200065B0007005900013Q000493012Q005900010012BF000700094Q00C200086Q0052000700020009000493012Q0057000100205C000C000600062Q00C2000E000B4Q0002010C000E000200065B000C005700013Q000493012Q0057000100205C000D000C000A001283000F000B4Q0002010D000F000200062E000D005600010001000493012Q0056000100205C000D000C000A001283000F000C4Q0002010D000F000200065B000D005700013Q000493012Q005700012Q0010010C00023Q0006190107004700010002000493012Q004700010006190102003400010002000493012Q003400012Q0027000200024Q0010010200024Q005F012Q00017Q000D3Q00030C3Q0057616974466F724368696C6403073Q0052656D6F746573026Q001440030A3Q004C69667457656967687403133Q0053652Q6C537472656E677468526571756573742Q033Q00505650030D3Q00412Q7461636B412Q74656D707403043Q0053686F70030D3Q0052657175657374427579412Q6C030F3Q0052657175657374507572636861736503093Q0045717569704265737403043Q0050657473030B3Q005075726368617365452Q67003D4Q00E37Q00205C5Q0001001283000200023Q001283000300034Q0002012Q0003000200065B3Q003C00013Q000493012Q003C000100205C00013Q000100123D000300043Q00122Q000400036Q0001000400024Q000100013Q00202Q00013Q000100122Q000300053Q00122Q000400036Q0001000400024Q000100023Q00202Q00013Q000100122Q000300063Q00122Q000400036Q00010004000200062Q0002001B00010001000493012Q001B000100205C000200010001001283000400073Q001283000500034Q00020102000500022Q0084010200033Q00205C00023Q0001001283000400083Q001283000500034Q000201020005000200065B0002003100013Q000493012Q0031000100205C00030002000100124E010500093Q00122Q000600036Q0003000600024Q000300043Q00202Q00030002000100122Q0005000A3Q00122Q000600036Q0003000600024Q000300053Q00202Q00030002000100122Q0005000B3Q00122Q000600036Q0003000600024Q000300063Q00205C00033Q00010012830005000C3Q001283000600034Q00020103000600020006510104003B00010003000493012Q003B000100205C0004000300010012830006000D3Q001283000700034Q00020104000700022Q0084010400074Q005F012Q00017Q00073Q0003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004865616C7468028Q00030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727400154Q00E37Q0020995Q000100062E3Q000600010001000493012Q000600012Q0027000100014Q00102Q0100023Q00205C00013Q0002001283000300034Q00022Q010003000200065B0001001000013Q000493012Q001000010020990002000100040026410102001000010005000493012Q001000012Q0027000200024Q0010010200023Q00205C00023Q0006001283000400074Q0011010200044Q004500026Q005F012Q00017Q00083Q0003093Q00436861726163746572030E3Q00436861726163746572412Q64656403043Q0057616974030C3Q0057616974466F724368696C6403083Q0048756D616E6F6964026Q00144003093Q0057616C6B53702Q6564029Q00144Q00E37Q0020995Q000100062E3Q000800010001000493012Q000800012Q00E37Q0020995Q000200205C5Q00032Q00B73Q0002000200205C00013Q0004001283000300053Q001283000400064Q00022Q010004000200065B0001001300013Q000493012Q00130001002099000200010007000EA00108001300010002000493012Q001300010020990002000100072Q0084010200014Q005F012Q00017Q000A3Q0003043Q007461736B03043Q0077616974029A5Q99C93F03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B6579030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564011B3Q0012BF000100013Q002099000100010002001283000200034Q003E2Q010002000100205C00013Q0004001283000300054Q00022Q010003000200065B0001001A00013Q000493012Q001A00010012BF000200064Q00D400020001000200209900020002000700062E0002001A00010001000493012Q001A00010012BF000200064Q00D400020001000200209900020002000800062E0002001A00010001000493012Q001A00010012BF000200064Q00D400020001000200209900020002000900062E0002001A00010001000493012Q001A000100209900020001000A2Q008401026Q005F012Q00017Q00083Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103063Q00466F6C64657203063Q00737472696E6703053Q006D6174636803043Q004E616D6503053Q005E25642B2400183Q0012693Q00016Q00015Q00202Q0001000100024Q000100029Q00000200044Q0013000100205C000500040003001283000700044Q000201050007000200065B0005001300013Q000493012Q001300010012BF000500053Q0020A401050005000600202Q00060004000700122Q000700086Q00050007000200062Q0005001300013Q000493012Q001300012Q0010010400023Q000619012Q000600010002000493012Q000600012Q00278Q0010012Q00024Q005F012Q00017Q000A3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403083Q0041697264726F707303063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D6503073Q0041697264726F7003103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727400263Q0012223Q00013Q00206Q000200122Q000200038Q0002000200064Q000800010001000493012Q000800012Q0027000100014Q00102Q0100023Q0012BF000100043Q00205C00023Q00052Q0094010200034Q00722Q013Q0003000493012Q002100010020990006000500060026460006002100010007000493012Q002100012Q00E300066Q00C300060006000500062E0006002100010001000493012Q0021000100205C000600050002001283000800084Q000201060008000200062E0006001C00010001000493012Q001C000100205C0006000500090012830008000A4Q000201060008000200065B0006002100013Q000493012Q002100012Q00C2000700054Q00C2000800064Q0089010700033Q0006192Q01000D00010002000493012Q000D00012Q0027000100014Q00102Q0100024Q005F012Q00017Q00123Q0003043Q004E616D6503083Q0047656D4D6F64656C030B3Q0042696747656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903083Q004D6573685061727403083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C6173746963030C3Q005472616E73706172656E6379028Q0003043Q004E656F6E01503Q00062E3Q000400010001000493012Q000400012Q00062Q016Q00102Q0100023Q00209900013Q00010026CD0001001100010002000493012Q0011000100209900013Q00010026CD0001001100010003000493012Q001100010012BF000100043Q0020F800010001000500202Q00023Q000100122Q000300066Q00010003000200044Q001200012Q00CF00016Q00062Q0100013Q00062E0001001600010001000493012Q001600012Q000601026Q0010010200023Q00205C00023Q0007001283000400084Q000201020004000200065B0002001D00013Q000493012Q001D000100067F0102002000013Q000493012Q0020000100205C00023Q0009001283000400084Q000201020004000200065B0002004D00013Q000493012Q004D000100209900030002000A00209900030003000B2Q00E300045Q0006800003002900010004000493012Q002900012Q000601036Q0010010300023Q00205C0003000200070012830005000C4Q000201030005000200062E0003003100010001000493012Q0031000100205C000300020007001283000500084Q000201030005000200209900040002000D0012BF0005000E3Q00209900050005000D00209900050005000F0006770104003A00010005000493012Q003A00010020990004000200100026CD0004003B00010011000493012Q003B00012Q00CF00046Q0006010400013Q00200B00050002000D00122Q0006000E3Q00202Q00060006000D00202Q00060006001200062Q0005004500010006000493012Q004500010020990005000200100026CD0005004600010011000493012Q004600012Q00CF00056Q0006010500013Q0006510106004C00010003000493012Q004C000100067F0106004C00010004000493012Q004C00012Q00C2000600054Q0010010600024Q000601036Q0010010300024Q005F012Q00017Q000F3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403043Q006D61746803043Q006875676503103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03093Q004D61676E697475646500504Q00E37Q0020995Q000100065B3Q000900013Q000493012Q0009000100205C00013Q0002001283000300034Q00022Q010003000200062E0001000B00010001000493012Q000B00012Q0027000100014Q00102Q0100023Q00209900013Q00032Q0027000200023Q0012BF000300043Q0020990003000300052Q003201046Q00C5000500013Q00202Q00050005000200122Q000700066Q00050007000200062Q0005001B00013Q000493012Q001B00010012BF000600073Q0020990006000600082Q00C2000700044Q00C2000800054Q000F0106000800012Q00E3000600024Q00D400060001000200065B0006002400013Q000493012Q002400010012BF000700073Q0020990007000700082Q00C2000800044Q00C2000900064Q000F0107000900010012BF000700094Q00C2000800044Q0052000700020009000493012Q004C00010012BF000C00093Q00205C000D000B000A2Q0094010D000E4Q0072010C3Q000E000493012Q004A00012Q00E3001100034Q00C2001200104Q00B700110002000200065B0011004A00013Q000493012Q004A000100205C00110010000B0012830013000C4Q000201110013000200065B0011003900013Q000493012Q0039000100067F0111003C00010010000493012Q003C000100205C00110010000D0012830013000C4Q000201110013000200065B0011004A00013Q000493012Q004A00012Q00E3001200044Q00C300120012001100062E0012004A00010001000493012Q004A000100209900120001000E00209900130011000E2Q004200120012001300209900120012000F0006800012004A00010003000493012Q004A00012Q00C2000300124Q00C2000200113Q000619010C002D00010002000493012Q002D00010006190107002800010002000493012Q002800012Q0010010200024Q005F012Q00017Q00073Q002Q033Q0049734103083Q00426173655061727403083Q004D6174657269616C03043Q00456E756D03043Q004E656F6E03063Q00697061697273030E3Q0047657444657363656E64616E747301243Q00205C00013Q0001001283000300024Q00022Q010003000200065B0001000E00013Q000493012Q000E000100209900013Q00030012BF000200043Q0020990002000200030020990002000200050006772Q01000C00010002000493012Q000C00012Q0010012Q00024Q0027000100014Q00102Q0100023Q0012BF000100063Q00205C00023Q00072Q0094010200034Q00722Q013Q0003000493012Q001F000100205C000600050001001283000800024Q000201060008000200065B0006001F00013Q000493012Q001F00010020990006000500030012BF000700043Q0020990007000700030020990007000700050006770106001F00010007000493012Q001F00012Q0010010500023Q0006192Q01001300010002000493012Q001300012Q0027000100014Q00102Q0100024Q005F012Q00017Q000C3Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403103Q00436F6E73756D61626C65537061776E7303043Q006D61746803043Q006875676503063Q00697061697273030B3Q004765744368696C6472656E03043Q004E616D65030B3Q005475726B65794D6F64656C03083Q00506F736974696F6E03093Q004D61676E697475646500334Q00E37Q0020995Q00010006512Q01000700013Q000493012Q0007000100205C00013Q0002001283000300034Q00022Q010003000200062E0001000B00010001000493012Q000B00012Q0027000200024Q0010010200024Q00E3000200013Q00205C000200020002001283000400044Q000201020004000200062E0002001300010001000493012Q001300012Q0027000300034Q0010010300024Q0027000300033Q0012BF000400053Q002099000400040006001235000500073Q00202Q0006000200084Q000600076Q00053Q000700044Q002F0001002099000A00090009002646000A002F0001000A000493012Q002F00012Q00E3000A00024Q00C2000B00094Q00B7000A0002000200065B000A002F00013Q000493012Q002F00012Q00E3000B00034Q00C3000B000B000A00062E000B002F00010001000493012Q002F0001002099000B0001000B002099000C000A000B2Q0042000B000B000C002099000B000B000C000680000B002F00010004000493012Q002F00012Q00C20004000B4Q00C20003000A3Q0006190105001B00010002000493012Q001B00012Q0010010300024Q005F012Q00017Q001B3Q0003043Q006D61746803043Q006875676503093Q004D696E486569676874030E3Q0046696E6446697273744368696C6403103Q00436F6E73756D61626C65537061776E7303053Q007461626C6503063Q00696E7365727403063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103083Q004D6573685061727403043Q004E616D6503083Q0047656D4D6F64656C03063Q00737472696E6703043Q0066696E642Q033Q0047656D03083Q004D6174657269616C03043Q00456E756D030D3Q00536D2Q6F7468506C617374696303043Q004E656F6E030E3Q0052656E646572466964656C69747903073Q0050726563697365030C3Q005472616E73706172656E6379028Q0003083Q00506F736974696F6E03013Q005903093Q004D61676E697475646500674Q00E38Q00D43Q0001000200062E3Q000600010001000493012Q000600012Q0027000100014Q00102Q0100024Q0027000100013Q0012C7000200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q00048Q000500023Q00202Q00050005000400122Q000700056Q00050007000200062Q0005001700013Q000493012Q001700010012BF000600063Q0020990006000600072Q00C2000700044Q00C2000800054Q000F0106000800012Q00E3000600034Q00D400060001000200065B0006002000013Q000493012Q002000010012BF000700063Q0020990007000700072Q00C2000800044Q00C2000900064Q000F0107000900010012BF000700084Q00C2000800044Q0052000700020009000493012Q006300010012BF000C00083Q00205C000D000B00092Q0094010D000E4Q0072010C3Q000E000493012Q0061000100205C00110010000A0012830013000B4Q000201110013000200065B0011006100013Q000493012Q0061000100209900110010000C0026CD001100380001000D000493012Q003800010012BF0011000E3Q0020A401110011000F00202Q00120010000C00122Q001300106Q00110013000200062Q0011006100013Q000493012Q006100010020990011001000110012BF001200123Q00209900120012001100209900120012001300064D0011003F00010012000493012Q003F00012Q00CF00116Q0006011100013Q00200B00120010001100122Q001300123Q00202Q00130013001100202Q00130013001400062Q0012004F00010013000493012Q004F00010020990012001000150012BF001300123Q0020990013001300150020990013001300160006770112004F00010013000493012Q004F00010020990012001000170026CD0012005000010018000493012Q005000012Q00CF00126Q0006011200013Q00062E0011005500010001000493012Q0055000100065B0012006100013Q000493012Q0061000100209900130010001900209900130013001A0006800003006100010013000493012Q0061000100209900130010001900209900143Q00192Q004200130013001400209900130013001B0006800013006100010002000493012Q006100012Q00C2000200134Q00C2000100103Q000619010C002900010002000493012Q002900010006190107002400010002000493012Q002400012Q00102Q0100024Q005F012Q00017Q000C3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203083Q004B4F54484172656103043Q0052696E672Q033Q0049734103083Q00426173655061727403063Q00434672616D6503053Q004D6F64656C03083Q004765745069766F74003F3Q00128F3Q00013Q00206Q000200122Q000200038Q0002000200064Q000B00013Q000493012Q000B00010012BF3Q00013Q0020995Q000300205C5Q0002001283000200044Q0002012Q000200020006512Q01001000013Q000493012Q0010000100205C00013Q0002001283000300054Q00022Q01000300020006510102001500010001000493012Q0015000100205C000200010002001283000400064Q000201020004000200065B0002003C00013Q000493012Q003C000100205C000300020002001283000500074Q000201030005000200065B0003002C00013Q000493012Q002C000100205C000400030008001283000600094Q000201040006000200065B0004002400013Q000493012Q0024000100209900040003000A2Q0010010400023Q000493012Q002C000100205C0004000300080012830006000B4Q000201040006000200065B0004002C00013Q000493012Q002C000100205C00040003000C2Q0011010400054Q004500045Q00205C000400020008001283000600094Q000201040006000200065B0004003400013Q000493012Q0034000100209900040002000A2Q0010010400023Q000493012Q003C000100205C0004000200080012830006000B4Q000201040006000200065B0004003C00013Q000493012Q003C000100205C00040002000C2Q0011010400054Q004500046Q0027000300034Q0010010300024Q005F012Q00017Q00133Q0003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00737472696E6703053Q006C6F77657203043Q00542Q6F6C03043Q0066696E6403043Q004E616D65026Q00F03F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q00497341030C3Q00556E6571756970542Q6F6C7303043Q007461736B03043Q0077616974029A5Q99A93F03093Q004571756970542Q6F6C01574Q00712Q015Q00202Q0001000100014Q00025Q00202Q00020002000200122Q000400036Q00020004000200062Q0003000B00010001000493012Q000B000100205C000300010004001283000500054Q000201030005000200065B0001001100013Q000493012Q0011000100065B0002001100013Q000493012Q0011000100062E0003001300010001000493012Q001300012Q000601046Q0010010400023Q0012BF000400063Q00201A0004000400074Q00058Q00040002000200202Q00050001000400122Q000700086Q00050007000200062Q0005002A00013Q000493012Q002A00010012BF000600063Q00204100060006000900122Q000700063Q00202Q00070007000700202Q00080005000A4Q0007000200024Q000800043Q00122Q0009000B6Q000A00016Q0006000A000200062Q0006002A00013Q000493012Q002A00012Q0006010600014Q0010010600024Q0027000600063Q0012350007000C3Q00202Q00080002000D4Q000800096Q00073Q000900044Q0043000100205C000C000B000E001283000E00084Q0002010C000E000200065B000C004300013Q000493012Q004300010012BF000C00063Q002041000C000C000900122Q000D00063Q00202Q000D000D000700202Q000E000B000A4Q000D000200024Q000E00043Q00122Q000F000B6Q001000016Q000C0010000200062Q000C004300013Q000493012Q004300012Q00C20006000B3Q000493012Q004500010006190107003000010002000493012Q0030000100062E0006004900010001000493012Q004900012Q000601076Q0010010700023Q00065B0005005100013Q000493012Q0051000100205C00070003000F2Q004401070002000100122Q000700103Q00202Q00070007001100122Q000800126Q00070002000100205C0007000300132Q009C000900066Q0007000900014Q000700016Q000700028Q00017Q00013Q00030D3Q0050726553696D756C6174696F6E00074Q00E38Q00E3000100013Q00209900010001000100067E00023Q000100012Q00E33Q00024Q000F012Q000200012Q005F012Q00013Q00013Q00133Q0003093Q0043686172616374657203073Q0067657467656E7603063Q004E6F636C697003063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C696465010003153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C7565030F3Q004A756D70506F776572546F2Q676C65030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C756500334Q00E37Q0020995Q000100062E3Q000500010001000493012Q000500012Q005F012Q00013Q0012BF000100024Q00D400010001000200209900010001000300065B0001001A00013Q000493012Q001A00010012BF000100043Q00205C00023Q00052Q0094010200034Q00722Q013Q0003000493012Q0018000100205C000600050006001283000800074Q000201060008000200065B0006001800013Q000493012Q0018000100209900060005000800065B0006001800013Q000493012Q001800010030330105000800090006192Q01000F00010002000493012Q000F000100205C00013Q000A0012830003000B4Q00022Q010003000200065B0001003200013Q000493012Q003200010012BF000200024Q00D400020001000200209900020002000C00065B0002002800013Q000493012Q002800010012BF000200024Q00D400020001000200209900020002000E0010632Q01000D00020012BF000200024Q00D400020001000200209900020002000F00065B0002003200013Q000493012Q003200010030332Q01001000110012BF000200024Q00D40002000100020020990002000200130010632Q01001200022Q005F012Q00017Q00093Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030B3Q004368616E6765537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503073Q004A756D70696E6700143Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001300013Q000493012Q001300012Q00E37Q0020995Q00030006512Q01000C00013Q000493012Q000C000100205C00013Q0004001283000300054Q00022Q010003000200065B0001001300013Q000493012Q0013000100205C0002000100060012BF000400073Q0020990004000400080020990004000400092Q000F0102000400012Q005F012Q00017Q00093Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E563203043Q007461736B03043Q0077616974027Q004003083Q0054656C65706F727403043Q0067616D6503073Q00506C616365496400153Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001400013Q000493012Q001400010012BF3Q00014Q00D43Q000100020020995Q000300062E3Q001400010001000493012Q001400010012BF3Q00043Q0020995Q0005001283000100064Q003E012Q000200012Q00E37Q00205C5Q00070012BF000200083Q0020990002000200092Q00E3000300014Q000F012Q000300012Q005F012Q00017Q001C3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B657903093Q0043686172616374657203063Q00697061697273030E3Q0047657444657363656E64616E74732Q033Q0049734103083Q004261736550617274030A3Q0043616E436F2Q6C6964650100030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00476574537461746503043Q00456E756D03113Q0048756D616E6F696453746174655479706503083Q0046722Q6566612Q6C03163Q00412Q73656D626C794C696E65617256656C6F6369747903013Q0059026Q00344003073Q00566563746F72332Q033Q006E657703013Q0058026Q0049C003013Q005A026Q004EC0026Q0034C0004A3Q0012BF3Q00014Q00D43Q000100020020995Q000200062E3Q000B00010001000493012Q000B00010012BF3Q00014Q00D43Q000100020020995Q000300062E3Q000B00010001000493012Q000B00012Q005F012Q00014Q00E37Q0020995Q000400062E3Q001000010001000493012Q001000012Q005F012Q00013Q0012BF000100053Q00205C00023Q00062Q0094010200034Q00722Q013Q0003000493012Q001B000100205C000600050007001283000800084Q000201060008000200065B0006001B00013Q000493012Q001B000100303301050009000A0006192Q01001500010002000493012Q0015000100205C00013Q000B00125D0103000C6Q00010003000200202Q00023Q000D00122Q0004000E6Q00020004000200062Q0001004900013Q000493012Q0049000100065B0002004900013Q000493012Q0049000100205C00030002000F2Q00B800030002000200122Q000400103Q00202Q00040004001100202Q00040004001200062Q0003003200010004000493012Q00320001002099000300010013002099000300030014000EA00115003C00010003000493012Q003C00010012BF000300163Q0020C600030003001700202Q00040001001300202Q00040004001800122Q000500193Q00202Q00060001001300202Q00060006001A4Q00030006000200102Q000100130003000493012Q0049000100209900030001001300209900030003001400268A000300490001001B000493012Q004900010012BF000300163Q0020C600030003001700202Q00040001001300202Q00040004001800122Q0005001C3Q00202Q00060001001300202Q00060006001A4Q00030006000200102Q0001001300032Q005F012Q00017Q000E3Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B657903063Q007475726B657903103Q005475726B657953702Q656456616C7565030A3Q0053702Q656456616C756503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564003B4Q00E37Q0020995Q000100065B3Q003A00013Q000493012Q003A00010012BF3Q00023Q0020995Q0003001283000100044Q003E012Q000200010012C43Q00058Q0001000200206Q000600122Q000100056Q00010001000200202Q00010001000700064Q001200010001000493012Q0012000100065B00013Q00013Q000493014Q00012Q0027000200023Q00065B3Q002300013Q000493012Q0023000100065B0001002300013Q000493012Q002300012Q00E3000300013Q0026460003001F00010008000493012Q001F00010012BF000300054Q00D400030001000200209900030003000900067F0102002200010003000493012Q002200010012BF000300054Q00D400030001000200209900020003000A000493012Q002C000100065B0001002900013Q000493012Q002900010012BF000300054Q00D4000300010002002099000200030009000493012Q002C00010012BF000300054Q00D400030001000200209900020003000A2Q00E3000300023Q00209900030003000B0006510104003300010003000493012Q0033000100205C00040003000C0012830006000D4Q000201040006000200065B00043Q00013Q000493014Q000100209900050004000E00064D00053Q00010002000493014Q00010010630104000E0002000493014Q00012Q005F012Q00017Q002C3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B6579030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F7003093Q00436861726163746572030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403083Q00506F736974696F6E03093Q004D61676E697475646503063Q007475726B65792Q033Q0067656D03073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A026Q00184003023Q006F7303053Q00636C6F636B026Q0008402Q0103043Q007461736B03053Q0064656C6179026Q004E40026Q00E03F03043Q00556E697403043Q004C65727003043Q006D61746803053Q00636C616D70026Q002440026Q00F03F03043Q004D6F7665026Q000C4003063Q00434672616D6503063Q006C2Q6F6B417403013Q0059026Q002040026Q00104003113Q0066697265746F756368696E74657265737403043Q007A65726F01E93Q0012C4000100016Q00010001000200202Q00010001000200122Q000200016Q00020001000200202Q00020002000300062Q0001000B00010001000493012Q000B000100062E0002000B00010001000493012Q000B00012Q005F012Q00013Q00065B0001002300013Q000493012Q002300010012BF000300014Q00D400030001000200209900030003000400062E0003001C00010001000493012Q001C00010012BF000300014Q00D400030001000200209900030003000500062E0003001C00010001000493012Q001C00010012BF000300014Q00D400030001000200209900030003000600065B0003002300013Q000493012Q002300012Q00E300036Q00B900030001000400065B0003002300013Q000493012Q0023000100065B0004002300013Q000493012Q002300012Q005F012Q00014Q00E3000300013Q00209900030003000700062E0003002800010001000493012Q002800012Q005F012Q00013Q00205C00040003000800125D010600096Q00040006000200202Q00050003000A00122Q0007000B6Q00050007000200062Q0004003200013Q000493012Q0032000100062E0005003300010001000493012Q003300012Q005F012Q00013Q00065B0001003900013Q000493012Q003900012Q00E3000600024Q00D400060001000200062E0006003A00010001000493012Q003A00012Q0027000600063Q00065B0002004000013Q000493012Q004000012Q00E3000700034Q00D400070001000200062E0007004100010001000493012Q004100012Q0027000700074Q0027000800093Q00065B0006005800013Q000493012Q0058000100065B0007005800013Q000493012Q00580001002099000A0004000C002099000B0006000C2Q0042000A000A000B002099000A000A000D002099000B0004000C002099000C0007000C2Q0042000B000B000C002099000B000B000D00066C000A00540001000B000493012Q005400012Q00C2000A00064Q00E3000900044Q00C20008000A3Q000493012Q006300012Q00C2000A00074Q00E3000900054Q00C20008000A3Q000493012Q0063000100065B0006005E00013Q000493012Q005E00012Q00C2000A00064Q00E3000900044Q00C20008000A3Q000493012Q0063000100065B0007006300013Q000493012Q006300012Q00C2000A00074Q00E3000900054Q00C20008000A3Q00065B0008006D00013Q000493012Q006D00012Q00E3000A00053Q0006770109006B0001000A000493012Q006B0001001283000A000E3Q00062E000A006C00010001000493012Q006C0001001283000A000F4Q0084010A00063Q00065B000800D600013Q000493012Q00D60001002099000A0008000C002068000B0004000C4Q000A000A000B00122Q000B00103Q00202Q000B000B001100202Q000C000A001200122Q000D00133Q00202Q000E000A00144Q000B000E000200202Q000C000B000D00262Q000C009800010015000493012Q009800012Q00E3000D00073Q00064D000D008500010008000493012Q008500012Q00C2000D00083Q0012BF000E00163Q002099000E000E00172Q00D4000E000100022Q0084010E00084Q0084010D00073Q000493012Q009A00010012BF000D00163Q002079000D000D00174Q000D000100024Q000E00086Q000D000D000E000E2Q0018009A0001000D000493012Q009A00010020810009000800190012BF000D001A3Q002099000D000D001B001283000E001C3Q00067E000F3Q000100022Q00C23Q00094Q00C23Q00084Q0032000D000F00014Q000D000D6Q000D00078Q00013Q00044Q009A00012Q0027000D000D4Q0084010D00073Q000EA0011D00C60001000C000493012Q00C60001002099000D000B001E2Q004E000E00093Q00202Q000E000E001F4Q0010000D3Q00122Q001100203Q00202Q00110011002100202Q00123Q002200122Q001300133Q00122Q001400236Q001100146Q000E3Q00024Q000E00093Q00202Q000E000500244Q001000096Q00118Q000E00110001000E2Q002500C60001000C000493012Q00C600010012BF000E00263Q002099000E000E0027002099000F0004000C0012BF001000103Q00209900100010001100209900110008000C00209900110011001200209900120004000C00209900120012002800209900130008000C0020990013001300142Q004B011000134Q006E010E3Q000200202Q000F0004002600202Q000F000F001F4Q0011000E3Q00122Q001200203Q00202Q00120012002100202Q00133Q002900122Q001400133Q00122Q001500236Q001200154Q004C010F3Q000200106301040026000F002641010C00E80001002A000493012Q00E800010012BF000D002B3Q00065B000D00E800013Q000493012Q00E800010012BF000D002B4Q00CC000E00046Q000F00083Q00122Q001000136Q000D0010000100122Q000D002B6Q000E00046Q000F00083Q00122Q001000236Q000D0010000100044Q00E800012Q0027000A000A4Q00F2000A00076Q000A00093Q00202Q000A000A001F00122Q000C00103Q00202Q000C000C002C00122Q000D00203Q00202Q000D000D002100202Q000E3Q002900122Q000F00133Q00122Q001000234Q004B010D00104Q0014000A3Q00024Q000A00093Q00202Q000A000500244Q000C00096Q000D8Q000A000D00012Q005F012Q00013Q00013Q00015Q00044Q00E38Q00E3000100013Q0020813Q000100012Q005F012Q00017Q00153Q0003083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03053Q00416C697665026Q00444003093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465026Q00104003063Q004D6F7665546F03043Q007461736B03043Q0077616974029A5Q99B93F03043Q004D6F766503043Q007A65726F024D3Q00209900023Q00010012BF000300023Q0020990003000300032Q00D40003000100022Q00C2000400014Q00D400040001000200065B0004003A00013Q000493012Q003A00012Q00E300045Q00209900040004000400065B0004003A00013Q000493012Q003A00010012BF000400023Q0020990004000400032Q00D40004000100022Q004200040004000300268A0004003A00010005000493012Q003A00012Q00E3000400014Q00D40004000100022Q00E3000500023Q00209900050005000600065B0005001D00013Q000493012Q001D00012Q00E3000500023Q00209900050005000600205C000500050007001283000700084Q000201050007000200065B0004003A00013Q000493012Q003A000100062E0005002200010001000493012Q00220001000493012Q003A00010012BF000600093Q0020D700060006000A00202Q00070002000B00202Q00080004000100202Q00080008000B4Q00070007000800122Q0008000C3Q00202Q00090002000D00202Q000A0004000100202Q000A000A000D4Q00090009000A4Q00060009000200202Q00060006000E00262Q000600320001000F000493012Q00320001000493012Q003A000100205C0007000500102Q0028000900026Q00070009000100122Q000700113Q00202Q00070007001200122Q000800136Q00070002000100044Q000400012Q00E3000400023Q00209900040004000600065B0004004300013Q000493012Q004300012Q00E3000400023Q00209900040004000600205C000400040007001283000600084Q000201040006000200065B0004004900013Q000493012Q0049000100205C0005000400140012BF000700093Q0020990007000700152Q000F0105000700012Q00C2000500014Q001C010500014Q004500056Q005F012Q00017Q000C3Q0003063Q0069706169727303073Q0067657467656E76030E3Q0046696E6446697273744368696C642Q033Q0049734103083Q00426173655061727403063Q00434672616D6503083Q004765745069766F74028Q0003043Q007461736B03043Q0077616974029A5Q99B93F010003363Q0012BF000300014Q00C2000400014Q0052000300020005000493012Q003000010012BF000800024Q00D40008000100022Q00C300080008000200062E0008000A00010001000493012Q000A0001000493012Q0032000100205C00083Q00032Q00C2000A00074Q00020108000A000200065B0008003000013Q000493012Q0030000100205C000900080004001283000B00054Q00020109000B000200065B0009001700013Q000493012Q0017000100209900090008000600062E0009001900010001000493012Q0019000100205C0009000800072Q00B70009000200022Q00E3000A6Q00C2000B00093Q00067E000C3Q000100012Q00C23Q00024Q0002010A000C000200062E000A002100010001000493012Q00210001000493012Q00320001001283000A00084Q00E3000B00013Q000680000A00300001000B000493012Q003000010012BF000B00024Q00D4000B000100022Q00C3000B000B000200065B000B003000013Q000493012Q003000010012BF000B00093Q0020BB000B000B000A00122Q000C000B6Q000B0002000100202Q000A000A000B00044Q002200010006190103000400010002000493012Q000400010012BF000300024Q00D400030001000200208100030002000C2Q005F012Q00013Q00013Q00013Q0003073Q0067657467656E7600063Q00121E012Q00018Q000100024Q00019Q003Q00016Q00028Q00017Q00043Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E010003063Q004E6F636C6970000C3Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q000B00013Q000493012Q000B00010012BF3Q00014Q00EE3Q0001000200304Q0002000300124Q00018Q0001000200304Q000400032Q005F012Q00017Q00043Q0003073Q0067657467656E7603083Q004175746F4C69667403043Q007461736B03053Q00737061776E010D3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000C00013Q000493012Q000C00010012BF000100033Q00209900010001000400067E00023Q000100032Q00E38Q00E33Q00014Q00E33Q00024Q003E2Q01000200012Q005F012Q00013Q00013Q000F3Q0003053Q007063612Q6C03073Q0067657467656E7603083Q004175746F4C69667403093Q00436861726163746572030E3Q0046696E6446697273744368696C6403083Q004261636B7061636B03153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03163Q0046696E6446697273744368696C64576869636849734103083Q0048756D616E6F696403093Q004571756970542Q6F6C030A3Q004669726553657276657203043Q007461736B03043Q0077616974029A5Q99B93F00333Q0012BF3Q00013Q00067E00013Q000100012Q00E38Q003E012Q000200010012BF3Q00024Q00D43Q000100020020995Q000300065B3Q003200013Q000493012Q003200012Q00E33Q00013Q0020995Q00042Q00C5000100013Q00202Q00010001000500122Q000300066Q00010003000200064Q002100013Q000493012Q0021000100065B0001002100013Q000493012Q0021000100205C00023Q0007001283000400084Q000201020004000200062E0002002100010001000493012Q0021000100205C000300010009001283000500084Q000201030005000200065B0003002100013Q000493012Q0021000100209900043Q000A00205C00040004000B2Q00C2000600034Q000F0104000600012Q00E3000200023Q00065B0002002800013Q000493012Q002800012Q00E3000200023Q00205C00020002000C2Q003E010200020001000493012Q002C00010012BF000200013Q00067E00030001000100012Q00C28Q003E0102000200010012BF0002000D3Q00205001020002000E00122Q0003000F6Q0002000200019Q0000044Q000400012Q005F012Q00013Q00023Q00083Q00030C3Q0053656E644B65794576656E7403043Q00456E756D03073Q004B6579436F64652Q033Q004F6E6503043Q0067616D6503043Q007461736B03043Q0077616974029A5Q99A93F00174Q0059016Q00206Q00014Q000200013Q00122Q000300023Q00202Q00030003000300202Q0003000300044Q00045Q00122Q000500058Q0005000100124Q00063Q0020995Q0007001283000100084Q003E012Q000200012Q00E37Q00205C5Q00012Q000601025Q0012BF000300023Q0020990003000300030020990003000300042Q000601045Q0012BF000500054Q000F012Q000500012Q005F012Q00017Q00033Q0003153Q0046696E6446697273744368696C644F66436C612Q7303043Q00542Q6F6C03083Q004163746976617465000C4Q00E37Q00065B3Q000700013Q000493012Q000700012Q00E37Q00205C5Q0001001283000200024Q0002012Q0002000200065B3Q000B00013Q000493012Q000B000100205C00013Q00032Q003E2Q01000200012Q005F012Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F50756E636803043Q007461736B03053Q00737061776E010B3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000A00013Q000493012Q000A00010012BF000100033Q00209900010001000400067E00023Q000100012Q00E38Q003E2Q01000200012Q005F012Q00013Q00013Q00083Q0003073Q0067657467656E7603093Q004175746F50756E6368030A3Q004669726553657276657203053Q0050756E6368026Q00F03F03043Q007461736B03043Q0077616974029A5Q99A93F00133Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001200013Q000493012Q001200012Q00E37Q00065B3Q000D00013Q000493012Q000D00012Q00E37Q00205C5Q0003001283000200043Q001283000300054Q000F012Q000300010012BF3Q00063Q0020995Q0007001283000100084Q003E012Q00020001000493014Q00012Q005F012Q00017Q00043Q0003073Q0067657467656E7603093Q004175746F53746F6D7003043Q007461736B03053Q00737061776E010B3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000A00013Q000493012Q000A00010012BF000100033Q00209900010001000400067E00023Q000100012Q00E38Q003E2Q01000200012Q005F012Q00013Q00013Q00073Q0003073Q0067657467656E7603093Q004175746F53746F6D70030A3Q004669726553657276657203053Q0053746F6D7003043Q007461736B03043Q0077616974029A5Q99A93F00123Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001100013Q000493012Q001100012Q00E37Q00065B3Q000C00013Q000493012Q000C00012Q00E37Q00205C5Q0003001283000200044Q000F012Q000200010012BF3Q00053Q0020995Q0006001283000100074Q003E012Q00020001000493014Q00012Q005F012Q00017Q00083Q0003073Q0067657467656E76030B3Q004175746F41697264726F70030F3Q004175746F54652Q7269746F72696573010003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01153Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q001400013Q000493012Q001400010012BF000100014Q00D40001000100020030332Q01000300040012BF000100053Q0020142Q01000100064Q00028Q00010002000100122Q000100073Q00202Q00010001000800067E00023Q000100042Q00E33Q00014Q00E33Q00024Q00E38Q00E33Q00034Q003E2Q01000200012Q005F012Q00013Q00013Q000D3Q0003073Q0067657467656E76030B3Q004175746F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F030C3Q004175746F47656D54772Q656E03063Q00434672616D652Q033Q006E6577028Q00026Q000840026Q002E402Q01029A5Q99C93F003D3Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q003C00013Q000493012Q003C00010012BF3Q00033Q0020995Q0004001283000100054Q003E012Q000200012Q00E38Q00D43Q000100022Q00E3000100014Q00B900010001000200065B00013Q00013Q000493014Q000100065B00023Q00013Q000493014Q000100065B5Q00013Q000493014Q00010012BF000300014Q00D400030001000200209900030003000600062E00033Q00010001000493014Q000100209900030002000700121D000400073Q00202Q00040004000800122Q000500093Q00122Q0006000A3Q00122Q000700096Q0004000700024Q00030003000400104Q0007000300122Q000300033Q00202Q0003000300040012830004000B4Q003E0103000200012Q00E3000300023Q00208100030001000C2Q00E3000300034Q00D40003000100022Q00E300046Q00D400040001000200065B00033Q00013Q000493014Q000100065B00043Q00013Q000493014Q00010012BF000500073Q00206D00050005000800122Q000600093Q00122Q0007000A3Q00122Q000800096Q0005000800024Q00050003000500102Q000400070005001292010500033Q00202Q00050005000400122Q0006000D6Q00050002000100046Q00012Q005F012Q00017Q00073Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F72696573030C3Q004175746F47656D4272696E670100030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01133Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q001200013Q000493012Q001200012Q00E300016Q00BD00010001000100122Q000100016Q00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100063Q00202Q00010001000700067E00023Q000100012Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q000A3Q0003023Q00543103023Q00543203023Q00543303023Q00543403023Q00543503093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0054652Q7269746F72696573030F3Q004175746F54652Q7269746F72696573001A4Q00C93Q00053Q00122Q000100013Q00122Q000200023Q00122Q000300033Q00122Q000400043Q00122Q000500058Q000500010012BF000100063Q00205C000100010007001283000300084Q00022Q010003000200065B0001001200013Q000493012Q001200010012BF000100063Q00209900010001000800205C000100010007001283000300094Q00022Q010003000200065B0001001900013Q000493012Q001900012Q00E300026Q00C2000300014Q00C200045Q0012830005000A4Q000F0102000500012Q005F012Q00017Q00063Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003053Q007461626C6503053Q00636C65617203043Q007461736B03053Q00737061776E01123Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q001100013Q000493012Q001100010012BF000100033Q0020142Q01000100044Q00028Q00010002000100122Q000100053Q00202Q00010001000600067E00023Q000100042Q00E33Q00014Q00E33Q00024Q00E33Q00034Q00E38Q003E2Q01000200012Q005F012Q00013Q00013Q00133Q0003073Q0067657467656E76030D3Q0057616C6B546F41697264726F7003043Q007461736B03043Q0077616974026Q00E03F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q00506172656E7403083Q00506F736974696F6E03093Q004D61676E6974756465026Q00144003063Q004D6F7665546F029A5Q99C93F03043Q004D6F766503073Q00566563746F723303043Q007A65726F026Q002E402Q01004C3Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q004B00013Q000493012Q004B00010012BF3Q00033Q0020995Q0004001283000100054Q003E012Q000200012Q00E37Q0020995Q00062Q00E3000100014Q00D40001000100020006510102001200013Q000493012Q0012000100205C00023Q0007001283000400084Q00020102000400022Q00E3000300024Q00B900030001000400065B00033Q00013Q000493014Q000100065B00043Q00013Q000493014Q000100065B00013Q00013Q000493014Q000100065B00023Q00013Q000493014Q00010012BF000500014Q00D400050001000200209900050005000200065B0005003200013Q000493012Q0032000100209900050003000900065B0005003200013Q000493012Q0032000100209900050001000A00209900060004000A2Q004200050005000600209900050005000B000EA0010C003200010005000493012Q0032000100205C00050002000D00200901070004000A4Q00050007000100122Q000500033Q00202Q00050005000400122Q0006000E6Q00050002000100044Q001C00010012BF000500014Q00D400050001000200209900050005000200065B00053Q00013Q000493014Q000100209900050003000900065B00053Q00013Q000493014Q000100209900050001000A00209900060004000A2Q004200050005000600209900050005000B00264101053Q0001000C000493014Q000100205C00050002000F001218010700103Q00202Q0007000700114Q00050007000100122Q000500033Q00202Q00050005000400122Q000600126Q0005000200014Q000500033Q00202Q00050003001300046Q00012Q005F012Q00017Q000D3Q0003073Q0067657467656E76030B3Q004175746F47656D57616C6B03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D4272696E67010003093Q0057616C6B53702Q6564030A3Q0053702Q656456616C7565030C3Q0057616C6B546F5475726B657903043Q004D6F766503073Q00566563746F723303043Q007A65726F01263Q0012AB2Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000493012Q000A000100205C000200010004001283000400054Q000201020004000200065B3Q001800013Q000493012Q001800012Q00E3000300014Q000A0003000100010012BF000300014Q00D400030001000200303301030006000700065B0002002500013Q000493012Q002500010012BF000300014Q00D4000300010002002099000300030009001063010200080003000493012Q0025000100065B0002002500013Q000493012Q002500010012BF000300014Q00D400030001000200209900030003000A00062E0003002500010001000493012Q0025000100205C00030002000B00123C0005000C3Q00202Q00050005000D4Q0003000500014Q000300023Q00102Q0002000800032Q005F012Q00017Q000F3Q0003073Q0067657467656E76030C3Q0057616C6B546F5475726B657903093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q004175746F47656D4272696E67010003053Q007461626C6503053Q00636C65617203093Q0057616C6B53702Q656403103Q005475726B657953702Q656456616C7565030B3Q004175746F47656D57616C6B03043Q004D6F766503073Q00566563746F723303043Q007A65726F012A3Q0012AB2Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000493012Q000A000100205C000200010004001283000400054Q000201020004000200065B3Q001C00013Q000493012Q001C00012Q00E3000300014Q000A0003000100010012BF000300014Q00D40003000100020030330103000600070012BF000300083Q0020990003000300092Q00E3000400024Q003E01030002000100065B0002002900013Q000493012Q002900010012BF000300014Q00D400030001000200209900030003000B0010630102000A0003000493012Q0029000100065B0002002900013Q000493012Q002900010012BF000300014Q00D400030001000200209900030003000C00062E0003002900010001000493012Q0029000100205C00030002000D00123C0005000E3Q00202Q00050005000F4Q0003000500014Q000300033Q00102Q0002000A00032Q005F012Q00017Q00063Q0003073Q0067657467656E76030F3Q004175746F4461696C7952657761726403073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01163Q0012E6000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D00010001000493012Q000D00012Q005F012Q00013Q0012BF000200053Q00209900020002000600067E00033Q000100042Q00E33Q00014Q00C23Q00014Q00E38Q00E33Q00024Q003E0102000200012Q005F012Q00013Q00013Q00053Q0003053Q007063612Q6C026Q00F03F025Q00C0724003043Q007461736B03043Q0077616974001C3Q00067E5Q000100032Q00E38Q00E33Q00014Q00E33Q00024Q00C200016Q00D400010001000200065B0001001B00013Q000493012Q001B00010012BF000100013Q00067E00020001000100012Q00E33Q00034Q003A00010002000100122Q000100023Q00122Q000200033Q00122Q000300023Q00042Q0001001A00012Q00C200056Q00D400050001000200062E0005001500010001000493012Q00150001000493012Q000400010012BF000500043Q002099000500050005001283000600024Q003E0105000200010004AA000100100001000493012Q000400012Q005F012Q00013Q00023Q00043Q0003073Q0067657467656E76030F3Q004175746F4461696C7952657761726403053Q00416C69766503073Q0073652Q73696F6E00123Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001000013Q000493012Q001000012Q00E37Q0020995Q000300065B3Q001000013Q000493012Q001000012Q00E33Q00014Q00E3000100023Q00209900010001000400064D3Q000F00010001000493012Q000F00012Q00CF8Q0006012Q00014Q0010012Q00024Q005F012Q00017Q00053Q0003073Q0052656D6F74657303063Q0054696D657273030E3Q004765744576656E74526577617264030C3Q00496E766F6B65536572766572030C3Q005468616E6B73676976696E6700084Q0099016Q00206Q000100206Q000200206Q000300202Q00013Q000400122Q000300056Q0001000300016Q00017Q00023Q0003073Q0067657467656E76030A3Q0053702Q656456616C756501043Q0012BF000100014Q00D40001000100020010632Q0100024Q005F012Q00017Q00023Q0003073Q0067657467656E7603103Q005475726B657953702Q656456616C756501043Q0012BF000100014Q00D40001000100020010632Q0100024Q005F012Q00017Q00023Q0003073Q0067657467656E76030D3Q0047656D54772Q656E53702Q656401043Q0012BF000100014Q00D40001000100020010632Q0100024Q005F012Q00017Q00113Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03063Q004E6F636C6970030C3Q004175746F47656D4272696E670100030F3Q004175746F54652Q7269746F72696573030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B657903093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403043Q007461736B03053Q00737061776E01383Q001270000100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200102Q000100033Q00064Q003700013Q000493012Q003700010012BF000100014Q00EE00010001000200302Q00010004000500122Q000100016Q00010001000200302Q0001000600050012BF000100014Q00D400010001000200209900010001000700062E0001001600010001000493012Q001600010012BF000100014Q00D40001000100020020990001000100080012BF000200014Q00EE00020001000200302Q00020007000500122Q000200016Q00020001000200302Q00020008000500065B0001002D00013Q000493012Q002D00012Q00E300025Q0020990002000200090006510103002500010002000493012Q0025000100205C00030002000A0012830005000B4Q000201030005000200065B0003002D00013Q000493012Q002D000100205C00040003000C00123C0006000D3Q00202Q00060006000E4Q0004000600014Q000400013Q00102Q0003000F00040012BF000200103Q00209900020002001100067E00033Q000100062Q00E33Q00024Q00E33Q00034Q00E33Q00044Q00E33Q00054Q00E33Q00064Q00E33Q00074Q003E0102000200012Q005F012Q00013Q00013Q00173Q0003073Q0067657467656E76030C3Q004175746F47656D54772Q656E03093Q0048656172746265617403043Q0057616974030B3Q004175746F41697264726F7003063Q00434672616D652Q033Q006E6577028Q00026Q00084003163Q00412Q73656D626C794C696E65617256656C6F6369747903073Q00566563746F723303173Q00412Q73656D626C79416E67756C617256656C6F6369747903043Q007461736B03043Q0077616974026Q002E402Q01029A5Q99C93F03083Q00506F736974696F6E030D3Q0047656D54772Q656E53702Q656403093Q004D61676E697475646503043Q00556E697403063Q004E6F636C6970012Q00783Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q006F00013Q000493012Q006F00012Q00E37Q002085014Q000300206Q00046Q000200024Q000100016Q00010001000200062Q00013Q00013Q000493014Q00012Q00E3000200024Q005B01020001000300122Q000400016Q00040001000200202Q00040004000500062Q0004004A00013Q000493012Q004A000100065B0002004A00013Q000493012Q004A000100065B0003004A00013Q000493012Q004A0001002099000400030006001262000500063Q00202Q00050005000700122Q000600083Q00122Q000700093Q00122Q000800086Q0005000800024Q00040004000500102Q00010006000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000A000400122Q0004000B3Q00202Q00040004000700122Q000500083Q00122Q000600083Q00122Q000700086Q00040007000200102Q0001000C000400122Q0004000D3Q00202Q00040004000E00122Q0005000F6Q0004000200014Q000400033Q00202Q0004000200104Q000400046Q0004000100024Q000500016Q00050001000200062Q00043Q00013Q000493014Q000100065B00053Q00013Q000493014Q00010012BF000600063Q00206D00060006000700122Q000700083Q00122Q000800093Q00122Q000900086Q0006000900024Q00060004000600102Q0005000600060012920106000D3Q00202Q00060006000E00122Q000700116Q00060002000100046Q00012Q00E3000400054Q00D400040001000200065B00043Q00013Q000493014Q00010020990005000400120020990006000100122Q00420005000500060012BF000600014Q00D40006000100020020990006000600132Q0049010600063Q00209900070005001400066C0007005B00010006000493012Q005B00010020990007000400060010632Q0100060007000493012Q006000010020990007000100060020990008000500152Q00490108000800062Q00DA0007000700080010632Q01000600070012BF0007000B3Q00207700070007000700122Q000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000A000700122Q0007000B3Q00202Q00070007000700122Q000800083Q00122Q000900083Q00122Q000A00086Q0007000A000200102Q0001000C000700046Q00010012BF3Q00014Q00D43Q000100020020995Q000200062E3Q007700010001000493012Q007700010012BF3Q00014Q00D43Q00010002003033012Q001600172Q005F012Q00017Q000E3Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03053Q00737061776E03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F726564010001273Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100012Q00E38Q003E2Q0100020001000493012Q002600010012BF000100053Q00205C000100010006001283000300074Q00022Q010003000200065B0001002600013Q000493012Q002600010012BF000100083Q0012A9010200053Q00202Q00020002000700202Q0002000200094Q000200036Q00013Q000300044Q0024000100205C00060005000A0012830008000B4Q000201060008000200065B0006002400013Q000493012Q0024000100205C0006000500060012830008000C4Q000201060008000200065B0006002400013Q000493012Q0024000100209900060005000C0030330106000D000E0006192Q01001800010002000493012Q001800012Q005F012Q00013Q00013Q00143Q0003073Q0067657467656E7603093Q00426F2Q734272696E6703043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403063Q00434672616D652Q033Q006E6577028Q00026Q001AC0026Q001EC003083Q00416E63686F7265642Q0100343Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q003300013Q000493012Q003300010012BF3Q00033Q00200C014Q000400122Q000100058Q000200019Q006Q0001000200066Q00013Q000493014Q00010012BF000100063Q00205C000100010007001283000300084Q00022Q010003000200065B00013Q00013Q000493014Q00010012BF000100093Q0012A9010200063Q00202Q00020002000800202Q00020002000A4Q000200036Q00013Q000300044Q0030000100205C00060005000B0012830008000C4Q000201060008000200065B0006003000013Q000493012Q0030000100205C0006000500070012830008000D4Q000201060008000200065B0006003000013Q000493012Q0030000100209900060005000D00205700073Q000E00122Q0008000E3Q00202Q00080008000F00122Q000900103Q00122Q000A00113Q00122Q000B00126Q0008000B00024Q00070007000800102Q0006000E000700202Q00060005000D00302Q0006001300140006192Q01001A00010002000493012Q001A0001000493014Q00012Q005F012Q00017Q00043Q0003073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303043Q007461736B03053Q00737061776E010C3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100022Q00E38Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q001B3Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q0052696768744C6F7765724C656703103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E657703083Q00506F736974696F6E03073Q00566563746F7233026Q002E40027Q0040028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03073Q0067657467656E76030A3Q0057616C6B546F426F2Q7303093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403063Q004D6F7665546F00723Q0012223Q00013Q00206Q000200122Q000200038Q0002000200064Q000700010001000493012Q000700012Q005F012Q00014Q0027000100013Q001235000200043Q00202Q00033Q00054Q000300046Q00023Q000400044Q0023000100205C000700060006001283000900074Q000201070009000200065B0007002300013Q000493012Q0023000100205C000700060002001283000900084Q000201070009000200067F2Q01002000010007000493012Q0020000100205C000700060002001283000900094Q000201070009000200067F2Q01002000010007000493012Q0020000100205C00070006000A0012830009000B4Q00020107000900022Q00C2000100073Q00065B0001002300013Q000493012Q00230001000493012Q002500010006190102000D00010002000493012Q000D00012Q00E300026Q00D400020001000200065B0002003B00013Q000493012Q003B000100065B0001003B00013Q000493012Q003B00010012BF0003000C3Q00204601030003000D00202Q00040001000E00122Q0005000F3Q00202Q00050005000D00122Q000600103Q00122Q000700113Q00122Q000800126Q0005000800024Q0004000400054Q00030002000200102Q0002000C000300122Q000300133Q00202Q00030003001400122Q000400156Q0003000200010012BF000300164Q00D400030001000200209900030003001700065B0003007100013Q000493012Q007100010012BF000300133Q00207501030003001400122Q000400156Q0003000200014Q000300013Q00202Q00030003001800062Q0004004B00010003000493012Q004B000100205C0004000300190012830006001A4Q00020104000600022Q0027000500053Q001235000600043Q00202Q00073Q00054Q000700086Q00063Q000800044Q0067000100205C000B000A0006001283000D00074Q0002010B000D000200065B000B006700013Q000493012Q0067000100205C000B000A0002001283000D00084Q0002010B000D000200067F010500640001000B000493012Q0064000100205C000B000A0002001283000D00094Q0002010B000D000200067F010500640001000B000493012Q0064000100205C000B000A000A001283000D000B4Q0002010B000D00022Q00C20005000B3Q00065B0005006700013Q000493012Q00670001000493012Q006900010006190106005100010002000493012Q0051000100065B0004003B00013Q000493012Q003B000100065B0005003B00013Q000493012Q003B000100205C00060004001B00209900080005000E2Q000F010600080001000493012Q003B00012Q005F012Q00017Q00063Q0003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C030A3Q0057616C6B546F426F2Q73010003043Q007461736B03053Q00737061776E010E3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000D00013Q000493012Q000D00010012BF000100014Q00D40001000100020030332Q01000300040012BF000100053Q00209900010001000600067E00023Q000100012Q00E38Q003E2Q01000200012Q005F012Q00013Q00013Q00153Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03043Q007461736B03043Q0077616974027B14AE47E17A843F03063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03103Q0048756D616E6F6964522Q6F745061727403163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403063Q00434672616D652Q033Q006E6577028Q00026Q000C4003083Q0056656C6F6369747903073Q00566563746F723300413Q0012223Q00013Q00206Q000200122Q000200038Q0002000200064Q000700010001000493012Q000700012Q005F012Q00013Q0012BF000100044Q00D400010001000200209900010001000500065B0001004000013Q000493012Q004000010012BF000100063Q002099000100010007001283000200084Q003E2Q01000200012Q00E300016Q00D40001000100022Q0027000200023Q001235000300093Q00202Q00043Q000A4Q000400056Q00033Q000500044Q0029000100205C00080007000B001283000A000C4Q00020108000A000200065B0008002900013Q000493012Q0029000100205C000800070002001283000A000D4Q00020108000A000200067F0102002600010008000493012Q0026000100205C00080007000E001283000A000F4Q00020108000A00022Q00C2000200083Q00065B0002002900013Q000493012Q00290001000493012Q002B00010006190103001800010002000493012Q0018000100065B0001000700013Q000493012Q0007000100065B0002000700013Q000493012Q0007000100209900030002001000121D000400103Q00202Q00040004001100122Q000500123Q00122Q000600123Q00122Q000700136Q0004000700024Q00030003000400102Q00010010000300122Q000300153Q00202Q000300030011001283000400123Q001283000500123Q001283000600124Q00020103000600020010632Q0100140003000493012Q000700012Q005F012Q00017Q00023Q0003073Q0067657467656E7603103Q0053656C6563746564452Q67496E64657801043Q0012BF000100014Q00D40001000100020010632Q0100024Q005F012Q00017Q00043Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q6703043Q007461736B03053Q00737061776E010D3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000C00013Q000493012Q000C00010012BF000100033Q00209900010001000400067E00023Q000100032Q00E38Q00E33Q00014Q00E33Q00024Q003E2Q01000200012Q005F012Q00013Q00013Q000B3Q0003073Q0067657467656E7603143Q004175746F486174636853656C6563746564452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q003000013Q000493012Q003000012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002A00013Q000493012Q002A00010012BF000100014Q00D400010001000200209900010001000700062E0001002300010001000493012Q00230001001283000100083Q0012BF000200093Q00209900020002000A00067E00033Q000100022Q00C28Q00C23Q00014Q003E0102000200012Q00F700015Q0012BF000100093Q0020572Q010001000B4Q000200026Q0001000200019Q0000046Q00012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012BF3Q00013Q00067E00013Q000100022Q00E38Q00E33Q00014Q003E012Q000200012Q005F012Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00084003073Q0049736C616E647300074Q00827Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030F3Q004175746F48617463685065724F6E6503043Q007461736B03053Q00737061776E010D3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000C00013Q000493012Q000C00010012BF000100033Q00209900010001000400067E00023Q000100032Q00E38Q00E33Q00014Q00E33Q00024Q003E2Q01000200012Q005F012Q00013Q00013Q000B3Q0003073Q0067657467656E76030F3Q004175746F48617463685065724F6E65030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703103Q0053656C6563746564452Q67496E646578026Q00F03F03043Q007461736B03053Q00737061776E03043Q007761697400313Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q003000013Q000493012Q003000012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002A00013Q000493012Q002A00010012BF000100014Q00D400010001000200209900010001000700062E0001002300010001000493012Q00230001001283000100083Q0012BF000200093Q00209900020002000A00067E00033Q000100022Q00C28Q00C23Q00014Q003E0102000200012Q00F700015Q0012BF000100093Q0020572Q010001000B4Q000200026Q0001000200019Q0000046Q00012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012BF3Q00013Q00067E00013Q000100022Q00E38Q00E33Q00014Q003E012Q000200012Q005F012Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572026Q00F03F03073Q0049736C616E647300074Q00827Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00023Q0003053Q007063612Q6C2Q01000C3Q0012BF3Q00013Q00067E00013Q000100012Q00E38Q00523Q000200010006510102000A00013Q000493012Q000A00010026CD0001000900010002000493012Q000900012Q00CF00026Q0006010200014Q0010010200024Q005F012Q00013Q00013Q00043Q00030A3Q00506C617965724461746103083Q0047616D6570612Q7303053Q004F70656E3303053Q0056616C756500074Q002F7Q00206Q000100206Q000200206Q000300206Q00046Q00028Q00017Q00043Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q6703043Q007461736B03053Q00737061776E010C3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100022Q00E38Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q00093Q0003073Q0067657467656E7603103Q004175746F486174636843756265452Q67030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0050657473030B3Q005075726368617365452Q6703043Q007461736B03053Q00737061776E03043Q007761697400283Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q002700013Q000493012Q002700012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002200013Q000493012Q002200010012BF000100073Q00209900010001000800067E00023Q000100012Q00C28Q003E2Q01000200010012BF000100073Q0020990001000100092Q000A0001000100012Q00F77Q000493014Q00012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012BF3Q00013Q00067E00013Q000100012Q00E38Q003E012Q000200012Q005F012Q00013Q00013Q00043Q00030C3Q00496E766F6B65536572766572026Q00F03F026Q00084003093Q0043756265576F726C6400074Q002A7Q00206Q000100122Q000200023Q00122Q000300033Q00122Q000400048Q000400016Q00017Q00163Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C6403093Q0052696E674172656173030B3Q0052616E676553797374656D03063Q0053657276657203043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001503Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q004A00013Q000493012Q004A00010012BF000100033Q00205C000100010004001283000300054Q00022Q010003000200065B0001002100013Q000493012Q002100010012BF000100033Q0020B600010001000500202Q00010001000400122Q000300066Q00010003000200062Q0001002100013Q000493012Q002100010012BF000100033Q0020BA00010001000500202Q00010001000600202Q00010001000400122Q000300076Q00010003000200065B0001002100013Q000493012Q002100010012BF000100033Q00205E2Q010001000500202Q00010001000600202Q00010001000700202Q00010001000400122Q000300086Q0001000300022Q00E300026Q00D400020001000200065B0002003F00013Q000493012Q003F000100065B0001003F00013Q000493012Q003F000100205C0003000100090012830005000A4Q000201030005000200065B0003003000013Q000493012Q0030000100205C00030001000B2Q00B700030002000200062E0003003100010001000493012Q0031000100209900030001000C0012BF0004000C3Q0020A200040004000D00122Q0005000E3Q00122Q0006000F3Q00122Q0007000E6Q0004000700024Q00040003000400102Q0002000C000400122Q000400103Q00202Q00040004001100122Q000500126Q00040002000100302Q00020013001400044Q0042000100065B0002004200013Q000493012Q004200010030330102001300140012BF000300103Q00209900030003001500067E00043Q000100032Q00E33Q00014Q00E33Q00024Q00E33Q00034Q003E010300020001000493012Q004F00012Q00E300016Q00D400010001000200065B0001004F00013Q000493012Q004F00010030332Q01001300162Q005F012Q00013Q00013Q00083Q0003073Q0067657467656E7603083Q004175746F53652Q6C03093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001F00013Q000493012Q001F00012Q00E37Q0020C05Q000300206Q00046Q000200016Q00013Q00064Q001700010001000493012Q001700012Q00E33Q00023Q00205C5Q0005001283000200064Q0002012Q0002000200065B3Q001700013Q000493012Q001700012Q00E33Q00023Q0020995Q000600205C5Q0005001283000200074Q0002012Q0002000200065B3Q001D00013Q000493012Q001D00010012BF000100083Q00067E00023Q000100012Q00C28Q003E2Q01000200012Q00F77Q000493014Q00012Q005F012Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q00E37Q00205C5Q00012Q003E012Q000200012Q005F012Q00017Q00043Q0003073Q0067657467656E76030E3Q004175746F4275795765696768747303043Q007461736B03053Q00737061776E010C3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100022Q00E38Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q000A3Q0003073Q0067657467656E76030E3Q004175746F42757957656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q002800013Q000493012Q002800012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002200013Q000493012Q002200010012BF000100073Q00209900010001000800067E00023Q000100012Q00C28Q003E2Q01000200010012BF000100073Q0020502Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012BF3Q00013Q00067E00013Q000100012Q00E38Q003E012Q000200012Q005F012Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q0057656967687403073Q0049736C616E647300064Q0074016Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q00043Q0003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E010C3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100022Q00E38Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q000A3Q0003073Q0067657467656E76030A3Q004175746F427579444E41030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q002800013Q000493012Q002800012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002200013Q000493012Q002200010012BF000100073Q00209900010001000800067E00023Q000100012Q00C28Q003E2Q01000200010012BF000100073Q0020502Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q005F012Q00013Q00013Q00063Q00026Q00F03F026Q005E4003073Q0067657467656E76030A3Q004175746F427579444E4103043Q007461736B03053Q00737061776E00133Q0012833Q00013Q001283000100023Q001283000200013Q00049E012Q001200010012BF000400034Q00D400040001000200209900040004000400062E0004000A00010001000493012Q000A0001000493012Q001200010012BF000400053Q00209900040004000600067E00053Q000100022Q00E38Q00C23Q00034Q003E0104000200012Q00F700035Q0004AA3Q000400012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012BF3Q00013Q00067E00013Q000100022Q00E38Q00E33Q00014Q003E012Q000200012Q005F012Q00013Q00013Q00033Q00030C3Q00496E766F6B655365727665722Q033Q00444E4103073Q0049736C616E647300074Q00827Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E010C3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100022Q00E38Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q000A3Q0003073Q0067657467656E76030D3Q004175746F427579426F64696573030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030F3Q0052657175657374507572636861736503043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q002800013Q000493012Q002800012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002200013Q000493012Q002200010012BF000100073Q00209900010001000800067E00023Q000100012Q00C28Q003E2Q01000200010012BF000100073Q0020502Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q005F012Q00013Q00013Q00073Q00027Q0040025Q00802Q40026Q00F03F03073Q0067657467656E76030D3Q004175746F427579426F6469657303043Q007461736B03053Q00737061776E00133Q0012833Q00013Q001283000100023Q001283000200033Q00049E012Q001200010012BF000400044Q00D400040001000200209900040004000500062E0004000A00010001000493012Q000A0001000493012Q001200010012BF000400063Q00209900040004000700067E00053Q000100022Q00E38Q00C23Q00034Q003E0104000200012Q00F700035Q0004AA3Q000400012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00063Q0012BF3Q00013Q00067E00013Q000100022Q00E38Q00E33Q00014Q003E012Q000200012Q005F012Q00013Q00013Q00033Q00030C3Q00496E766F6B65536572766572030B3Q00426F64795570677261646503073Q0049736C616E647300074Q00827Q00206Q00014Q000200013Q00122Q000300023Q00122Q000400038Q000400016Q00017Q00043Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B65745765696768747303043Q007461736B03053Q00737061776E010C3Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q000B00013Q000493012Q000B00010012BF000100033Q00209900010001000400067E00023Q000100022Q00E38Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q000A3Q0003073Q0067657467656E7603193Q004175746F42757953757065726D61726B657457656967687473030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q0053686F70030D3Q0052657175657374427579412Q6C03043Q007461736B03053Q00737061776E03043Q0077616974026Q00E03F00293Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q002800013Q000493012Q002800012Q00E37Q00062E3Q001B00010001000493012Q001B00012Q00E33Q00013Q00205C5Q0003001283000200044Q0002012Q0002000200065B3Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020B65Q000400206Q000300122Q000200058Q0002000200064Q001B00013Q000493012Q001B00012Q00E33Q00013Q0020BA5Q000400206Q000500206Q000300122Q000200068Q0002000200065B3Q002200013Q000493012Q002200010012BF000100073Q00209900010001000800067E00023Q000100012Q00C28Q003E2Q01000200010012BF000100073Q0020502Q010001000900122Q0002000A6Q0001000200019Q0000046Q00012Q005F012Q00013Q00013Q00013Q0003053Q007063612Q6C00053Q0012BF3Q00013Q00067E00013Q000100012Q00E38Q003E012Q000200012Q005F012Q00013Q00013Q00033Q00030C3Q00496E766F6B6553657276657203063Q00576569676874030B3Q0053757065726D61726B657400064Q0074016Q00206Q000100122Q000200023Q00122Q000300038Q000300016Q00017Q001A3Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B657403053Q0053686F707303093Q0052696E67417265617303063Q00536572766572030B3Q0052616E676553797374656D030F3Q0053757065726D61726B657453652Q6C03043Q0053652Q6C2Q033Q0049734103053Q004D6F64656C03083Q004765745069766F7403063Q00434672616D652Q033Q006E6577028Q00026Q00084003043Q007461736B03043Q0077616974029A5Q99B93F03083Q00416E63686F7265642Q0103053Q00737061776E010001633Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q005D00013Q000493012Q005D00010012BF000100033Q00205C000100010004001283000300054Q00022Q01000300020006510102000E00010001000493012Q000E000100205C000200010004001283000400064Q00020102000400022Q0027000300033Q00065B0002003400013Q000493012Q0034000100205C000400020004001283000600074Q000201040006000200062E0004001900010001000493012Q0019000100205C000400020004001283000600084Q00020104000600020006510105002900010004000493012Q0029000100205C000500040004001283000700094Q000201050007000200062E0005002900010001000493012Q0029000100205C0005000400040012830007000A4Q000201050007000200065B0005002900013Q000493012Q0029000100209900050004000A00205C000500050004001283000700094Q000201050007000200065B0005003400013Q000493012Q0034000100205C0006000500040012830008000B4Q000201060008000200067F0103003400010006000493012Q0034000100205C0006000500040012830008000C4Q00020106000800022Q00C2000300064Q00E300046Q00D400040001000200065B0004005200013Q000493012Q0052000100065B0003005200013Q000493012Q0052000100205C00050003000D0012830007000E4Q000201050007000200065B0005004300013Q000493012Q0043000100205C00050003000F2Q00B700050002000200062E0005004400010001000493012Q004400010020990005000300100012BF000600103Q0020A200060006001100122Q000700123Q00122Q000800133Q00122Q000900126Q0006000900024Q00060005000600102Q00040010000600122Q000600143Q00202Q00060006001500122Q000700166Q00060002000100302Q00040017001800044Q0055000100065B0004005500013Q000493012Q005500010030330104001700180012BF000500143Q00209900050005001900067E00063Q000100032Q00E33Q00014Q00E33Q00024Q00E33Q00034Q003E010500020001000493012Q006200012Q00E300016Q00D400010001000200065B0001006200013Q000493012Q006200010030332Q010017001A2Q005F012Q00013Q00013Q00083Q0003073Q0067657467656E7603133Q004175746F53652Q6C53757065726D61726B657403093Q0048656172746265617403043Q0057616974030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303133Q0053652Q6C537472656E6774685265717565737403053Q007063612Q6C00203Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001F00013Q000493012Q001F00012Q00E37Q0020C05Q000300206Q00046Q000200016Q00013Q00064Q001700010001000493012Q001700012Q00E33Q00023Q00205C5Q0005001283000200064Q0002012Q0002000200065B3Q001700013Q000493012Q001700012Q00E33Q00023Q0020995Q000600205C5Q0005001283000200074Q0002012Q0002000200065B3Q001D00013Q000493012Q001D00010012BF000100083Q00067E00023Q000100012Q00C28Q003E2Q01000200012Q00F77Q000493014Q00012Q005F012Q00013Q00013Q00013Q00030A3Q004669726553657276657200044Q00E37Q00205C5Q00012Q003E012Q000200012Q005F012Q00017Q00073Q0003073Q0067657467656E76031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D4272696E670100030B3Q004175746F41697264726F7003043Q007461736B03053Q00737061776E01133Q0012BF000100014Q00D40001000100020010632Q0100023Q00065B3Q001200013Q000493012Q001200012Q00E300016Q00BD00010001000100122Q000100016Q00010001000200302Q00010003000400122Q000100016Q00010001000200302Q00010005000400122Q000100063Q00202Q00010001000700067E00023Q000100012Q00E33Q00014Q003E2Q01000200012Q005F012Q00013Q00013Q00093Q0003023Q00543103023Q00543203023Q00543303093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E73030B3Q0053757065726D61726B6574030B3Q0054652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573001B4Q0032012Q00033Q001283000100013Q001283000200023Q001283000300034Q0012012Q000300010012BF000100043Q00205C000100010005001283000300064Q00022Q01000300020006510102000E00010001000493012Q000E000100205C000200010005001283000400074Q00020102000400020006510103001300010002000493012Q0013000100205C000300020005001283000500084Q000201030005000200065B0003001A00013Q000493012Q001A00012Q00E300046Q00C2000500034Q00C200065Q001283000700094Q000F0104000700012Q005F012Q00017Q00043Q0003073Q00456E61626C656403073Q0067657467656E76030B3Q0057616C6B546F4375626573010001084Q00E300015Q0010632Q0100013Q00065B3Q000700013Q000493012Q000700010012BF000100024Q00D40001000100020030332Q01000300042Q005F012Q00017Q00013Q0003053Q0053702Q656401034Q00E300015Q0010632Q0100014Q005F012Q00017Q00053Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q0044696D656E73696F6E7303093Q0043756265576F726C6403063Q0047726F756E6400103Q00128C012Q00013Q00206Q000200122Q000200038Q0002000200062Q0001000900013Q000493012Q0009000100205C00013Q0002001283000300044Q00022Q01000300020006510102000E00010001000493012Q000E000100205C000200010002001283000400054Q00020102000400022Q0010010200024Q005F012Q00017Q00083Q00030F3Q0067657447726F756E64466F6C6465722Q033Q0049734103083Q00426173655061727403163Q0046696E6446697273744368696C64576869636849734103083Q00506F736974696F6E03013Q005903043Q0053697A65027Q0040001E4Q00E37Q0020995Q00012Q00D43Q0001000200062E3Q000700010001000493012Q000700012Q0027000100014Q00102Q0100023Q00205C00013Q0002001283000300034Q00022Q010003000200065B0001000E00013Q000493012Q000E000100067F2Q01001200013Q000493012Q0012000100205C00013Q0004001283000300034Q0006010400014Q00022Q010004000200062E0001001600010001000493012Q001600012Q0027000200024Q0010010200023Q0020990002000100050020B500020002000600202Q00030001000700202Q00030003000600202Q0003000300084Q0002000200034Q000200028Q00017Q00133Q0003093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030F3Q005F4C6F63616C4375626544726F7073030C3Q0067657447726F756E64546F7003063Q00697061697273030B3Q004765744368696C6472656E03093Q00626C61636B6C697374030C3Q005069636B7570486974626F78030A3Q00437562655374726F6B6503163Q0046696E6446697273744368696C64576869636849734103083Q00426173655061727403083Q00506F736974696F6E03013Q0059027Q00C003093Q006D617848656967687403053Q007461626C6503063Q00696E7365727403043Q006375626503043Q0070617274003B3Q00122D012Q00013Q00206Q000200122Q000200038Q000200024Q00015Q00202Q0001000100044Q00010001000200064Q000B00013Q000493012Q000B000100062E0001000D00010001000493012Q000D00012Q003201026Q0010010200024Q003201025Q001235000300053Q00202Q00043Q00064Q000400056Q00033Q000500044Q003700012Q00E300085Q0020990008000800072Q00C300080008000700062E0008003700010001000493012Q0037000100205C000800070002001283000A00084Q00020108000A000200062E0008002500010001000493012Q0025000100205C000800070002001283000A00094Q00020108000A000200062E0008002500010001000493012Q0025000100205C00080007000A001283000A000B4Q00020108000A000200065B0008003700013Q000493012Q0037000100209900090008000C00209900090009000D2Q0042000900090001000E9B000E003700010009000493012Q003700012Q00E3000A5Q002099000A000A000F00066C000900370001000A000493012Q003700010012BF000A00103Q00204F010A000A00114Q000B00026Q000C3Q000200102Q000C0012000700102Q000C001300084Q000A000C00010006190103001300010002000493012Q001300012Q0010010200024Q005F012Q00017Q00063Q0003073Q00566563746F72332Q033Q006E657703013Q0058028Q0003013Q005A03093Q004D61676E6974756465020D3Q0012D2000200013Q00202Q00020002000200202Q00033Q000300202Q0004000100034Q00030003000400122Q000400043Q00202Q00053Q000500202Q0006000100054Q0005000500064Q00020005000200202Q0002000200064Q000200028Q00017Q00083Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303073Q0073652Q73696F6E026Q00F03F03073Q00456E61626C6564010003043Q007461736B03053Q00737061776E011A3Q0012E6000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D00010001000493012Q000D00012Q005F012Q00014Q00E3000200013Q0030330102000500060012BF000200073Q00209900020002000800067E00033Q000100062Q00E33Q00024Q00C23Q00014Q00E38Q00E33Q00034Q00E33Q00044Q00E33Q00054Q003E0102000200012Q005F012Q00013Q00013Q00203Q0003043Q006D61746803043Q0068756765028Q0003043Q007461736B03043Q0077616974029A5Q99B93F03093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q006375626503063Q00506172656E7403043Q007061727403063Q00697061697273030D3Q00676574466C2Q6F72437562657303083Q00506F736974696F6E03023Q006F7303053Q00636C6F636B03073Q0067657467656E76030D3Q0057616C6B437562654465627567030C3Q0067657447726F756E64546F7003053Q007072696E7403353Q005B57616C6B437562655D2074617267657420257320646973743D252E31662068656967687441626F766547726F756E643D252E316603063Q00666F726D617403043Q004E616D6503013Q005903063Q004D6F7665546F027Q0040026Q001840026Q00244003043Q004D6F766503073Q00566563746F723303043Q007A65726F00983Q0012BF000100013Q002099000100010002001283000200033Q00067E00033Q000100032Q00E38Q00E33Q00014Q00E33Q00023Q00067E00040001000100012Q00E33Q00024Q00C2000500034Q00D400050001000200065B0005008800013Q000493012Q008800010012BF000500043Q00207501050005000500122Q000600066Q0005000200014Q000500033Q00202Q00050005000700062Q0006001800010005000493012Q0018000100205C000600050008001283000800094Q00020106000800022Q00E3000700044Q00D400070001000200065B0006001E00013Q000493012Q001E000100062E0007002000010001000493012Q002000012Q00277Q000493012Q0009000100065B3Q002B00013Q000493012Q002B000100209900083Q000A00209900080008000B00065B0008002A00013Q000493012Q002A000100209900083Q000C00209900080008000B00062E0008002B00010001000493012Q002B00012Q00277Q00062E3Q006300010001000493012Q006300012Q0027000800083Q001240010900013Q00202Q00090009000200122Q000A000D6Q000B00023Q00202Q000B000B000E4Q000B00016Q000A3Q000C00044Q004000012Q00E3000F00053Q00206801100007000F00202Q0011000E000C00202Q00110011000F4Q000F0011000200062Q000F004000010009000493012Q004000012Q00C20010000E4Q00C20009000F4Q00C2000800103Q000619010A003600010002000493012Q0036000100065B0008006300013Q000493012Q006300012Q00C2000A00084Q00C2000B00093Q0012BF000C00103Q002099000C000C00112Q00D4000C000100022Q00C20002000C4Q00C20001000B4Q00C23Q000A3Q0012BF000A00124Q00D4000A00010002002099000A000A001300065B000A006300013Q000493012Q006300012Q00E3000A00023Q002099000A000A00142Q00D4000A0001000200062E000A005700010001000493012Q00570001001283000A00033Q0012BF000B00153Q001239000C00163Q00202Q000C000C001700202Q000E0008000A00202Q000E000E00184Q000F00093Q00202Q00100008000C00202Q00100010000F00202Q0010001000194Q00100010000A4Q000C00106Q000B3Q000100065B3Q008400013Q000493012Q008400012Q00E3000800053Q00206A00090007000F00202Q000A3Q000C00202Q000A000A000F4Q0008000A000200202Q00090006001A00202Q000B3Q000C00202Q000B000B000F4Q0009000B000100202Q00090001001B00062Q0008007800010009000493012Q007800012Q00C2000900083Q00120D000A00103Q00202Q000A000A00114Q000A000100024Q0002000A6Q000100093Q00044Q000900010012BF000900103Q0020990009000900112Q00D40009000100022Q0042000900090002000EA0011C000900010009000493012Q000900012Q00C2000900043Q0020A7000A3Q000A00122Q000B001D6Q0009000B00019Q0000044Q0009000100205C00080006001A002099000A0007000F2Q000F0108000A0001000493012Q000900012Q00E3000500033Q00209900050005000700065B0005009100013Q000493012Q009100012Q00E3000500033Q00209900050005000700205C000500050008001283000700094Q000201050007000200065B0005009700013Q000493012Q0097000100205C00060005001E0012BF0008001F3Q0020990008000800202Q000F0106000800012Q005F012Q00013Q00023Q00043Q0003073Q0067657467656E76030B3Q0057616C6B546F437562657303053Q00416C69766503073Q0073652Q73696F6E00123Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001000013Q000493012Q001000012Q00E37Q0020995Q000300065B3Q001000013Q000493012Q001000012Q00E33Q00014Q00E3000100023Q00209900010001000400064D3Q000F00010001000493012Q000F00012Q00CF8Q0006012Q00014Q0010012Q00024Q005F012Q00017Q000B3Q0003073Q0067657467656E76030D3Q0057616C6B43756265446562756703053Q007072696E74031E3Q005B57616C6B437562655D20736B692Q70696E6720737475636B206375626503043Q004E616D652Q033Q00666F7203013Q007303093Q00626C61636B6C6973742Q0103043Q007461736B03053Q0064656C617902173Q0012BF000200014Q00D400020001000200209900020002000200065B0002000C00013Q000493012Q000C00010012BF000200033Q001283000300043Q00209900043Q0005001283000500064Q00C2000600013Q001283000700074Q000F0102000700012Q00E300025Q00207500020002000800202Q00023Q000900122Q0002000A3Q00202Q00020002000B4Q000300013Q00067E00043Q000100022Q00E38Q00C28Q000F0102000400012Q005F012Q00013Q00013Q00023Q0003093Q00626C61636B6C6973742Q00054Q00E37Q0020995Q00012Q00E3000100013Q0020813Q000100022Q005F012Q00017Q00033Q0003073Q0067657467656E7603093Q0044697361626C65334403153Q00536574336452656E646572696E67456E61626C656401083Q0012732Q0100016Q00010001000200102Q000100026Q00015Q00202Q0001000100034Q00038Q0001000300016Q00017Q00023Q0003073Q0067657467656E76030A3Q004175746F52656A6F696E01043Q0012BF000100014Q00D40001000100020010632Q0100024Q005F012Q00017Q00033Q00034Q0003053Q007063612Q6C2Q0100153Q0012833Q00013Q0012BF000100023Q00067E00023Q000100022Q00C28Q00E38Q003E2Q01000200010026CD3Q000A00010001000493012Q000A00012Q00062Q0100014Q00102Q0100023Q0012BF000100023Q0002E9000200014Q00520001000200020006510103001300010001000493012Q001300010026CD0002001200010003000493012Q001200012Q00CF00036Q0006010300014Q0010010300024Q005F012Q00013Q00023Q00013Q00030F3Q00476574452Q726F724D652Q7361676500054Q000A012Q00013Q00206Q00016Q000200029Q006Q00017Q00083Q0003043Q0067616D65030A3Q004765745365727669636503073Q00436F7265477569030E3Q0046696E6446697273744368696C64030F3Q00526F626C6F7850726F6D7074477569030D3Q0070726F6D70744F7665726C617900030B3Q00452Q726F7250726F6D707400173Q0012263Q00013Q00206Q000200122Q000200038Q0002000200202Q00013Q000400122Q000300056Q00010003000200062Q0002000C00010001000493012Q000C000100205C000200010004001283000400064Q00020102000400020026CD0002001300010007000493012Q0013000100205C000300020004001283000500084Q00020103000500020026460003001400010007000493012Q001400012Q00CF00036Q0006010300014Q0010010300024Q005F012Q00017Q00083Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203073Q0073652Q73696F6E026Q00F03F03043Q0062757379010003043Q007461736B03053Q00737061776E01193Q0012BF000100014Q00D40001000100020010632Q0100024Q00E300016Q00E300025Q00209900020002000300209D0102000200040010632Q01000300022Q00E300015Q0020990001000100032Q00E300025Q00303301020005000600062E3Q000F00010001000493012Q000F00012Q005F012Q00013Q0012BF000200073Q00209900020002000800067E00033Q000100052Q00E33Q00014Q00C23Q00014Q00E38Q00E33Q00024Q00E33Q00034Q003E0102000200012Q005F012Q00013Q00013Q00173Q0003043Q007461736B03043Q0077616974026Q00F03F030E3Q006973446973636F2Q6E656374656403043Q00627573792Q01028Q0003123Q0054656C65706F7274496E69744661696C656403073Q00436F2Q6E65637403113Q0071756575655F6F6E5F74656C65706F727403073Q0067657467656E7603123Q0056656C6F636974794C6F6164737472696E6703053Q007063612Q6C03023Q006F7303053Q00636C6F636B026Q002E40026Q00E03F030A3Q00446973636F2Q6E65637403043Q006D6174682Q033Q006D696E026Q000840026Q003E40012Q005C3Q00067E5Q000100032Q00E38Q00E33Q00014Q00E33Q00024Q00C200016Q00D400010001000200065B0001005B00013Q000493012Q005B00010012BF000100013Q00206B2Q010001000200122Q000200036Q0001000200014Q000100023Q00202Q0001000100044Q00010001000200062Q0001000400013Q000493012Q000400012Q00E3000100023Q00209900010001000500062E0001000400010001000493012Q000400012Q00E3000100023Q0030332Q0100050006001283000100074Q00C200026Q00D400020001000200065B0002005800013Q000493012Q0058000100209D2Q01000100032Q000601026Q00E3000300033Q00209900030003000800205C00030003000900067E00050001000100012Q00C23Q00024Q00020103000500020012BF0004000A3Q00065B0004003200013Q000493012Q003200010012BF0004000B4Q00D400040001000200209900040004000C00065B0004003200013Q000493012Q003200010012BF0004000D3Q00127A0105000A3Q00122Q0006000B6Q00060001000200202Q00060006000C4Q0004000600010012BF0004000D3Q00067E00050002000100032Q00E33Q00034Q00E33Q00024Q00E33Q00044Q003E0104000200010012BF0004000E3Q00209900040004000F2Q00D40004000100022Q00C200056Q00D400050001000200065B0005004C00013Q000493012Q004C000100062E0002004C00010001000493012Q004C00010012BF0005000E3Q00209900050005000F2Q00D40005000100022Q004200050005000400268A0005004C00010010000493012Q004C00010012BF000500013Q002099000500050002001283000600114Q003E010500020001000493012Q003B000100205C0005000300122Q003101050002000100122Q000500013Q00202Q00050005000200122Q000600133Q00202Q00060006001400202Q00070001001500122Q000800166Q000600086Q00053Q00014Q00025Q00044Q001800012Q00E3000200023Q003033010200050017000493012Q000400012Q005F012Q00013Q00033Q00043Q0003073Q0067657467656E76030C3Q004175746F52656A6F696E563203053Q00416C69766503073Q0073652Q73696F6E00123Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001000013Q000493012Q001000012Q00E37Q0020995Q000300065B3Q001000013Q000493012Q001000012Q00E33Q00014Q00E3000100023Q00209900010001000400064D3Q000F00010001000493012Q000F00012Q00CF8Q0006012Q00014Q0010012Q00024Q005F012Q00019Q003Q00034Q0006012Q00014Q0084017Q005F012Q00017Q00033Q0003173Q0054656C65706F7274546F506C616365496E7374616E636503073Q00706C616365496403053Q006A6F62496400094Q007B7Q00206Q00014Q000200013Q00202Q0002000200024Q000300013Q00202Q0003000300034Q000400028Q000400016Q00017Q00073Q0003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q6564030E3Q0057616C6B53702Q656456616C756501163Q0012AB2Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000493012Q000A000100205C000200010004001283000400054Q000201020004000200065B0002001500013Q000493012Q0015000100065B3Q001300013Q000493012Q001300010012BF000300014Q00D4000300010002002099000300030007001063010200060003000493012Q001500012Q00E3000300013Q0010630102000600032Q005F012Q00017Q00073Q0003073Q0067657467656E76030E3Q0057616C6B53702Q656456616C7565030F3Q0057616C6B53702Q6564546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403093Q0057616C6B53702Q656401133Q0012202Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001200013Q000493012Q001200012Q00E300015Q0020990001000100040006510102000F00010001000493012Q000F000100205C000200010005001283000400064Q000201020004000200065B0002001200013Q000493012Q00120001001063010200074Q005F012Q00017Q000A3Q0003073Q0067657467656E76030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F776572030E3Q004A756D70506F77657256616C7565026Q00494001163Q0012AB2Q0100016Q00010001000200102Q000100026Q00015Q00202Q00010001000300062Q0002000A00010001000493012Q000A000100205C000200010004001283000400054Q000201020004000200065B0002001500013Q000493012Q0015000100065B3Q001400013Q000493012Q0014000100303301020006000700123F000300016Q00030001000200202Q00030003000900102Q00020008000300044Q0015000100303301020008000A2Q005F012Q00017Q00093Q0003073Q0067657467656E76030E3Q004A756D70506F77657256616C7565030F3Q004A756D70506F776572546F2Q676C6503093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F6964030C3Q005573654A756D70506F7765722Q0103093Q004A756D70506F77657201143Q0012202Q0100016Q00010001000200102Q000100023Q00122Q000100016Q00010001000200202Q00010001000300062Q0001001300013Q000493012Q001300012Q00E300015Q0020990001000100040006510102000F00010001000493012Q000F000100205C000200010005001283000400064Q000201020004000200065B0002001300013Q000493012Q00130001003033010200070008001063010200094Q005F012Q00017Q00023Q0003073Q0067657467656E76030C3Q00496E66696E6974654A756D7001043Q0012BF000100014Q00D40001000100020010632Q0100024Q005F012Q00017Q00043Q0003083Q00746F737472696E6703043Q0067737562030C3Q005E25732A282E2D2925732A2403023Q00253101093Q0012BF000100014Q00C200026Q00B700010002000200205C000100010002001283000300033Q001283000400044Q00022Q01000400022Q00102Q0100024Q005F012Q00017Q00123Q00028Q0003063Q00737472696E6703053Q00752Q70657203043Q006773756203013Q002C034Q002Q033Q0025732B026Q00F03F03043Q0066696E6403013Q004B025Q00408F4003013Q004D024Q0080842E4103013Q0042024Q00652QCD4103013Q0054023Q00A0941A6D4203083Q00746F6E756D626572014D3Q00062E3Q000400010001000493012Q00040001001283000100014Q00102Q0100023Q0012BF000100023Q0020990001000100032Q00C200026Q00B70001000200022Q00C23Q00013Q00205C00013Q0004001283000300053Q001283000400064Q00022Q010004000200205000010001000400122Q000300073Q00122Q000400066Q0001000400026Q00013Q001283000100083Q00205C00023Q00090012830004000A4Q000201020004000200065B0002001F00013Q000493012Q001F00010012830001000B3Q00206A01023Q000400122Q0004000A3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100205C00023Q00090012830004000C4Q000201020004000200065B0002002B00013Q000493012Q002B00010012830001000D3Q00206A01023Q000400122Q0004000C3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100205C00023Q00090012830004000E4Q000201020004000200065B0002003700013Q000493012Q003700010012830001000F3Q00206A01023Q000400122Q0004000E3Q00122Q000500066Q0002000500026Q00023Q00044Q0042000100205C00023Q0009001283000400104Q000201020004000200065B0002004200013Q000493012Q00420001001283000100113Q00205000023Q000400122Q000400103Q00122Q000500066Q0002000500026Q00023Q0012BF000200124Q00C200036Q00B700020002000200065B0002004A00013Q000493012Q004A00012Q004901030002000100062E0003004B00010001000493012Q004B0001001283000300014Q0010010300024Q005F012Q00017Q000E3Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D6503063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A65637403053Q007461626C6503063Q00696E7365727403043Q004E616D6503043Q00736F7274003A4Q00447Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q000493012Q001E000100205C00013Q0001001283000300034Q00022Q010003000200065B0001001E00013Q000493012Q001E000100209900013Q000300205C000100010001001283000300044Q00022Q010003000200065B0001001E00013Q000493012Q001E000100209900013Q00030020B600010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q000493012Q001E000100209900013Q00030020BA00010001000400202Q00010001000500202Q00010001000100122Q000300066Q00010003000200062E0001002200010001000493012Q002200012Q0027000200024Q0010010200024Q003201025Q001235000300073Q00202Q0004000100084Q000400056Q00033Q000500044Q0032000100205C000800070009001283000A000A4Q00020108000A000200065B0008003200013Q000493012Q003200010012BF0008000B3Q00209900080008000C2Q00C2000900023Q002099000A0007000D2Q000F0108000A00010006190103002800010002000493012Q002800010012BF0003000B3Q00200C00030003000E4Q000400026Q0003000200014Q000200028Q00017Q00163Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q0050657473496E76656E746F727903053Q00496E646578030E3Q005363726F2Q6C696E674672616D65028Q0003063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103093Q004775694F626A656374026Q00F03F030E3Q0047657444657363656E64616E747303073Q0056697369626C6503043Q004E616D6503063Q004C6F636B656403043Q004C6F636B03073Q00556E6B6E6F776E030A3Q00496D6167654C6162656C030B3Q00496D616765436F6C6F723303063Q00436F6C6F72332Q033Q006E657700634Q00447Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q000493012Q001E000100205C00013Q0001001283000300034Q00022Q010003000200065B0001001E00013Q000493012Q001E000100209900013Q000300205C000100010001001283000300044Q00022Q010003000200065B0001001E00013Q000493012Q001E000100209900013Q00030020B600010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q000493012Q001E000100209900013Q00030020BA00010001000400202Q00010001000500202Q00010001000100122Q000300066Q00010003000200062E0001002200010001000493012Q002200012Q0027000200024Q0010010200023Q001283000200073Q0012A4000300073Q00122Q000400083Q00202Q0005000100094Q000500066Q00043Q000600044Q005D000100205C00090008000A001283000B000B4Q00020109000B000200065B0009005D00013Q000493012Q005D000100209D01030003000C2Q000601095Q001235000A00083Q00202Q000B0008000D4Q000B000C6Q000A3Q000C00044Q0058000100205C000F000E000A0012830011000B4Q0002010F0011000200065B000F004800013Q000493012Q00480001002099000F000E000E00065B000F004800013Q000493012Q00480001002099000F000E000F0026CD000F004600010010000493012Q00460001002099000F000E000F0026CD000F004600010011000493012Q00460001002099000F000E000F002646000F004800010012000493012Q004800012Q0006010900013Q000493012Q005A000100205C000F000E000A001283001100134Q0002010F0011000200065B000F005800013Q000493012Q00580001002099000F000E00140012BF001000153Q002099001000100016001283001100073Q001283001200073Q001283001300074Q0002011000130002000677010F005800010010000493012Q005800012Q0006010900013Q000493012Q005A0001000619010A003500010002000493012Q0035000100062E0009005D00010001000493012Q005D000100209D01020002000C0006190104002900010002000493012Q002900012Q00C2000400024Q00C2000500034Q0089010400034Q005F012Q00017Q00043Q00030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657200114Q00C57Q00206Q000100122Q000200028Q0002000200064Q000A00013Q000493012Q000A000100205C00013Q0001001283000300034Q00022Q01000300022Q00C23Q00013Q0006512Q01000F00013Q000493012Q000F000100205C00013Q0001001283000300044Q00022Q01000300022Q00102Q0100024Q005F012Q00017Q00193Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q6665727303063Q0069706169727303053Q00546965723103053Q00546965723203053Q0054696572332Q033Q005065742Q033Q0049734103093Q00546578744C6162656C03073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03043Q006E616D6503043Q005465787403053Q00707269636503013Q003F03093Q00617661696C61626C6503073Q0056697369626C65034Q0003023Q00783000674Q00447Q00206Q000100122Q000200028Q0002000200062Q0001001E00013Q000493012Q001E000100205C00013Q0001001283000300034Q00022Q010003000200065B0001001E00013Q000493012Q001E000100209900013Q000300205C000100010001001283000300044Q00022Q010003000200065B0001001E00013Q000493012Q001E000100209900013Q00030020B600010001000400202Q00010001000100122Q000300056Q00010003000200062Q0001001E00013Q000493012Q001E000100209900013Q00030020BA00010001000400202Q00010001000500202Q00010001000100122Q000300066Q00010003000200062E0001002200010001000493012Q002200012Q0027000200024Q0010010200024Q003201025Q0012E2000300076Q000400033Q00122Q000500083Q00122Q000600093Q00122Q0007000A6Q0004000300012Q0052000300020005000493012Q0063000100205C0008000100012Q00C2000A00074Q00020108000A00020006510109003300010008000493012Q0033000100205C000900080001001283000B000B4Q00020109000B000200065B0009006300013Q000493012Q0063000100205C000A0009000C001283000C000D4Q0002010A000C000200065B000A006300013Q000493012Q0063000100205C000A00080001001290010C000E6Q000A000C000200202Q000B0008000100122Q000D000F6Q000B000D000200202Q000C0008000100122Q000E00106Q000C000E000200062Q000D00480001000C000493012Q0048000100205C000D000C0001001283000F00114Q0002010D000F00022Q0032010E3Q0003002099000F00090013001063010E0012000F00065B000D005000013Q000493012Q00500001002099000F000D001300062E000F005100010001000493012Q00510001001283000F00153Q001063010E0014000F00065B000A005700013Q000493012Q00570001002099000F000A001700062E000F005F00010001000493012Q005F000100065B000B005C00013Q000493012Q005C0001002099000F000B001300062E000F005D00010001000493012Q005D0001001283000F00183Q002646000F006000010019000493012Q006000012Q00CF000F6Q0006010F00013Q001063010E0016000F2Q002D00020007000E0006190103002B00010002000493012Q002B00012Q0010010200024Q005F012Q00017Q00033Q0003063Q0069706169727303073Q0067657467656E7603123Q0053656C656374656454617267657450657473010F3Q0012E4000100013Q00122Q000200026Q00020001000200202Q0002000200034Q00010002000300044Q000A00010006770105000A00013Q000493012Q000A00012Q0006010600014Q0010010600023Q0006192Q01000600010002000493012Q000600012Q00062Q016Q00102Q0100024Q005F012Q00017Q00093Q0003073Q0067657467656E7603123Q0053656C65637465645461726765745065747303063Q0069706169727303053Q007461626C6503063Q0072656D6F76652Q033Q006D617803043Q004D617820030E3Q00207065747320726561636865642103063Q00696E7365727401263Q0012BF000100014Q00D40001000100020020990001000100020012BF000200034Q00C2000300014Q0052000200020004000493012Q001000010006770106001000013Q000493012Q001000010012BF000700043Q0020F10007000700054Q000800016Q000900056Q0007000900014Q000700016Q000700023Q0006190102000700010002000493012Q000700012Q0092000200014Q00E300035Q00209900030003000600066C0003001E00010002000493012Q001E00012Q000601025Q00122A010300076Q00045Q00202Q00040004000600122Q000500086Q0003000300054Q000200033Q0012BF000200043Q0020F10002000200094Q000300016Q00048Q0002000400014Q000200016Q000200024Q005F012Q00017Q00043Q0003053Q007461626C6503053Q00636C65617203073Q0067657467656E7603123Q0053656C65637465645461726765745065747300073Q00129D3Q00013Q00206Q000200122Q000100036Q00010001000200202Q0001000100046Q000200016Q00017Q00063Q0003073Q0067657467656E7603123Q0053656C656374656454617267657450657473028Q0003043Q004E6F6E65026Q00F03F2Q033Q00202B2000143Q0012173Q00018Q0001000200206Q00024Q00015Q00262Q0001000800010003000493012Q00080001001283000100044Q00102Q0100024Q009200015Q0026460001000D00010005000493012Q000D000100209900013Q00052Q00102Q0100023Q00209900013Q0005001283000200064Q009200035Q0020980003000300052Q00630001000100032Q00102Q0100024Q005F012Q00017Q00033Q0003073Q0067657467656E7603143Q004D65726368616E744D6178507269636554657874034Q0001073Q0012BF000100014Q00D400010001000200067F0102000500013Q000493012Q00050001001283000200033Q0010632Q01000200022Q005F012Q00017Q00063Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E026Q00F03F03043Q007461736B03053Q00737061776E01173Q0012E6000100016Q00010001000200102Q000100026Q00018Q00025Q00202Q00020002000300202Q00020002000400102Q0001000300024Q00015Q00202Q00010001000300064Q000D00010001000493012Q000D00012Q005F012Q00013Q0012BF000200053Q00209900020002000600067E00033Q000100052Q00C23Q00014Q00E38Q00E33Q00014Q00E33Q00024Q00E33Q00034Q003E0102000200012Q005F012Q00013Q00013Q00283Q0003073Q0067657467656E76030E3Q004D65726368616E74536E6970657203073Q0073652Q73696F6E030E3Q0046696E6446697273744368696C6403073Q0052656D6F74657303043Q005065747303153Q0050757263686173654D65726368616E744F2Q66657203063Q0069706169727303123Q0053656C65637465645461726765745065747303063Q00737472696E6703053Q00752Q7065722Q0103043Q006E65787403093Q00506C6179657247756903023Q005549030D3Q004D65726368616E744672616D6503043Q0053686F7003063Q004F2Q66657273030A3Q007061727365507269636503143Q004D65726368616E744D617850726963655465787403053Q00546965723103053Q00546965723203053Q0054696572332Q033Q0050657403073Q00536F6C644F757403053Q0053746F636B2Q033Q00427579030A3Q0050726963654C6162656C03073Q0056697369626C6503043Q0054657874034Q002Q033Q0049734103093Q00546578744C6162656C03023Q007830028Q0003053Q007063612Q6C0003043Q007461736B03043Q0077616974026Q33D33F00BF4Q0032017Q0027000100013Q0012BF000200014Q00D400020001000200209900020002000200065B000200BE00013Q000493012Q00BE00012Q00E300026Q00E3000300013Q002099000300030003000677010200BE00010003000493012Q00BE000100062E0001001E00010001000493012Q001E00012Q00E3000200023Q00205C000200020004001283000400054Q000201020004000200065B0002001800013Q000493012Q0018000100205C000300020004001283000500064Q00020103000500022Q00C2000200033Q0006512Q01001E00010002000493012Q001E000100205C000300020004001283000500074Q00020103000500022Q00C2000100034Q003201025Q0012E4000300083Q00122Q000400016Q00040001000200202Q0004000400094Q00030002000500044Q002C00010012BF0008000A3Q00200801080008000B4Q000900036Q000A00076Q0009000A6Q00083Q000200202Q00020008000C0006190103002500010002000493012Q0025000100065B000100B900013Q000493012Q00B900010012BF0003000D4Q00C2000400024Q00B700030002000200065B000300B900013Q000493012Q00B900012Q00E3000300043Q00205C0003000300040012830005000E4Q00020103000500020006510104005300010003000493012Q0053000100205C0004000300040012830006000F4Q000201040006000200065B0004005300013Q000493012Q0053000100209900040003000F00205C000400040004001283000600104Q000201040006000200065B0004005300013Q000493012Q0053000100209900040003000F0020B600040004001000202Q00040004000400122Q000600116Q00040006000200062Q0004005300013Q000493012Q0053000100209900040003000F0020BA00040004001000202Q00040004001100202Q00040004000400122Q000600126Q00040006000200065B000400B700013Q000493012Q00B700012Q00E3000500013Q0020D800050005001300122Q000600016Q00060001000200202Q0006000600144Q00050002000200122Q000600086Q000700033Q00122Q000800153Q00122Q000900163Q00122Q000A00174Q00120107000300012Q0052000600020008000493012Q00B4000100205C000B000400042Q00C2000D000A4Q0002010B000D00022Q0006010C5Q00065B000B00B000013Q000493012Q00B0000100205C000D000B000400127D010F00186Q000D000F000200202Q000E000B000400122Q001000196Q000E0010000200202Q000F000B000400122Q0011001A6Q000F0011000200202Q0010000B000400122Q0012001B6Q00100012000200062Q0011007A00010010000493012Q007A000100205C0011001000040012830013001C4Q00020111001300020006510112007D0001000E000493012Q007D00010020990012000E001D00065B000F008200013Q000493012Q008200010020990013000F001E00062E0013008300010001000493012Q008300010012830013001F3Q00065B000D00B000013Q000493012Q00B0000100205C0014000D0020001283001600214Q000201140016000200065B001400B000013Q000493012Q00B0000100062E001200B000010001000493012Q00B000010026CD001300B000010022000493012Q00B000010012BF0014000A3Q00209900140014000B2Q00E3001500033Q0020990016000D001E2Q0094011500164Q004C01143Q00022Q00C300140002001400065B001400B000013Q000493012Q00B0000100065B0011009F00013Q000493012Q009F00012Q00E3001400013Q00209900140014001300209900150011001E2Q00B700140002000200062E001400A000010001000493012Q00A00001001283001400233Q00269F010500A400010023000493012Q00A4000100066C001400B000010005000493012Q00B000012Q0006010C00014Q00C300153Q000A00062E001500B000010001000493012Q00B000010012BF001500243Q00067E00163Q000100022Q00C23Q00014Q00C23Q000A4Q00B700150002000200065B001500B000013Q000493012Q00B000010020813Q000A000C00062E000C00B300010001000493012Q00B300010020813Q000A00252Q00F700095Q0006190106006300010002000493012Q00630001000493012Q00B900012Q003201056Q00C23Q00053Q0012BF000300263Q002099000300030027001283000400284Q003E010300020001000493012Q000200012Q005F012Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q00CB7Q00206Q00014Q000200018Q000200016Q00017Q00053Q0003023Q00756903063Q007374617475732Q033Q00536574034Q0003073Q0063752Q72656E74021B4Q00E300025Q00209900020002000100209900020002000200065B0002000D00013Q000493012Q000D00012Q00E300025Q00209900020002000100209900020002000200205C00020002000300067F0104000C00013Q000493012Q000C0001001283000400044Q000F0102000400012Q00E300025Q00209900020002000100209900020002000500065B0002001A00013Q000493012Q001A000100065B0001001A00013Q000493012Q001A00012Q00E300025Q00209900020002000100209900020002000500205C0002000200032Q00C2000400014Q000F0102000400012Q005F012Q00017Q00013Q0003173Q005365744175746F486174636853656C6563746564452Q6701054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q0003123Q005365744175746F48617463685065724F6E6501054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030C3Q005365744175746F50756E636801054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030B3Q005365744175746F4C69667401054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030C3Q005365744175746F53746F6D7001054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030F3Q005365744175746F47656D54772Q656E01054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030B3Q005365744175746F53652Q6C01054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q0003123Q005365744175746F54652Q7269746F7269657301054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00013Q00030C3Q005365744175746F437562657301054Q00822Q015Q00202Q0001000100014Q00028Q0001000200016Q00017Q00043Q0003093Q004175746F437562657303073Q00456E61626C65642Q0103073Q0067657467656E7601123Q0026463Q000900010001000493012Q000900012Q00E300015Q0020990001000100020026CD0001000700010003000493012Q000700012Q00CF00016Q00062Q0100014Q00102Q0100023Q0012BF000100044Q00D40001000100022Q00C3000100013Q0026CD0001000F00010003000493012Q000F00012Q00CF00016Q00062Q0100014Q00102Q0100024Q005F012Q00017Q00023Q0003143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E65000D4Q00E38Q00D43Q0001000200065B3Q000800013Q000493012Q000800012Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q0032012Q00013Q001283000100024Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q0003093Q004175746F50756E636800054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q000E3Q0003083Q0074704D616E75616C03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C03073Q006F776E656454702Q01030F3Q005365745470546F426F2Q734B692Q6C012Q003A4Q00E37Q0020995Q000100065B3Q000500013Q000493012Q000500012Q005F012Q00013Q0012BF3Q00023Q00201C5Q000300122Q000200048Q000200024Q00015Q00064Q001A00013Q000493012Q001A00010012BF000200053Q00205C00033Q00062Q0094010300044Q007201023Q0004000493012Q0018000100205C000700060007001283000900084Q000201070009000200065B0007001800013Q000493012Q001800012Q00062Q0100013Q000493012Q001A00010006190102001100010002000493012Q0011000100065B0001002800013Q000493012Q002800010012BF000200094Q00D400020001000200209900020002000A00062E0002002800010001000493012Q002800012Q00E300025Q00304A0102000B000C4Q000200013Q00202Q00020002000D4Q000300016Q000200020001000493012Q0039000100062E0001003900010001000493012Q003900012Q00E300025Q00209900020002000B00065B0002003900013Q000493012Q003900010012BF000200094Q00D400020001000200209900020002000A00065B0002003900013Q000493012Q003900012Q00E300025Q00304A0102000B000E4Q000200013Q00202Q00020002000D4Q00038Q0002000200012Q005F012Q00017Q00013Q0003083Q004175746F4C69667400054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q0003063Q0049736C616E6400044Q00E37Q001283000100014Q003E012Q000200012Q005F012Q00017Q00013Q0003083Q004175746F4C69667400054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q0003063Q004D61726B657400044Q00E37Q001283000100014Q003E012Q000200012Q005F012Q00017Q00013Q0003083Q004175746F4C69667400054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q0003093Q004175746F50756E636800054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q0003093Q004175746F53746F6D7000054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q00030C3Q004175746F47656D54772Q656E00054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q0003083Q004175746F53652Q6C00054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00013Q00030F3Q004175746F54652Q7269746F7269657300054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q00073Q0003073Q0067657467656E76030F3Q004175746F54652Q7269746F7269657303043Q006F776E7303053Q007461626C6503063Q00696E7365727403053Q006F776E656403123Q005365744175746F54652Q7269746F7269657300163Q0012BF3Q00014Q00D43Q000100020020995Q000200062E3Q001500010001000493012Q001500012Q00E37Q0020995Q0003001283000100024Q00B73Q0002000200062E3Q001100010001000493012Q001100010012BF3Q00043Q00205A014Q00054Q00015Q00202Q00010001000600122Q000200028Q000200012Q00E33Q00013Q0020995Q00072Q00062Q0100014Q003E012Q000200012Q005F012Q00017Q00013Q0003093Q004175746F437562657300054Q0032012Q00013Q001283000100014Q0012012Q000100012Q0010012Q00024Q005F012Q00017Q001D3Q0003063Q00737472696E6703053Q006C6F776572034Q0003043Q0066696E6403053Q0068617463682Q033Q00652Q6703063Q00706C617965722Q033Q0070767003043Q00626F2Q7303103Q0025665B25615D706C617925665B25415D03083Q00706C617974696D6503043Q00686F757203063Q006D696E75746503063Q006F6E6C696E6503073Q0070612Q7369766503043Q006C69667403063Q0069736C616E64030B3Q006C6966745F69736C616E6403063Q006D61726B6574030B3Q0073757065726D61726B6574030B3Q006C6966745F6D61726B657403053Q0070756E636803053Q0073746F6D702Q033Q0067656D03073Q00636F2Q6C65637403043Q0073652Q6C03083Q0074652Q7269746F7203093Q0074652Q7269746F727903043Q0063756265018C3Q0012BF000100013Q00209900010001000200067F0102000500013Q000493012Q00050001001283000200034Q00B700010002000200205C000200010004001283000400054Q000201020004000200062E0002001000010001000493012Q0010000100205C000200010004001283000400064Q000201020004000200065B0002001200013Q000493012Q00120001001283000200054Q0010010200023Q00205C000200010004001283000400074Q000201020004000200062E0002001C00010001000493012Q001C000100205C000200010004001283000400084Q000201020004000200065B0002001E00013Q000493012Q001E0001001283000200084Q0010010200023Q00205C000200010004001283000400094Q000201020004000200065B0002002500013Q000493012Q00250001001283000200094Q0010010200023Q00205C0002000100040012830004000A4Q000201020004000200062E0002003E00010001000493012Q003E000100205C0002000100040012830004000B4Q000201020004000200062E0002003E00010001000493012Q003E000100205C0002000100040012830004000C4Q000201020004000200062E0002003E00010001000493012Q003E000100205C0002000100040012830004000D4Q000201020004000200062E0002003E00010001000493012Q003E000100205C0002000100040012830004000E4Q000201020004000200065B0002004000013Q000493012Q004000010012830002000F4Q0010010200023Q00205C000200010004001283000400104Q000201020004000200065B0002005A00013Q000493012Q005A000100205C000200010004001283000400114Q000201020004000200065B0002004C00013Q000493012Q004C0001001283000200124Q0010010200023Q00205C000200010004001283000400134Q000201020004000200062E0002005600010001000493012Q0056000100205C000200010004001283000400144Q000201020004000200065B0002005800013Q000493012Q00580001001283000200154Q0010010200023Q001283000200104Q0010010200023Q00205C000200010004001283000400164Q000201020004000200065B0002006100013Q000493012Q00610001001283000200164Q0010010200023Q00205C000200010004001283000400174Q000201020004000200065B0002006800013Q000493012Q00680001001283000200174Q0010010200023Q00205C000200010004001283000400184Q000201020004000200062E0002007200010001000493012Q0072000100205C000200010004001283000400194Q000201020004000200065B0002007400013Q000493012Q00740001001283000200184Q0010010200023Q00205C0002000100040012830004001A4Q000201020004000200065B0002007B00013Q000493012Q007B00010012830002001A4Q0010010200023Q00205C0002000100040012830004001B4Q000201020004000200065B0002008200013Q000493012Q008200010012830002001C4Q0010010200023Q00205C0002000100040012830004001D4Q000201020004000200065B0002008900013Q000493012Q008900010012830002001D4Q0010010200024Q0027000200024Q0010010200024Q005F012Q00017Q00063Q00030E3Q0046696E6446697273744368696C6403093Q00506C6179657247756903023Q005549030A3Q005175657374734C697374030E3Q005363726F2Q6C696E674672616D6503043Q004C697374001A4Q00447Q00206Q000100122Q000200028Q0002000200062Q0001000900013Q000493012Q0009000100205C00013Q0001001283000300034Q00022Q01000300020006510102000E00010001000493012Q000E000100205C000200010001001283000400044Q00020102000400020006510103001300010002000493012Q0013000100205C000300020001001283000500054Q00020103000500020006510104001800010003000493012Q0018000100205C000400030001001283000600064Q00020104000600022Q0010010400024Q005F012Q00017Q000A3Q0003043Q006773756203013Q002C034Q0003043Q0066696E6403013Q003A028Q0003063Q00676D617463682Q033Q0025642B026Q004E4003083Q00746F6E756D626572011C3Q00205000013Q000100122Q000300023Q00122Q000400036Q0001000400026Q00013Q00205C00013Q0004001283000300054Q00022Q010003000200065B0001001700013Q000493012Q00170001001283000100063Q00205C00023Q0007001283000400084Q00B1000200040004000493012Q0014000100207C0106000100090012BF0007000A4Q00C2000800054Q00B70007000200022Q00DA0001000600070006190102000F00010001000493012Q000F00012Q00102Q0100023Q0012BF0001000A4Q00C200026Q00112Q0100024Q004500016Q005F012Q00017Q00033Q0003063Q00737472696E6703053Q006D61746368031D3Q00285B25643A2C252E5D2B2925732A2F25732A285B25643A2C252E5D2B2901113Q001287000100013Q00202Q0001000100024Q00025Q00122Q000300036Q00010003000200062Q0001000900010001000493012Q000900012Q0027000300034Q0010010300024Q00E300038Q000400016Q0003000200024Q00048Q000500026Q000400056Q00039Q0000017Q00043Q002Q033Q0049734103093Q004775694F626A65637403073Q0056697369626C6503063Q00506172656E7402144Q00C200025Q00065B0002001100013Q000493012Q0011000100064D0002001100010001000493012Q0011000100205C000300020001001283000500024Q000201030005000200065B0003000F00013Q000493012Q000F000100209900030002000300062E0003000F00010001000493012Q000F00012Q000601036Q0010010300023Q002099000200020004000493012Q000100012Q0006010300014Q0010010300024Q005F012Q00017Q00243Q0003073Q006765744C69737403063Q00697061697273030B3Q004765744368696C6472656E03063Q00737472696E6703053Q006D6174636803043Q004E616D65030A3Q005E4461696C7943617264030E3Q0046696E6446697273744368696C64030A3Q00436F6E74656E74526F7703043Q00496E666F03093Q0051756573744E616D65030E3Q0051756573744E616D65426F6E7573034Q0003073Q0056697369626C6503043Q0054657874030B3Q00426F6E75734368726F6D652Q033Q0049734103093Q004775694F626A656374030E3Q0047657444657363656E64616E747303093Q00546578744C6162656C030D3Q00706172736550726F6772652Q73028Q0003053Q006C6F77657203043Q0066696E6403073Q00636C61696D656403093Q00636F6D706C6574656403053Q007461626C6503063Q00696E7365727403023Q00696403013Q007C03043Q006E616D6503043Q006465736303043Q006B696E6403083Q00636C612Q7369667903053Q00626F6E757303043Q00646F6E65008D4Q00E87Q00206Q00016Q000100024Q00015Q00064Q000700010001000493012Q000700012Q00102Q0100023Q0012BF000200023Q00205C00033Q00032Q0094010300044Q007201023Q0004000493012Q008900010012BF000700043Q0020A401070007000500202Q00080006000600122Q000900076Q00070009000200062Q0007008900013Q000493012Q0089000100205C000700060008001283000900094Q000201070009000200065B0007001C00013Q000493012Q001C000100209900070006000900205C0007000700080012830009000A4Q00020107000900020006510108002100010007000493012Q0021000100205C000800070008001283000A000B4Q00020108000A00020006510109002600010007000493012Q0026000100205C000900070008001283000B000C4Q00020109000B0002001283000A000D3Q00065B0008002E00013Q000493012Q002E0001002099000B0008000E00065B000B002E00013Q000493012Q002E0001002099000A0008000F000493012Q0038000100065B0009003500013Q000493012Q00350001002099000B0009000E00065B000B003500013Q000493012Q00350001002099000A0009000F000493012Q0038000100065B0008003800013Q000493012Q00380001002099000A0008000F00205C000B00060008001283000D00104Q0002010B000D000200065B000B004500013Q000493012Q0045000100205C000C000B0011001283000E00124Q0002010C000E000200065B000C004500013Q000493012Q00450001002099000C000B000E00062E000C004600010001000493012Q004600012Q0006010C6Q0006010D5Q001235000E00023Q00202Q000F000600134Q000F00106Q000E3Q001000044Q0073000100205C001300120011001283001500144Q000201130015000200065B0013007300013Q000493012Q007300012Q00E3001300014Q00C2001400124Q00C2001500064Q000201130015000200065B0013007300013Q000493012Q007300012Q00E300135Q00209900130013001500209900140012000F2Q005200130002001400065B0013006400013Q000493012Q0064000100065B0014006400013Q000493012Q00640001000EA00116006400010014000493012Q0064000100066C0014006400010013000493012Q006400012Q0006010D00013Q0012BF001500043Q00201501150015001700202Q00160012000F4Q00150002000200202Q00160015001800122Q001800196Q00160018000200062Q0016007200010001000493012Q0072000100205C0016001500180012830018001A4Q000201160018000200065B0016007300013Q000493012Q007300012Q0006010D00013Q000619010E004C00010002000493012Q004C00010012BF000E001B3Q00201B010E000E001C4Q000F00016Q00103Q000600202Q00110006000600122Q0012001E6Q0013000A6Q00110011001300102Q0010001D001100202Q00110006000600102Q0010001F001100102Q00100020000A4Q00115Q00202Q0011001100224Q0012000A6Q00110002000200102Q00100021001100102Q00100023000C00102Q00100024000D4Q000E001000010006190102000C00010002000493012Q000C00012Q00102Q0100024Q005F012Q00017Q00023Q0003063Q0069706169727303053Q006F776E6564010E3Q001296000100016Q00025Q00202Q0002000200024Q00010002000300044Q000900010006770105000900013Q000493012Q000900012Q0006010600014Q0010010600023Q0006192Q01000500010002000493012Q000500012Q00062Q016Q00102Q0100024Q005F012Q00017Q00093Q0003053Q006F776E6564026Q00F03F026Q00F0BF03053Q007461626C6503063Q0072656D6F766503073Q006F776E6564547003073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C012Q00204Q006E7Q00206Q00019Q0000122Q000100023Q00122Q000200033Q00044Q001400012Q00E3000400014Q003D01055Q00202Q0005000500014Q0005000500034Q00040002000200062Q0004001300010001000493012Q001300010012BF000400043Q0020740004000400054Q00055Q00202Q0005000500014Q000600036Q0004000600010004AA3Q000600012Q00E37Q0020995Q000600065B3Q001F00013Q000493012Q001F00010012BF3Q00074Q00D43Q000100020020995Q000800062E3Q001F00010001000493012Q001F00012Q00E37Q003033012Q000600092Q005F012Q00017Q00073Q0003053Q006F776E656403073Q006F776E6564547003063Q006163746976650003063Q0069706169727303053Q007063612Q6C030F3Q005365745470546F426F2Q734B692Q6C00224Q00337Q00206Q00014Q00015Q00202Q0001000100024Q00028Q00038Q00048Q00058Q00065Q00302Q00040003000400102Q00030002000600102Q00020001000500122Q000200056Q00038Q00020002000400044Q001800012Q00E3000700014Q00C300070007000600065B0007001800013Q000493012Q001800010012BF000800064Q00C2000900074Q0006010A6Q000F0108000A00010006190102001000010002000493012Q0010000100065B0001002100013Q000493012Q002100010012BF000200064Q00E3000300023Q0020990003000300072Q000601046Q000F0102000400012Q005F012Q00017Q000D3Q0003043Q0073746F7003063Q0061637469766503083Q0074704D616E75616C03073Q0067657467656E76030C3Q005470546F426F2Q734B692Q6C2Q0103063Q0069706169727303053Q00666C61677303053Q007461626C6503063Q00696E7365727403053Q006F776E656403053Q007063612Q6C03043Q007469636B01344Q002500015Q00202Q0001000100014Q0001000100014Q000100016Q000100013Q00062Q0001000800010001000493012Q000800012Q005F012Q00014Q00E300025Q001054010200026Q00025Q00122Q000300046Q00030001000200202Q00030003000500262Q0003001100010006000493012Q001100012Q00CF00036Q0006010300013Q00103501020003000300122Q000200073Q00202Q0003000100084Q000300016Q00023Q000400044Q002B00012Q00E3000700024Q00C2000800064Q00B700070002000200062E0007002B00010001000493012Q002B00010012BF000700093Q00207400070007000A4Q00085Q00202Q00080008000B4Q000900066Q0007000900012Q00E3000700034Q00C300070007000600065B0007002B00013Q000493012Q002B00010012BF0008000C4Q00C2000900074Q0006010A00014Q000F0108000A00010006190102001800010002000493012Q0018000100209900020001000D00065B0002003300013Q000493012Q003300010012BF0002000C3Q00209900030001000D2Q003E0102000200012Q005F012Q00017Q00093Q0003073Q0067657467656E7603093Q004175746F517565737403073Q0073652Q73696F6E026Q00F03F03043Q0073746F702Q033Q004F2Q6603013Q002D03043Q007461736B03053Q00737061776E011E3Q0012BF000100014Q00D40001000100020010632Q0100024Q00E300016Q00E300025Q00209900020002000300209D0102000200040010632Q01000300022Q00E300015Q0020990001000100032Q00E300025Q0020990002000200052Q000A00020001000100062E3Q001400010001000493012Q001400012Q00E3000200013Q001283000300063Q001283000400074Q000F0102000400012Q005F012Q00013Q0012BF000200083Q00209900020002000900067E00033Q000100052Q00E33Q00024Q00C23Q00014Q00E38Q00E33Q00014Q00E33Q00034Q003E0102000200012Q005F012Q00013Q00013Q00253Q00028Q00025Q00208C4003123Q0072656C656173655573657253746F2Q70656403093Q0072656164436172647303193Q004F70656E2074686520517565737473206C697374206F6E636503013Q002D03063Q0069706169727303073Q0067657467656E76030A3Q005175657374446562756703053Q007072696E74032A3Q005B51756573745D202573207C202573207C206B696E643D257320626F6E75733D257320646F6E653D257303063Q00666F726D617403043Q006E616D6503043Q006465736303083Q00746F737472696E6703043Q006B696E6403053Q00626F6E757303043Q00646F6E6503073Q0070612Q736976652Q033Q0070767003023Q00696403053Q00737461727403023Q006F7303053Q00636C6F636B03073Q00576F726B696E6703043Q007469636B03053Q007063612Q6C2Q0103043Q007461736B03053Q0064656C617903043Q0073746F70031F3Q004F6E6C7920706C617974696D65202F2050765020717565737473206C65667403073Q00536B692Q70656403173Q00556E6B6E6F776E2071756573742074797065206C656674030F3Q00412Q6C2071756573747320646F6E6503043Q0077616974026Q00F03F00AC3Q00067E5Q000100032Q00E38Q00E33Q00014Q00E33Q00024Q0027000100013Q001283000200014Q003201035Q001283000400024Q00C200056Q00D400050001000200065B000500A800013Q000493012Q00A800012Q00E3000500023Q0020010005000500034Q0005000100014Q000500023Q00202Q0005000500044Q0005000100024Q000600053Q00262Q0006001900010001000493012Q001900012Q00E3000600033Q001283000700053Q001283000800064Q000F0106000800012Q0027000600064Q006700078Q00088Q00095Q00122Q000A00076Q000B00056Q000A0002000C00044Q005500010012BF000F00084Q00D4000F00010002002099000F000F000900065B000F003600013Q000493012Q003600010012BF000F000A3Q0012830010000B3Q00205C00100010000C0020990012000E000D0020990013000E000E0012BF0014000F3Q0020990015000E00102Q00B70014000200020012BF0015000F3Q0020990016000E00112Q00B70015000200020012BF0016000F3Q0020990017000E00122Q0094011600174Q00EB00106Q000E010F3Q0001002099000F000E001100062E000F005500010001000493012Q00550001002099000F000E001200062E000F005500010001000493012Q00550001002099000F000E0010002646000F004100010013000493012Q004100012Q0006010700013Q000493012Q00550001002099000F000E0010002646000F004600010014000493012Q004600012Q0006010900013Q000493012Q00550001002099000F000E001000065B000F005100013Q000493012Q00510001002099000F000E00152Q00C3000F0003000F00062E000F005100010001000493012Q0051000100062E0006005000010001000493012Q005000012Q00C20006000E3Q000493012Q00550001002099000F000E001000062E000F005500010001000493012Q005500012Q0006010800013Q000619010A002100010002000493012Q0021000100065B0006008B00013Q000493012Q008B0001002099000A0006001500064D000100660001000A000493012Q006600012Q00E3000A00023Q002099000A000A0016002099000B000600102Q003E010A00020001002099000A000600150012BF000B00173Q002099000B000B00182Q00D4000B000100022Q00C20002000B4Q00C20001000A4Q00E3000A00033Q00126D010B00193Q00202Q000C0006000E4Q000A000C00014Q000A00043Q00202Q000B000600104Q000A000A000B00062Q000A007500013Q000493012Q00750001002099000B000A001A00065B000B007500013Q000493012Q007500010012BF000B001B3Q002099000C000A001A2Q003E010B000200010012BF000B00173Q002099000B000B00182Q00D4000B000100022Q0042000B000B0002000680000400A30001000B000493012Q00A30001002099000B0006001500202C0003000B001C00202Q000B0006001500122Q000C001D3Q00202Q000C000C001E4Q000D00043Q00067E000E0001000100022Q00C23Q00034Q00C23Q000B4Q000D010C000E00014Q000C00023Q00202Q000C000C001F4Q000C000100014Q000100016Q000B5Q00044Q00A300012Q00E3000A00023Q002099000A000A001F2Q000A000A000100012Q0027000100013Q00062E0007009300010001000493012Q0093000100065B0009009800013Q000493012Q009800012Q00E3000A00033Q001283000B00203Q001283000C00214Q000F010A000C0001000493012Q00A3000100065B0008009F00013Q000493012Q009F00012Q00E3000A00033Q001283000B00223Q001283000C00064Q000F010A000C0001000493012Q00A300012Q00E3000A00033Q001283000B00233Q001283000C00064Q000F010A000C00010012BF000A001D3Q002099000A000A0024001283000B00254Q003E010A00020001000493012Q000800012Q00E3000500023Q00209900050005001F2Q000A0005000100012Q005F012Q00013Q00023Q00043Q0003073Q0067657467656E7603093Q004175746F517565737403053Q00416C69766503073Q0073652Q73696F6E00123Q0012BF3Q00014Q00D43Q000100020020995Q000200065B3Q001000013Q000493012Q001000012Q00E37Q0020995Q000300065B3Q001000013Q000493012Q001000012Q00E33Q00014Q00E3000100023Q00209900010001000400064D3Q000F00010001000493012Q000F00012Q00CF8Q0006012Q00014Q0010012Q00024Q005F012Q00017Q00015Q00044Q00E38Q00E3000100013Q0020813Q000100012Q005F012Q00017Q00053Q0003083Q00496E7374616E63652Q033Q006E657703053Q00706169727303063Q00506172656E7403063Q00697061697273031D3Q001224000300013Q00202Q0003000300024Q00048Q00030002000200122Q000400036Q000500016Q00040002000600044Q000B00010026CD0007000B00010004000493012Q000B00012Q002D0003000700080006190104000800010002000493012Q0008000100065B0002001600013Q000493012Q001600010012BF000400054Q00C2000500024Q0052000400020006000493012Q001400010010630108000400030006190104001300010002000493012Q0013000100209900040001000400065B0004001B00013Q000493012Q001B00010020990004000100040010630103000400042Q0010010300024Q005F012Q00017Q00073Q0003083Q005549436F726E6572030C3Q00436F726E657252616469757303043Q005544696D2Q033Q006E6577028Q00026Q00144003063Q00506172656E74020F4Q009C01025Q00122Q000300016Q00043Q000200122Q000500033Q00202Q00050005000400122Q000600053Q00062Q0007000900010001000493012Q00090001001283000700064Q0002010500070002001063010400020005001063010400074Q0011010200044Q004500026Q005F012Q00017Q00083Q0003083Q0055495374726F6B6503053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q00E03F03093Q00546869636B6E652Q73026Q00F03F03063Q00506172656E7404144Q00E300045Q001283000500014Q003201063Q000400067F0107000700010001000493012Q000700012Q00E3000700013Q00209900070007000300106301060002000700067F0107000B00010002000493012Q000B0001001283000700053Q00106301060004000700067F0107000F00010003000493012Q000F0001001283000700073Q001063010600060007001063010600084Q0011010400064Q004500046Q005F012Q00017Q00013Q0003053Q007063612Q6C030E3Q0012BF000300013Q00067E00043Q000100042Q00E38Q00C23Q00014Q00C23Q00024Q00C28Q005200030002000400065B0003000B00013Q000493012Q000B000100067F0105000C00010004000493012Q000C00012Q0027000500054Q0010010500024Q005F012Q00013Q00013Q00093Q0003083Q005549536861646F77030A3Q00426C757252616469757303043Q005544696D2Q033Q006E6577028Q00026Q002840030C3Q005472616E73706172656E6379026Q00E03F03063Q00506172656E7400164Q00067Q00122Q000100016Q00023Q000300122Q000300033Q00202Q00030003000400122Q000400056Q000500013Q00062Q0005000A00010001000493012Q000A0001001283000500064Q00020103000500020010630102000200032Q00E3000300023Q00062E0003001000010001000493012Q00100001001283000300083Q0010630102000700032Q00E3000300033Q0010630102000900032Q0011012Q00024Q00458Q005F012Q00017Q00023Q0003063Q0043726561746503043Q00506C6179030C4Q00E300035Q00205C0003000300012Q00C200055Q00067F0106000600010001000493012Q000600012Q00E3000600014Q00C2000700024Q008E01030007000200202Q0004000300024Q0004000200014Q000300028Q00017Q00133Q0003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0054657874034Q0003083Q00466F6E744661636503043Q00466F6E742Q033Q006E657703043Q00456E756D030A3Q00466F6E7457656967687403043Q00426F6C6403073Q00526567756C6172030A3Q0054657874436F6C6F7233030A3Q00546578745363616C65642Q0103043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574026Q002C40042A4Q00A801045Q00122Q000500016Q00063Q000600302Q00060002000300062Q0007000700013Q000493012Q00070001001283000700053Q0010630106000400070012BF000700073Q0020990007000700082Q00E3000800013Q00065B0002001200013Q000493012Q001200010012BF000900093Q00209900090009000A00209900090009000B00062E0009001500010001000493012Q001500010012BF000900093Q00209900090009000A00209900090009000C2Q000201070009000200106301060006000700067F0107001B00010003000493012Q001B00012Q00E3000700023Q0020990007000700040010630106000D00070030330106000E000F0012BF000700113Q00209900070007001200067F0108002200010001000493012Q00220001001283000800133Q00067F0109002500010001000493012Q00250001001283000900134Q00020107000900020010630106001000072Q0011010400064Q004500046Q005F012Q00017Q00153Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q00284003083Q00506F736974696F6E03063Q00506172656E74026Q0028C003053Q007461626C6503063Q00696E7365727403063Q00697061697273030A3Q00496E707574426567616E03073Q00436F2Q6E656374030C3Q00496E7075744368616E67656402953Q00067F0106000300010001000493012Q000300012Q003201066Q00E300075Q001290000800016Q00093Q000600302Q00090002000300302Q00090004000500302Q00090006000700122Q000A00093Q00202Q000A000A000A00122Q000B00073Q00122Q000C000B3Q00122Q000D000B3Q0012A1000E000C6Q000A000E000200102Q00090008000A00122Q000A00093Q00202Q000A000A000A00122Q000B000B3Q00122Q000C000B3Q00122Q000D000B3Q00122Q000E000B6Q000A000E00020010630109000D000A0010630109000E4Q00610107000900024Q00085Q00122Q000900016Q000A3Q000600302Q000A0002000300302Q000A0004000500302Q000A0006000700122Q000B00093Q00202Q000B000B000A00122Q000C00073Q001258010D000B3Q00122Q000E000B3Q00122Q000F000C6Q000B000F000200102Q000A0008000B00122Q000B00093Q00202Q000B000B000A00122Q000C000B3Q00122Q000D000B3Q00122Q000E00073Q001283000F000F4Q0002010B000F0002001063010A000D000B001063010A000E4Q00610108000A00024Q00095Q00122Q000A00016Q000B3Q000600302Q000B0002000300302Q000B0004000500302Q000B0006000700122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q001258010E000C3Q00122Q000F00073Q00122Q0010000B6Q000C0010000200102Q000B0008000C00122Q000C00093Q00202Q000C000C000A00122Q000D000B3Q00122Q000E000B3Q00122Q000F000B3Q0012830010000B4Q0002010C00100002001063010B000D000C001063010B000E4Q00610109000B00024Q000A5Q00122Q000B00016Q000C3Q000600302Q000C0002000300302Q000C0004000500302Q000C0006000700122Q000D00093Q00202Q000D000D000A00122Q000E000B3Q001258010F000C3Q00122Q001000073Q00122Q0011000B6Q000D0011000200102Q000C0008000D00122Q000D00093Q00202Q000D000D000A00122Q000E00073Q00122Q000F000F3Q00122Q0010000B3Q0012830011000B4Q0002010D00110002001063010C000D000D001063010C000E4Q0002010A000C00020012BF000B00103Q002099000B000B00112Q00C2000C00064Q00C2000D00074Q000F010B000D00010012BF000B00103Q002099000B000B00112Q00C2000C00064Q00C2000D00084Q000F010B000D00010012BF000B00103Q002099000B000B00112Q00C2000C00064Q00C2000D00094Q000F010B000D00010012BF000B00103Q002099000B000B00112Q00C2000C00064Q00C2000D000A4Q000F010B000D00010012BF000B00124Q00C2000C00064Q0052000B0002000D000493012Q008800010020990010000F001300205C00100010001400067E00123Q000100042Q00C23Q00024Q00C23Q00044Q00C23Q00054Q00C28Q000F0110001200010020990010000F001500205C00100010001400067E00120001000100012Q00C23Q00034Q000F011000120001000619010B007B00010002000493012Q007B00012Q00E3000B00014Q00E3000C00023Q002099000C000C001500067E000D0002000100052Q00C23Q00034Q00C23Q00024Q00C23Q00044Q00C28Q00C23Q00054Q000F010B000D00012Q005F012Q00013Q00033Q00073Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636803083Q00506F736974696F6E03073Q004368616E67656403073Q00436F2Q6E656374011A3Q00209A00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000493012Q000C000100209900013Q00010012BF000200023Q0020990002000200010020990002000200040006772Q01001900010002000493012Q001900012Q00062Q0100014Q00842Q015Q00209900013Q00052Q00842Q0100014Q00E3000100033Q0020990001000100052Q00842Q0100023Q00209900013Q000600205C00010001000700067E00033Q000100022Q00C28Q00E38Q000F2Q01000300012Q005F012Q00013Q00013Q00033Q00030E3Q0055736572496E707574537461746503043Q00456E756D2Q033Q00456E64000A4Q00E37Q00200B5Q000100122Q000100023Q00202Q00010001000100202Q00010001000300064Q000900010001000493012Q000900012Q0006017Q0084012Q00014Q005F012Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F756368010E3Q00209A00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000493012Q000C000100209900013Q00010012BF000200023Q0020990002000200010020990002000200040006772Q01000D00010002000493012Q000D00012Q0084017Q005F012Q00017Q00073Q0003083Q00506F736974696F6E03053Q005544696D322Q033Q006E657703013Q005803053Q005363616C6503063Q004F2Q6673657403013Q0059011F4Q00E300015Q000677012Q001E00010001000493012Q001E00012Q00E3000100013Q00065B0001001E00013Q000493012Q001E000100209900013Q00012Q0065000200026Q0001000100024Q000200033Q00122Q000300023Q00202Q0003000300034Q000400043Q00202Q00040004000400202Q0004000400054Q000500043Q00202Q00050005000400202Q00050005000600202Q0006000100044Q0005000500064Q000600043Q00202Q00060006000700202Q0006000600054Q000700043Q00202Q00070007000700202Q00070007000600202Q0008000100074Q0007000700084Q00030007000200102Q0002000100032Q005F012Q00017Q00043Q00030A3Q00496E707574426567616E03073Q00436F2Q6E656374030A3Q00496E707574456E646564030C3Q00496E7075744368616E67656402184Q000601025Q00067E00033Q000100022Q00C28Q00C23Q00013Q00209900043Q000100205C00040004000200067E00060001000100022Q00C23Q00024Q00C23Q00034Q000F0104000600012Q00E300046Q00E3000500013Q00209900050005000300067E00060002000100012Q00C23Q00024Q000F0104000600012Q00E300046Q00E3000500013Q00209900050005000400067E00060003000100022Q00C23Q00024Q00C23Q00034Q000F0104000600012Q005F012Q00013Q00043Q00093Q0003103Q004162736F6C757465506F736974696F6E030C3Q004162736F6C75746553697A6503043Q006D61746803053Q00636C616D7003083Q00506F736974696F6E03013Q0058028Q00026Q00F03F03013Q0059011F4Q00E300015Q0020990001000100012Q00E300025Q0020990002000200020012BF000300033Q00209900030003000400209900043Q00050020990004000400060020990005000100062Q00420004000400050020990005000200062Q0005010400040005001283000500073Q001283000600084Q00020103000600020012BF000400033Q00209900040004000400209900053Q00050020990005000500090020990006000100092Q00420005000500060020990006000200092Q009601050005000600122Q000600073Q00122Q000700086Q0004000700024Q000500016Q000600036Q000700046Q0005000700016Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F75636801123Q00209A00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000493012Q000C000100209900013Q00010012BF000200023Q0020990002000200010020990002000200040006772Q01001100010002000493012Q001100012Q00062Q0100014Q00842Q016Q00E3000100014Q00C200026Q003E2Q01000200012Q005F012Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030C3Q004D6F75736542752Q746F6E3103053Q00546F756368010F3Q00209A00013Q000100122Q000200023Q00202Q00020002000100202Q00020002000300062Q0001000C00010002000493012Q000C000100209900013Q00010012BF000200023Q0020990002000200010020990002000200040006772Q01000E00010002000493012Q000E00012Q00062Q016Q00842Q016Q005F012Q00017Q00043Q00030D3Q0055736572496E7075745479706503043Q00456E756D030D3Q004D6F7573654D6F76656D656E7403053Q00546F75636801134Q00E300015Q00065B0001001200013Q000493012Q0012000100209900013Q00010012BF000200023Q00209900020002000100209900020002000300064D0001000F00010002000493012Q000F000100209900013Q00010012BF000200023Q0020990002000200010020990002000200040006772Q01001200010002000493012Q001200012Q00E3000100014Q00C200026Q003E2Q01000200012Q005F012Q00017Q00093Q0003063Q0067657468756903053Q007063612Q6C030B3Q004C6F63616C506C61796572030C3Q0057616974466F724368696C6403093Q00506C61796572477569026Q00144003043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900223Q0012BF3Q00013Q00065B3Q000B00013Q000493012Q000B00010012BF3Q00023Q0012BF000100014Q00523Q0002000100065B3Q000B00013Q000493012Q000B000100065B0001000B00013Q000493012Q000B00012Q00102Q0100023Q0012BF3Q00023Q0002E900016Q00523Q0002000100065B3Q001300013Q000493012Q0013000100065B0001001300013Q000493012Q001300012Q00102Q0100024Q00E300025Q00209900020002000300065B0002001C00013Q000493012Q001C000100205C000300020004001283000500053Q001283000600064Q0011010300064Q004500035Q0012BF000300073Q00201D01030003000800122Q000500096Q000300056Q00039Q0000013Q00013Q00043Q0003083Q00636C6F6E6572656603043Q0067616D65030A3Q004765745365727669636503073Q00436F726547756900113Q0012BF3Q00013Q00065B3Q000B00013Q000493012Q000B00010012BF3Q00013Q00122B000100023Q00202Q00010001000300122Q000300046Q000100039Q00000200064Q000F00010001000493012Q000F00010012BF3Q00023Q00205C5Q0003001283000200044Q0002012Q000200022Q0010012Q00024Q005F012Q00017Q00023Q002Q033Q0073796E030B3Q0070726F746563745F677569000C3Q0012BF3Q00013Q00065B3Q000B00013Q000493012Q000B00010012BF3Q00013Q0020995Q000200065B3Q000B00013Q000493012Q000B00010012BF3Q00013Q0020995Q00022Q00E300016Q003E012Q000200012Q005F012Q00017Q00433Q0003053Q00416C697665010003073Q0067657467656E76030F3Q0057616C6B53702Q6564546F2Q676C65030B3Q004175746F47656D57616C6B030C3Q0057616C6B546F5475726B6579030F3Q004A756D70506F776572546F2Q676C6503083Q004175746F53652Q6C03133Q004175746F53652Q6C53757065726D61726B657403093Q00426F2Q734272696E6703093Q0044697361626C65334403063Q0069706169727303083Q004175746F4C69667403093Q004175746F50756E636803093Q004175746F53746F6D70030B3Q004175746F41697264726F70030D3Q004175746F41697264726F705631030D3Q0057616C6B546F41697264726F70030F3Q004175746F54652Q7269746F72696573031A3Q004175746F53757065726D61726B657454652Q7269746F72696573030C3Q004175746F47656D54772Q656E030C3Q004175746F47656D4272696E67030F3Q004175746F4461696C79526577617264030A3Q0057616C6B546F426F2Q73030C3Q005470546F426F2Q734B692Q6C030E3Q004175746F42757957656967687473030A3Q004175746F427579444E41030D3Q004175746F427579426F6469657303193Q004175746F42757953757065726D61726B65745765696768747303143Q004175746F486174636853656C6563746564452Q67030F3Q004175746F48617463685065724F6E6503103Q004175746F486174636843756265452Q6703063Q004E6F636C6970030A3Q004175746F52656A6F696E030C3Q004175746F52656A6F696E5632030C3Q00496E66696E6974654A756D70030E3Q004D65726368616E74536E69706572030B3Q0057616C6B546F437562657303093Q004175746F517565737403073Q00456E61626C656403073Q0073652Q73696F6E026Q00F03F03043Q0073746F7003053Q007063612Q6C03053Q007461626C6503053Q00636C65617203093Q0043686172616374657203153Q0046696E6446697273744368696C644F66436C612Q7303083Q0048756D616E6F696403043Q004D6F766503073Q00566563746F723303043Q007A65726F03093Q0057616C6B53702Q656403093Q004A756D70506F776572026Q004940030E3Q0046696E6446697273744368696C6403103Q0048756D616E6F6964522Q6F745061727403083Q00416E63686F72656403093Q00776F726B7370616365030A3Q00426F2Q734D6F64656C73030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C030D3Q004B657953797374656D426C757203073Q0044657374726F7903133Q0056656C6F63697479487562496E7374616E63650001C64Q00E300015Q00209900010001000100062E0001000500010001000493012Q000500012Q005F012Q00014Q00E300015Q0030332Q01000100020012BF000100034Q00D400010001000200209900020001000400062E0002001000010001000493012Q0010000100209900020001000500062E0002001000010001000493012Q0010000100209900020001000600209900030001000700209900040001000800062E0004001500010001000493012Q0015000100209900040001000900209900050001000A00204900060001000B00122Q0007000C6Q000800193Q00122Q0009000D3Q00122Q000A000E3Q00122Q000B000F3Q00122Q000C00103Q00122Q000D00113Q00122Q000E00123Q00122Q000F00133Q00122Q001000143Q00122Q001100153Q00122Q001200163Q00122Q001300053Q00122Q001400063Q00122Q001500173Q00122Q001600083Q00122Q001700093Q00122Q0018000A3Q00122Q001900183Q00122Q001A00193Q00122Q001B001A3Q00122Q001C001B3Q00122Q001D001C3Q00122Q001E001D3Q00122Q001F001E3Q00122Q0020001F3Q00122Q002100203Q00122Q002200213Q00122Q0023000B3Q00122Q002400223Q00122Q002500233Q00122Q002600043Q00122Q002700073Q00122Q002800243Q00122Q002900253Q00122Q002A00263Q00122Q002B00276Q0008002300012Q0052000700020009000493012Q004000010020810001000B00020006190107003F00010002000493012Q003F00012Q00E3000700013Q0030330107002800022Q00E3000700024Q00E3000800023Q00209900080008002900209D01080008002A0010630107002900082Q00E3000700034Q00E3000800033Q00209900080008002900209D01080008002A0010630107002900082Q00E3000700044Q00E3000800043Q00209900080008002900209D01080008002A0010630107002900082Q00E3000700054Q00E3000800053Q00209900080008002900209D01080008002A0010630107002900082Q00E3000700064Q00E3000800063Q00209900080008002900209D01080008002A0010630107002900082Q00E3000700063Q00209900070007002B2Q000A0007000100012Q00E3000700074Q000A0007000100010012BF0007000C4Q00E3000800084Q0052000700020009000493012Q006B00010012BF000C002C3Q00067E000D3Q000100012Q00C23Q000B4Q003E010C000200012Q00F7000A5Q0006190107006600010002000493012Q006600010012BF0007002D3Q0020FB00070007002E4Q000800086Q0007000200014Q000700093Q00202Q00070007002F00062Q0008007800010007000493012Q0078000100205C000800070030001283000A00314Q00020108000A000200065B0008008500013Q000493012Q0085000100065B0002008200013Q000493012Q0082000100205C00090008003200123C000B00333Q00202Q000B000B00344Q0009000B00014Q0009000A3Q00102Q00080035000900065B0003008500013Q000493012Q0085000100303301080036003700065B0004008F00013Q000493012Q008F00010006510109008C00010007000493012Q008C000100205C000900070038001283000B00394Q00020109000B000200065B0009008F00013Q000493012Q008F00010030330109003A000200065B000500A900013Q000493012Q00A900010012BF0009003B3Q00205C000900090038001283000B003C4Q00020109000B000200065B000900A900013Q000493012Q00A900010012BF000A000C3Q00205C000B0009003D2Q0094010B000C4Q0072010A3Q000C000493012Q00A7000100205C000F000E003E0012830011003F4Q0002010F0011000200065B000F00A400013Q000493012Q00A4000100205C000F000E0038001283001100394Q0002010F0011000200065B000F00A700013Q000493012Q00A70001003033010F003A0002000619010A009C00010002000493012Q009C000100065B000600AF00013Q000493012Q00AF00010012BF0009002C3Q00067E000A0001000100012Q00E33Q000B4Q003E0109000200012Q00E30009000C3Q00205C000900090038001283000B00404Q00020109000B000200065B000900B700013Q000493012Q00B7000100205C000A000900412Q003E010A000200010012BF000A00034Q00D4000A00010002002099000A000A00422Q00E3000B5Q000677010A00C00001000B000493012Q00C000010012BF000A00034Q00D4000A00010002003033010A0042004300062E3Q00C500010001000493012Q00C500012Q00E3000A000D3Q00205C000A000A00412Q003E010A000200012Q005F012Q00013Q00023Q00013Q00030A3Q00446973636F2Q6E65637400044Q00E37Q00205C5Q00012Q003E012Q000200012Q005F012Q00017Q00013Q0003153Q00536574336452656E646572696E67456E61626C656400054Q0028016Q00206Q00014Q000200018Q000200016Q00019Q003Q00044Q00E38Q00062Q016Q003E012Q000200012Q005F012Q00019Q003Q00044Q00E38Q00062Q0100014Q003E012Q000200012Q005F012Q00017Q003C3Q0003083Q00496E7374616E63652Q033Q006E6577030A3Q00426C7572452Q6665637403043Q004E616D65030D3Q004B657953797374656D426C757203043Q0053697A65026Q00344003063Q00506172656E74028Q0003053Q004672616D65030E3Q004B657953797374656D4672616D65030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E03053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240025Q00C0624003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03063Q004163746976652Q01026Q00184003063Q005374726F6B65026Q00304003093Q00546578744C6162656C03043Q005465787403173Q004B65792053797374656D20566572696669636174696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40026Q002840026Q00F03F03093Q005365636F6E64617279025Q00804640026Q006E40026Q003E40026Q00104003073Q0054657874426F78034Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q005375625465787403103Q00436C656172546578744F6E466F6375730100026Q0024C0026Q001440030A3Q005465787442752Q746F6E030A3Q005375626D6974204B657903063Q00412Q63656E74025Q00805640030F3Q004175746F42752Q746F6E436F6C6F7203093Q0041637469766174656403073Q00436F2Q6E65637402E23Q00126B000200013Q00202Q00020002000200122Q000300036Q00020002000200302Q00020004000500302Q0002000600074Q00035Q00102Q00020008000300122Q000300096Q000400013Q00122Q0005000A6Q00063Q000900302Q00060004000B00122Q0007000D3Q00202Q00070007000200122Q0008000E3Q00122Q0009000E6Q00070009000200102Q0006000C000700122Q000700103Q00202Q00070007000200122Q0008000E3Q00122Q000900093Q00122Q000A000E3Q00122Q000B00096Q0007000B000200102Q0006000F000700122Q000700103Q00202Q00070007001100122Q000800123Q00122Q000900136Q00070009000200102Q0006000600074Q000700023Q00202Q00070007001500102Q00060014000700302Q00060016001700302Q00060018000900302Q00060019001A4Q000700033Q00102Q0006000800074Q0004000600024Q000500046Q000600043Q00122Q0007001B6Q0005000700014Q000500056Q000600046Q000700023Q00202Q00070007001C00122Q0008000E6Q0005000800014Q000500066Q000600043Q00122Q0007001D3Q00122Q0008000E6Q0005000800014Q000500013Q00122Q0006001E6Q00073Q000800302Q0007001F00204Q000800073Q00102Q0007002100084Q000800023Q00202Q00080008001F00102Q00070022000800302Q00070023002400122Q000800103Q00202Q00080008000200122Q000900093Q00122Q000A00093Q00122Q000B00093Q00122Q000C00256Q0008000C000200102Q0007000F000800122Q000800103Q00202Q00080008000200122Q000900263Q00122Q000A00093Q00122Q000B00093Q001283000C00074Q00020108000C00020010630107000600080030330107001600260010630107000800042Q00020105000700022Q00E3000600013Q0012830007000A4Q003201083Q00062Q00E3000900023Q0020990009000900270010630108001400090012BF0009000D3Q0020DF00090009000200122Q000A000E3Q00122Q000B00096Q0009000B000200102Q0008000C000900122Q000900103Q00202Q00090009000200122Q000A000E3Q00122Q000B00093Q00122Q000C00093Q00122Q000D00286Q0009000D000200102Q0008000F00090012BF000900103Q00202E01090009000200122Q000A00093Q00122Q000B00293Q00122Q000C00093Q00122Q000D002A6Q0009000D00020010630108000600090030330108001800090010630108000800042Q00020106000800022Q00C1000700046Q000800063Q00122Q0009002B6Q0007000900014Q000700056Q000800066Q000900023Q00202Q00090009001C4Q000A00086Q0007000A00022Q00E3000800013Q0012830009002C4Q0032010A3Q000B003033010A00160026003033010A001F002D003033010A002E002F2Q00E3000B00023Q002099000B000B0031001063010A0030000B2Q00E3000B00093Q001063010A0021000B2Q00E3000B00023Q002054000B000B001F00102Q000A0022000B00302Q000A0023002500302Q000A0032003300122Q000B00103Q00202Q000B000B000200122Q000C00263Q00122Q000D00343Q00122Q000E00263Q00122Q000F00094Q0056010B000F000200102Q000A0006000B00122Q000B00103Q00202Q000B000B000200122Q000C00093Q00122Q000D00353Q00122Q000E00093Q00122Q000F00096Q000B000F000200102Q000A000F000B001063010A000800062Q00020108000A00022Q00E3000900013Q00129E000A00366Q000B3Q000A00302Q000B001F00374Q000C00073Q00102Q000B0021000C4Q000C00023Q00202Q000C000C001F00102Q000B0022000C00302Q000B002300254Q000C00023Q002099000C000C0038001063010B0014000C0012BF000C000D3Q0020DF000C000C000200122Q000D000E3Q00122Q000E00096Q000C000E000200102Q000B000C000C00122Q000C00103Q00202Q000C000C000200122Q000D000E3Q00122Q000E00093Q00122Q000F00093Q00122Q001000396Q000C0010000200102Q000B000F000C0012BF000C00103Q00202E010C000C000200122Q000D00093Q00122Q000E00293Q00122Q000F00093Q00122Q0010002A6Q000C00100002001063010B0006000C003033010B003A0033001063010B000800042Q00020109000B00022Q00E3000A00044Q00C2000B00093Q001283000C002B4Q000F010A000C000100067E000A3Q0001000B2Q00C23Q00084Q00C28Q00C23Q00024Q00E33Q000A4Q00C23Q00044Q00E33Q000B4Q00C23Q00014Q00C23Q00034Q00E33Q000C4Q00C23Q00074Q00E33Q00023Q002062010B0009003B00202Q000B000B003C4Q000D000A6Q000B000D00014Q000B000D6Q000C00046Q000D00016Q000E00056Q000D000100012Q000F010B000D00012Q005F012Q00013Q00013Q00173Q0003043Q005465787403073Q0044657374726F7903043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00C07240028Q0003043Q007461736B03043Q0077616974029A5Q99C93F026Q00F03F026Q00084003043Q004B69636B030B3Q00496E76616C6964204B6579034Q00030F3Q00506C616365686F6C6465725465787403063Q00737472696E6703063Q00666F726D617403133Q00496E76616C6964204B657921202825642F332903053Q00436F6C6F7203073Q005761726E696E6703053Q0064656C6179026Q00F83F004A4Q00E37Q0020995Q00012Q00E3000100013Q000677012Q001D00010001000493012Q001D00012Q00E33Q00023Q00205C5Q00022Q003E012Q000200012Q00E33Q00034Q00E3000100044Q00E3000200054Q003201033Q00010012BF000400043Q00206501040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300042Q000F012Q000300010012BF3Q00083Q0020995Q00090012830001000A4Q003E012Q000200012Q00E33Q00043Q00205C5Q00022Q003E012Q000200012Q00E33Q00064Q000A3Q00010001000493012Q004900012Q00E33Q00073Q00209D014Q000B2Q0084012Q00074Q00E33Q00073Q000E9B000C002E00013Q000493012Q002E00012Q00E33Q00023Q00065B3Q002900013Q000493012Q002900012Q00E33Q00023Q00205C5Q00022Q003E012Q000200012Q00E33Q00083Q00205C5Q000D0012830002000E4Q000F012Q000200012Q005F012Q00014Q00E37Q0030C83Q0001000F9Q0000122Q000100113Q00202Q00010001001200122Q000200136Q000300076Q00010003000200104Q001000016Q00034Q00A0000100096Q000200056Q00033Q00014Q0004000A3Q00202Q00040004001500102Q0003001400046Q000300010012BF3Q00083Q0020995Q0016001283000100173Q00067E00023Q000100052Q00E38Q00E33Q00034Q00E33Q00094Q00E33Q00054Q00E33Q000A4Q000F012Q000200012Q005F012Q00013Q00013Q00043Q00030F3Q00506C616365686F6C64657254657874030C3Q00456E746572206B65793Q2E03053Q00436F6C6F7203063Q005374726F6B65000B4Q00E37Q003033012Q000100022Q00163Q00016Q000100026Q000200036Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00643Q0003063Q00412Q63656E7403043Q005461627303083Q005F63752Q72656E740003083Q005F746F2Q676C657303053Q004672616D6503043Q004E616D6503063Q0057696E646F77030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407A40026Q00744003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E6403163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030F3Q00426F7264657253697A65506978656C03103Q00436C69707344657363656E64616E74732Q0103063Q0041637469766503063Q00506172656E74026Q00184003063Q005374726F6B65026Q00304003063Q00546F7042617203093Q005365636F6E64617279026Q00F03F026Q00414003063Q005A496E646578026Q001440026Q0020400285EB51B81E85EB3F03043Q006D2Q6F6E026Q00324003063Q00507572706C6503093Q00546578744C6162656C03053Q005469746C6503043Q0054657874030C3Q0056656C6F636974792048756203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002C40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q002Q40026Q0059C0026Q00344003013Q0078026Q0020C003063Q00436F6C6F723303073Q0066726F6D524742025Q00E06F40025Q0080564003053Q006D696E7573026Q0041C003083Q006B6579626F617264026Q004EC003093Q0041637469766174656403073Q00436F2Q6E65637403043Q00426F6479030E3Q005363726F2Q6C696E674672616D6503073Q005461624C697374029A5Q99C93F025Q00C05C40030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q7303103Q005363726F2Q6C696E67456E61626C6564030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D026Q00084003093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E67526967687403073Q00436F6E74656E74026Q005D40026Q005DC0030B3Q005369646544697669646572027Q004003043Q007461736B03053Q00737061776E030A3Q00496E707574426567616E03093Q0043726561746554616202A4012Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100023Q00209900020001000100065B0002000A00013Q000493012Q000A00012Q00E300025Q0020990003000100010010630102000100032Q003201023Q00032Q005D00035Q00102Q00020002000300302Q0002000300044Q00035Q00102Q0002000500034Q000300013Q00122Q000400066Q00053Q000A00302Q00050007000800122Q0006000A3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000C6Q00060008000200102Q00050009000600122Q0006000E3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000F3Q00122Q0009000C3Q00122Q000A000F6Q0006000A000200102Q0005000D000600122Q0006000E3Q00202Q00060006001100122Q000700123Q00122Q000800136Q00060008000200102Q0005001000064Q00065Q00202Q00060006001500102Q00050014000600302Q00050016001700302Q00050018000F00302Q00050019001A00302Q0005001B001A4Q000600023Q00102Q0005001C00064Q0003000500024Q000400036Q000500033Q00122Q0006001D6Q0004000600014Q000400046Q000500036Q00065Q00202Q00060006001E00122Q0007000C6Q0004000700014Q000400056Q000500033Q00122Q0006001F3Q00122Q0007000C6Q0004000700014Q000400013Q00122Q000500066Q00063Q000800302Q0006000700204Q00075Q00202Q00070007002100102Q00060014000700302Q00060016001700122Q0007000E3Q00202Q00070007000B00122Q000800223Q00122Q0009000F3Q00122Q000A000F3Q00122Q000B00236Q0007000B000200102Q00060010000700302Q00060018000F00302Q00060024002500302Q0006001B001A00102Q0006001C00034Q0004000600024Q000500046Q000600046Q00075Q00202Q00070007001E4Q000800064Q000F0105000800012Q00EC000500056Q000600043Q00122Q000700263Q00122Q000800276Q0005000800014Q000500073Q00122Q000600283Q00122Q000700296Q000800016Q00095Q00202Q00090009002A4Q00050009000200122Q0006000A3Q00202Q00060006000B00122Q0007000F3Q00122Q0008000C6Q00060008000200102Q00050009000600122Q0006000E3Q00202Q00060006000B00122Q0007000F3Q00122Q000800263Q00122Q0009000C3Q00122Q000A000F6Q0006000A000200102Q0005000D000600102Q0005001C00044Q000600013Q00122Q0007002B6Q00083Q000C00302Q00080007002C00202Q00090001002C00062Q0009007F00010001000493012Q007F00010012830009002E3Q0010630108002D00092Q00AC010900083Q00102Q0008002F00094Q00095Q00202Q00090009002D00102Q00080030000900302Q00080031003200122Q000900343Q00202Q00090009003300202Q00090009003500102Q00080033000900122Q000900343Q00202Q00090009003600202Q00090009003700102Q00080036000900302Q00080016002200122Q0009000A3Q00202Q00090009000B00122Q000A000F3Q00122Q000B000C6Q0009000B000200102Q00080009000900122Q0009000E3Q00202Q00090009000B00122Q000A000F3Q00122Q000B00383Q00122Q000C000C3Q00122Q000D000F6Q0009000D000200102Q0008000D000900122Q0009000E3Q00202Q00090009000B00122Q000A00223Q00122Q000B00393Q00122Q000C000F3Q00122Q000D003A6Q0009000D000200102Q00080010000900102Q0008001C00044Q00060008000200067E00073Q000100072Q00E33Q00014Q00E38Q00C23Q00044Q00E33Q00034Q00E33Q00074Q00E33Q00094Q00E33Q000A4Q00C2000800073Q0012830009003B3Q001283000A003C3Q0012BF000B003D3Q002099000B000B003E001283000C003F3Q001283000D00403Q001283000E00404Q004B010B000E4Q004C01083Q00022Q00C2000900073Q001237010A00413Q00122Q000B00426Q000C5Q00202Q000C000C002D4Q0009000C00024Q000A00073Q00122Q000B00433Q00122Q000C00446Q000D5Q00202Q000D000D002A2Q0002010A000D00022Q0006010B5Q002099000C000A004500205C000C000C004600067E000E00010001000D2Q00E33Q00024Q00C23Q000B4Q00E33Q00014Q00E38Q00E33Q00034Q00E33Q00044Q00E33Q00054Q00E33Q00084Q00E33Q000B4Q00E33Q000C4Q00E33Q00094Q00E33Q000A4Q00E33Q000D4Q000F010C000E00012Q00E3000C00013Q001283000D00064Q0032010E3Q0005003033010E00070047003033010E001600220012BC000F000E3Q00202Q000F000F000B00122Q0010000F3Q00122Q0011000F3Q00122Q0012000F3Q00122Q001300236Q000F0013000200102Q000E000D000F00122Q000F000E3Q00202Q000F000F000B001283001000223Q0012830011000F3Q001283001200223Q001283001300424Q0002010F00130002001063010E0010000F0010D0000E001C00034Q000C000E00024Q000D00013Q00122Q000E00486Q000F3Q000B00302Q000F000700494Q00105Q00202Q00100010002100102Q000F0014001000302Q000F0016004A003033010F0018000F0012BC0010000E3Q00202Q00100010000B00122Q0011000F3Q00122Q0012004B3Q00122Q001300223Q00122Q0014000F6Q00100014000200102Q000F0010001000122Q0010000E3Q00202Q00100010000B2Q00D4001000010002001086010F004C001000122Q001000343Q00202Q00100010004E00202Q00100010004F00102Q000F004D001000302Q000F0050000F00302Q000F0051001A00302Q000F001B001A00102Q000F001C000C4Q001000014Q00E3001100013Q001283001200524Q003201133Q00020012BF001400543Q00206501140014000B00122Q0015000F3Q00122Q001600556Q00140016000200102Q0013005300140012BF001400343Q0020990014001400560020990014001400570010890013005600144Q0011001300024Q001200013Q00122Q001300586Q00143Q000300122Q001500543Q00202Q00150015000B00122Q0016000F3Q00122Q0017001D6Q0015001700020010630114005900150012BF001500543Q00209900150015000B0012830016000F3Q0012830017001D4Q009B01150017000200102Q0014005A001500122Q001500543Q00202Q00150015000B00122Q0016000F3Q00122Q0017001D6Q00150017000200102Q0014005B00154Q001200146Q00103Q00012Q0002010D001000022Q00DC000E00013Q00122Q000F00066Q00103Q000500302Q00100007005C00302Q00100016002200122Q0011000E3Q00202Q00110011000B00122Q0012000F3Q00122Q0013005D3Q00122Q0014000F3Q00122Q0015000F6Q00110015000200102Q0010000D001100122Q0011000E3Q00202Q00110011000B00122Q001200223Q00122Q0013005E3Q00122Q001400223Q00122Q0015000F6Q00110015000200102Q00100010001100102Q0010001C000C4Q000E001000024Q000F00013Q00122Q001000066Q00113Q000800302Q00110007005F4Q00125Q00202Q00120012001E00102Q0011001400124Q001200063Q00102Q00110016001200302Q00110018000F00122Q0012000E3Q00202Q00120012000B00122Q0013000F3Q00122Q0014004B3Q00122Q0015000F3Q00122Q0016000F6Q00120016000200102Q0011000D001200122Q0012000E3Q00202Q00120012000B00122Q0013000F3Q00122Q001400223Q00122Q001500223Q00122Q0016000F6Q00120016000200102Q00110010001200302Q00110024006000102Q0011001C000C4Q000F001100014Q000F000D6Q001000036Q001100036Q001200046Q001300066Q001400056Q0011000300012Q000F010F001100010012BF000F00613Q002099000F000F006200067E00100002000100022Q00E33Q000E4Q00C23Q00024Q003E010F00020001002099000F0008004500205C000F000F004600067E00110003000100052Q00E33Q000F4Q00E33Q00094Q00C23Q00034Q00E33Q000A4Q00E33Q00024Q000F010F001100012Q0006010F5Q00209900100009004500205C00100010004600067E00120004000100052Q00C23Q000F4Q00C23Q000C4Q00E33Q00094Q00C23Q00034Q00E33Q00104Q000F0110001200012Q000601106Q00E30011000B4Q00E30012000C3Q00209900120012006300067E00130005000100032Q00C23Q000B4Q00C23Q00104Q00C23Q00034Q000F01110013000100067E00110006000100102Q00E33Q00014Q00E38Q00C23Q000D4Q00E33Q00034Q00E33Q00074Q00E33Q00114Q00C23Q000E4Q00C23Q00024Q00E33Q00094Q00E33Q00104Q00E33Q000A4Q00E33Q00044Q00E33Q00064Q00E33Q00084Q00E33Q00124Q00E33Q00133Q0010630102006400112Q0010010200024Q005F012Q00013Q00073Q001C3Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574026Q00364003063Q005A496E646578026Q00184003063Q00506172656E74026Q001440026Q002A4003073Q0053756254657874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503534Q00A701035Q00122Q000400016Q00053Q000900302Q00050002000300302Q0005000400054Q000600013Q00202Q00060006000700102Q00050006000600302Q00050008000900122Q0006000B3Q00202Q00060006000C00122Q000700093Q00122Q0008000D6Q00060008000200102Q0005000A000600122Q0006000F3Q00202Q00060006000C00122Q000700096Q000800013Q00122Q0009000D3Q00122Q000A00106Q0006000A000200102Q0005000E000600122Q0006000F3Q00202Q00060006001200122Q000700133Q00122Q000800136Q00060008000200102Q00050011000600302Q0005001400154Q000600023Q00102Q0005001600064Q0003000500024Q000400036Q000500033Q00122Q000600176Q0004000600014Q000400046Q00055Q00122Q000600186Q00078Q000800013Q00202Q0008000800194Q00040008000200122Q0005000B3Q00202Q00050005000C00122Q0006000D3Q00122Q0007000D6Q00050007000200102Q0004000A000500122Q0005000F3Q00202Q00050005000C00122Q0006000D3Q00122Q000700103Q00122Q0008000D3Q00122Q000900106Q00050009000200102Q0004000E000500302Q00040014001500102Q00040016000300202Q00050003001A00202Q00050005001B00067E00073Q000100062Q00E33Q00054Q00C23Q00034Q00E33Q00064Q00C23Q00044Q00C23Q00024Q00E33Q00014Q000F01050007000100209900050003001C00205C00050005001B00067E00070001000100052Q00E33Q00054Q00C23Q00034Q00E33Q00064Q00C23Q00044Q00E33Q00014Q000F0105000700012Q00C2000500034Q00C2000600044Q0089010500034Q005F012Q00013Q00023Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379028Q00030A3Q0054657874436F6C6F723303043Q005465787400124Q00439Q00000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00062Q0004000F00010001000493012Q000F00012Q00E3000400053Q0020990004000400040010630103000300042Q000F012Q000300012Q005F012Q00017Q00043Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F030A3Q0054657874436F6C6F723303073Q0053756254657874000F4Q00E09Q00000100016Q000200026Q00033Q000100302Q0003000100026Q000300019Q004Q000100036Q000200026Q00033Q00014Q000400043Q00202Q00040004000400102Q0003000300046Q000300016Q00017Q00463Q00030E3Q0046696E6446697273744368696C6403143Q004B657962696E645069636B65724F7665726C617903073Q0044657374726F7903053Q004672616D6503043Q004E616D65030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577026Q00E03F03083Q00506F736974696F6E03053Q005544696D32028Q0003043Q0053697A65030A3Q0066726F6D4F2Q66736574025Q00407040025Q00805B4003103Q004261636B67726F756E64436F6C6F7233030A3Q004261636B67726F756E64030F3Q00426F7264657253697A65506978656C03063Q005A496E646578026Q00594003063Q004163746976652Q0103063Q00506172656E74026Q00204003063Q00507572706C65029A5Q99C93F026Q00F83F026Q003440029A5Q99D93F03093Q00546578744C6162656C03043Q005465787403173Q00437573746F6D204B657962696E642053652Q74696E677303083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40026Q002440026Q00F03F026Q00324003163Q004261636B67726F756E645472616E73706172656E6379025Q0040594003093Q005365636F6E64617279026Q004240025Q00806B40026Q003E40026Q001440030A3Q0043552Q52454E543A205B03063Q00737472696E6703053Q00752Q70657203073Q0067657467656E7603093Q00546F2Q676C654B657903123Q005D0A5052452Q5320414E59204B45593Q2E026Q002640030E3Q005465787458416C69676E6D656E7403043Q00456E756D03063Q0043656E746572025Q00805940030A3Q005465787442752Q746F6E03063Q0043616E63656C03073Q005375625465787403073Q00456C656D656E74025Q00805240026Q003640030F3Q004175746F42752Q746F6E436F6C6F720100026Q00104003093Q0041637469766174656403073Q00436F2Q6E656374030A3Q00496E707574426567616E00F14Q00C57Q00206Q000100122Q000200028Q0002000200064Q000B00013Q000493012Q000B000100205C00013Q00032Q003E2Q01000200012Q00062Q016Q00842Q0100014Q005F012Q00014Q00062Q0100014Q00842Q0100014Q00E3000100023Q001283000200044Q003201033Q00090030330103000500020012BF000400073Q0020DF00040004000800122Q000500093Q00122Q000600096Q00040006000200102Q00030006000400122Q0004000B3Q00202Q00040004000800122Q000500093Q00122Q0006000C3Q00122Q000700093Q00122Q0008000C6Q00040008000200102Q0003000A00040012BF0004000B3Q00206501040004000E00122Q0005000F3Q00122Q000600106Q00040006000200102Q0003000D00042Q00E3000400033Q00209900040004001200106301030011000400303301030013000C0030330103001400150030000103001600174Q00045Q00102Q0003001800044Q0001000300024Q000200046Q000300013Q00122Q000400196Q0002000400014Q000200056Q000300014Q00E3000400033Q00205C01040004001A00122Q0005001B3Q00122Q0006001C6Q0002000600014Q000200066Q000300013Q00122Q0004001D3Q00122Q0005001E6Q0002000500014Q000200023Q0012830003001F4Q005501043Q000900302Q0004002000214Q000500073Q00102Q0004002200054Q000500033Q00202Q00050005002000102Q00040023000500302Q00040024002500122Q0005000B3Q00202Q0005000500080012830006000C3Q0012580107000C3Q00122Q0008000C3Q00122Q000900266Q00050009000200102Q0004000A000500122Q0005000B3Q00202Q00050005000800122Q000600273Q00122Q0007000C3Q00122Q0008000C3Q001283000900284Q00020105000900020010630104000D000500303301040029002700303301040014002A0010630104001800012Q00020102000400022Q00E3000300023Q0012AA010400046Q00053Q00074Q000600033Q00202Q00060006002B00102Q00050011000600122Q000600073Q00202Q00060006000800122Q000700093Q00122Q0008000C6Q00060008000200102Q00050006000600122Q0006000B3Q00202Q00060006000800122Q000700093Q00122Q0008000C3Q00122Q0009000C3Q00122Q000A002C6Q0006000A000200102Q0005000A000600122Q0006000B3Q00202Q00060006000800122Q0007000C3Q00122Q0008002D3Q00122Q0009000C3Q00122Q000A002E6Q0006000A000200102Q0005000D000600302Q00050013000C00302Q00050014002A00102Q0005001800014Q0003000500024Q000400046Q000500033Q00122Q0006002F6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006001A00122Q000700096Q0004000700024Q000500023Q00122Q0006001F6Q00073Q000900122Q000800303Q00122Q000900313Q00202Q00090009003200122Q000A00336Q000A0001000200202Q000A000A003400202Q000A000A00054Q00090002000200122Q000A00356Q00080008000A00102Q0007002000084Q000800073Q00102Q0007002200084Q000800033Q00202Q00080008001A00102Q00070023000800302Q00070024003600122Q000800383Q00202Q00080008003700202Q00080008003900102Q00070037000800122Q0008000B3Q00202Q00080008000800122Q000900273Q00122Q000A000C3Q00122Q000B00273Q00122Q000C000C6Q0008000C000200102Q0007000D000800302Q00070029002700302Q00070014003A00102Q0007001800034Q0005000700024Q000600023Q00122Q0007003B6Q00083Q000B00305300080020003C4Q000900073Q00102Q0008002200094Q000900033Q00202Q00090009003D00102Q00080023000900302Q0008002400364Q000900033Q00202Q00090009003E00102Q0008001100090012BF000900073Q0020DF00090009000800122Q000A00093Q00122Q000B000C6Q0009000B000200102Q00080006000900122Q0009000B3Q00202Q00090009000800122Q000A00093Q00122Q000B000C3Q00122Q000C000C3Q00122Q000D003F6Q0009000D000200102Q0008000A00090012560009000B3Q00202Q00090009000800122Q000A000C3Q00122Q000B00153Q00122Q000C000C3Q00122Q000D00406Q0009000D000200102Q0008000D000900302Q00080041004200302Q00080014002A0010630108001800012Q00980106000800024Q000700046Q000800063Q00122Q000900436Q0007000900014Q000700073Q00067E00083Q000100032Q00C23Q00074Q00C23Q00014Q00E33Q00013Q00207C00090006004400202Q0009000900454Q000B00086Q0009000B00014Q000900086Q000A00093Q00202Q000A000A004600067E000B0001000100062Q00E33Q00014Q00C23Q00084Q00C23Q00054Q00E33Q000A4Q00C23Q00044Q00E33Q000B4Q00090009000B00024Q000700096Q0009000C6Q000A00016Q000B00016Q000C00026Q000B000100012Q000F0109000B00012Q005F012Q00013Q00023Q00023Q00030A3Q00446973636F2Q6E65637403073Q0044657374726F7900114Q00E37Q00065B3Q000800013Q000493012Q000800012Q00E37Q00205C5Q00012Q003E012Q000200012Q00278Q0084017Q00E33Q00013Q00065B3Q000E00013Q000493012Q000E00012Q00E33Q00013Q00205C5Q00022Q003E012Q000200012Q0006017Q0084012Q00024Q005F012Q00017Q00163Q00030D3Q0055736572496E7075745479706503043Q00456E756D03083Q004B6579626F61726403073Q0067657467656E7603093Q00546F2Q676C654B657903073Q004B6579436F646503043Q0054657874030E3Q004E4557204B455942494E443A205B03063Q00737472696E6703053Q00752Q70657203043Q004E616D6503013Q005D030A3Q0054657874436F6C6F723303063Q00436F6C6F723303073Q0066726F6D524742026Q005E40025Q00E06F40026Q00644003053Q00436F6C6F7203043Q007461736B03043Q0077616974026Q00E03F02354Q00E300025Q00062E0002000600010001000493012Q000600012Q00E3000200014Q000A0002000100012Q005F012Q00013Q00209900023Q00010012BF000300023Q0020990003000300010020990003000300030006770102003400010003000493012Q003400010012BF000200044Q009700020001000200202Q00033Q000600102Q0002000500034Q000200023Q00122Q000300083Q00122Q000400093Q00202Q00040004000A00202Q00053Q000600202Q00050005000B4Q0004000200020012830005000C4Q001F00030003000500102Q0002000700034Q000200023Q00122Q0003000E3Q00202Q00030003000F00122Q000400103Q00122Q000500113Q00122Q000600126Q00030006000200102Q0002000D00032Q00E3000200034Q0059000300046Q000400056Q00053Q000100122Q0006000E3Q00202Q00060006000F00122Q000700103Q00122Q000800113Q00122Q000900126Q00060009000200102Q0005001300062Q000F010200050001001291000200143Q00202Q00020002001500122Q000300166Q0002000200014Q000200016Q0002000100012Q005F012Q00017Q00083Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00D03F03063Q0069706169727303083Q005F746F2Q676C657303053Q007063612Q6C03043Q0053796E6300154Q00E37Q0020995Q000100065B3Q001400013Q000493012Q001400010012BF3Q00023Q0020995Q0003001283000100044Q003E012Q000200010012963Q00056Q000100013Q00202Q0001000100066Q0002000200044Q001100010012BF000500073Q0020990006000400082Q00C2000700044Q000F010500070001000619012Q000D00010002000493012Q000D0001000493014Q00012Q005F012Q00017Q000A3Q0003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574030C3Q004162736F6C75746553697A6503013Q0058028Q0003043Q007461736B03043Q0077616974020AD7A3703D0AC73F03073Q0044657374726F7900184Q003F019Q000100018Q000200016Q00016Q000100026Q000200036Q00033Q000100122Q000400023Q00202Q0004000400034Q000500023Q00202Q00050005000400202Q00050005000500122Q000600066Q00040006000200102Q0003000100046Q0003000100124Q00073Q00206Q000800122Q000100098Q000200016Q00043Q00206Q000A6Q000200016Q00017Q000C3Q0003073Q0056697369626C65010003043Q0053697A6503053Q005544696D32030A3Q0066726F6D4F2Q66736574025Q00407A40026Q004140026Q00744003043Q007461736B03043Q007761697402B81E85EB51B8BE3F2Q0100264Q00A2019Q009Q009Q006Q00064Q001400013Q000493012Q001400012Q00E33Q00013Q0030293Q000100026Q00026Q000100036Q000200046Q00033Q000100122Q000400043Q00202Q00040004000500122Q000500063Q00122Q000600076Q00040006000200102Q0003000300046Q0003000100044Q002500012Q00E33Q00024Q00E3000100034Q00E3000200044Q003201033Q00010012BF000400043Q00206501040004000500122Q000500063Q00122Q000600086Q00040006000200102Q0003000300042Q000F012Q000300010012BF3Q00093Q0020995Q000A0012830001000B4Q003E012Q000200012Q00E33Q00013Q003033012Q0001000C2Q005F012Q00017Q00063Q0003073Q004B6579436F646503073Q0067657467656E7603093Q00546F2Q676C654B657903043Q00456E756D03013Q004B03073Q0056697369626C6502193Q00062E0001000500010001000493012Q000500012Q00E300025Q00065B0002000600013Q000493012Q000600012Q005F012Q00013Q00209900023Q00010012BF000300024Q00D400030001000200209900030003000300062E0003000F00010001000493012Q000F00010012BF000300043Q0020990003000300010020990003000300050006770102001800010003000493012Q001800012Q00E3000200014Q0066000200024Q0084010200014Q00E3000200024Q00E3000300014Q0066000300033Q0010630102000600032Q005F012Q00017Q00593Q0003063Q005F6F72646572028Q00030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577026Q003C4003073Q0056697369626C6503063Q0048692Q64656E03063Q00506172656E74026Q00144003093Q0049636F6E436F6C6F7203073Q005375625465787403043Q0049636F6E03063Q00636972636C65026Q002E40030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q00184003093Q00546578744C6162656C03043Q004E616D652Q033Q0054616203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64026Q003A40026Q003EC003053Q004672616D6503043Q0050616765030F3Q00426F7264657253697A65506978656C030E3Q005363726F2Q6C696E674672616D65030A3Q0043616E76617353697A6503133Q004175746F6D6174696343616E76617353697A65030D3Q004175746F6D6174696353697A6503013Q005903123Q005363726F2Q6C426172546869636B6E652Q73027Q004003143Q005363726F2Q6C426172496D616765436F6C6F723303063Q005374726F6B65031A3Q005363726F2Q6C426172496D6167655472616E73706172656E6379030C3Q0055494C6973744C61796F757403073Q0050612Q64696E6703043Q005544696D03093Q00536F72744F72646572030B3Q004C61796F75744F7264657203093Q00554950612Q64696E67030A3Q0050612Q64696E67546F70026Q002040030D3Q0050612Q64696E67426F2Q746F6D030B3Q0050612Q64696E674C656674030C3Q0050612Q64696E675269676874030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403043Q005F62746E03053Q005F69636F6E03053Q005F6E616D6503053Q005F7061676503053Q005F7772617003073Q005F73656C656374030A3Q005F69636F6E436F6C6F7203053Q007461626C6503063Q00696E7365727403043Q0054616273030D3Q0043726561746553656374696F6E030C3Q00437265617465546F2Q676C65030C3Q00437265617465536C69646572030D3Q0043726561746554657874626F78030B3Q004372656174654C6162656C030C3Q0043726561746542752Q746F6E03123Q00437265617465436F726E657242752Q746F6E030C3Q00437265617465427579526F770257012Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100024Q003201023Q00010030330102000100022Q00E300035Q001283000400034Q003201053Q00070030330105000400050030330105000600072Q00E3000600013Q0020990006000600090010630105000800060030330105000A000B0012BF0006000D3Q00202E01060006000E00122Q0007000B3Q00122Q000800023Q00122Q000900023Q00122Q000A000F6Q0006000A00020010630105000C00060020990006000100112Q0066000600063Q0010630105001000062Q00E3000600023Q0010D50005001200064Q0003000500024Q000400036Q000500033Q00122Q000600136Q00040006000100202Q00040001001400062Q0004002600010001000493012Q002600012Q00E3000400013Q0020990004000400152Q00E3000500043Q00209900060001001600062E0006002B00010001000493012Q002B0001001283000600173Q001283000700184Q008301088Q000900046Q00050009000200122Q0006001A3Q00202Q00060006000E00122Q000700023Q00122Q0008001B6Q00060008000200102Q00050019000600122Q0006000D3Q00202Q00060006000E00122Q000700023Q00122Q0008001D3Q00122Q0009001B3Q00122Q000A00026Q0006000A000200102Q0005001C000600102Q0005001200034Q00065Q00122Q0007001E6Q00083Q000B00302Q0008000A000B00202Q00090001001F00062Q0009004600010001000493012Q00460001001283000900203Q0010630108000400092Q00EA000900053Q00102Q0008002100094Q000900013Q00202Q00090009001500102Q00080022000900302Q00080023002400122Q000900263Q00202Q00090009002500202Q00090009002700102Q0008002500090012BF000900263Q0020990009000900280020990009000900290010630108002800090012BF0009001A3Q0020DF00090009000E00122Q000A00023Q00122Q000B001B6Q0009000B000200102Q00080019000900122Q0009000D3Q00202Q00090009000E00122Q000A00023Q00122Q000B002A3Q00122Q000C001B3Q00122Q000D00026Q0009000D000200102Q0008001C00090012300009000D3Q00202Q00090009000E00122Q000A000B3Q00122Q000B002B3Q00122Q000C000B3Q00122Q000D00026Q0009000D000200102Q0008000C000900102Q0008001200034Q0006000800022Q00E300075Q0012900008002C6Q00093Q000600302Q0009001F002D00302Q0009000A000B00302Q0009002E000200122Q000A000D3Q00202Q000A000A000E00122Q000B000B3Q00122Q000C00023Q00122Q000D000B3Q001283000E00024Q007E010A000E000200102Q0009000C000A00302Q0009001000074Q000A00063Q00102Q00090012000A4Q0007000900024Q00085Q00122Q0009002F6Q000A3Q000900302Q000A000A000B003033010A002E00020012BC000B000D3Q00202Q000B000B000E00122Q000C000B3Q00122Q000D00023Q00122Q000E000B3Q00122Q000F00026Q000B000F000200102Q000A000C000B00122Q000B000D3Q00202Q000B000B000E2Q00D4000B00010002001063010A0030000B0012BF000B00263Q002099000B000B0032002099000B000B0033001063010A0031000B003033010A003400352Q00E3000B00013Q002099000B000B0037001063010A0036000B003033010A0038001B001063010A001200072Q0032010B00014Q00E3000C5Q001283000D00394Q0032010E3Q00020012BF000F003B3Q002065010F000F000E00122Q001000023Q00122Q0011001D6Q000F0011000200102Q000E003A000F0012BF000F00263Q002099000F000F003C002099000F000F003D001089000E003C000F4Q000C000E00024Q000D5Q00122Q000E003E6Q000F3Q000400122Q0010003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q001000120002001063010F003F00100012BF0010003B3Q00206501100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F004100100012BF0010003B3Q00209900100010000E001283001100023Q001283001200404Q009B01100012000200102Q000F0042001000122Q0010003B3Q00202Q00100010000E00122Q001100023Q00122Q001200406Q00100012000200102Q000F004300104Q000D000F6Q000B3Q00012Q00020108000B000200067E00093Q0001000B2Q00E33Q00074Q00C23Q00024Q00C23Q00014Q00C23Q00074Q00E33Q00084Q00E33Q00094Q00E33Q000A4Q00E33Q00014Q00C23Q00034Q00C23Q00054Q00C23Q00063Q002099000A0003004400205C000A000A004500067E000C0001000100042Q00C23Q00074Q00E33Q00084Q00C23Q00034Q00E33Q000A4Q000F010A000C0001002099000A0003004600205C000A000A004500067E000C0002000100042Q00C23Q00074Q00E33Q00084Q00C23Q00034Q00E33Q000A4Q0047010A000C000100202Q000A0003004700202Q000A000A00454Q000C00096Q000A000C00014Q000A00036Q000B00056Q000C00066Q000D00086Q000E00073Q0010630102004D00090010630102004C000E0010630102004B000D0010630102004A000C00106301020049000B00106301020048000A0010630102004E00040012BF000A004F3Q002074000A000A00504Q000B00073Q00202Q000B000B00514Q000C00026Q000A000C00012Q00E3000A00073Q002099000A000A00512Q0092000A000A3Q002646000A2Q002Q01000B000493013Q002Q012Q00C2000A00094Q000A000A0001000100067E000A0003000100072Q00C23Q00024Q00E38Q00E33Q00014Q00C23Q00084Q00E33Q00034Q00E33Q000B4Q00E33Q000C3Q00067E000B0004000100062Q00C23Q00024Q00E38Q00C23Q00084Q00E33Q00044Q00E33Q00014Q00E33Q000D3Q00106301020052000B00067E000B0005000100092Q00C23Q000A4Q00E38Q00E33Q00044Q00E33Q00014Q00E33Q00054Q00E33Q00034Q00E33Q00084Q00E33Q000A4Q00E33Q00073Q00106301020053000B00067E000B0006000100072Q00C23Q000A4Q00E38Q00E33Q00054Q00E33Q00014Q00E33Q000D4Q00E33Q00034Q00E33Q000E3Q00106301020054000B00067E000B0007000100092Q00C23Q000A4Q00E38Q00E33Q00054Q00E33Q00014Q00E33Q00034Q00E33Q000B4Q00E33Q000C4Q00E33Q00084Q00E33Q000A3Q00106301020055000B00067E000B0008000100062Q00C23Q000A4Q00E33Q00044Q00E33Q00014Q00E38Q00E33Q00054Q00E33Q000D3Q00106301020056000B00067E000B0009000100072Q00C23Q000A4Q00E38Q00E33Q00044Q00E33Q00014Q00E33Q000D4Q00E33Q00084Q00E33Q000A3Q00106301020057000B00067E000B000A0001000A2Q00C23Q00084Q00E38Q00E33Q00014Q00C23Q00074Q00E33Q00034Q00E33Q000B4Q00E33Q00044Q00E33Q000D4Q00E33Q00084Q00E33Q000A3Q00106301020058000B00067E000B000B000100072Q00C23Q000A4Q00E38Q00E33Q00054Q00E33Q00014Q00E33Q000D4Q00E33Q00034Q00E33Q000F3Q00106301020059000B2Q0010010200024Q005F012Q00013Q000C3Q00193Q0003083Q005F63752Q72656E7403063Q0048692Q64656E03063Q0069706169727303043Q005461627303053Q005F7772617003073Q0056697369626C6501002Q0103083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577027B14AE47E17AA43F028Q0003043Q005F62746E03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03053Q005F69636F6E030A3Q0054657874436F6C6F7233030A3Q005F69636F6E436F6C6F7203073Q005375625465787403053Q005F6E616D6503113Q0053656C656374656449636F6E436F6C6F7203093Q0049636F6E436F6C6F7203063Q00412Q63656E7403043Q005465787400914Q00E37Q0020995Q00012Q00E3000100013Q000677012Q000600010001000493012Q000600012Q005F012Q00014Q00E33Q00023Q0020995Q000200065B3Q003200013Q000493012Q003200010012BF3Q00034Q00E300015Q0020990001000100042Q00523Q00020002000493012Q001400012Q00E3000500013Q00064D0004001400010005000493012Q00140001002099000500040005003033010500060007000619012Q000F00010002000493012Q000F00012Q00E38Q00422Q0100013Q00104Q000100016Q00033Q00304Q000600086Q00033Q00122Q0001000A3Q00202Q00010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q00010005000200104Q000900016Q00046Q000100036Q000200056Q00033Q000100122Q0004000A3Q00202Q00040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q00040008000200102Q0003000900046Q000300016Q00013Q0012BF3Q00034Q00E300015Q0020990001000100042Q00523Q00020002000493012Q005500012Q00E3000500013Q00064D0004003C00010005000493012Q003C00010020990005000400050030330105000600072Q00E3000500043Q00209900060004000E2Q00E3000700064Q003201083Q00010030330108000F00102Q000F0105000800012Q00E3000500043Q0020990006000400112Q00E3000700064Q003201083Q000100209900090004001300062E0009004B00010001000493012Q004B00012Q00E3000900073Q0020990009000900140010630108001200092Q008F0105000800014Q000500043Q00202Q0006000400154Q000700066Q00083Q00014Q000900073Q00202Q00090009001400102Q0008001200094Q000500080001000619012Q003700010002000493012Q003700012Q00E38Q00E3000100013Q001063012Q000100012Q00E33Q00033Q003033012Q000600082Q00E33Q00033Q0012BF0001000A3Q00202E2Q010001000B00122Q0002000C3Q00122Q0003000D3Q00122Q0004000D3Q00122Q0005000D6Q000100050002001063012Q000900012Q00E33Q00044Q00E3000100034Q00E3000200054Q003201033Q00010012BF0004000A3Q00202E01040004000B00122Q0005000D3Q00122Q0006000D3Q00122Q0007000D3Q00122Q0008000D6Q0004000800020010630103000900042Q000F012Q000300012Q00E33Q00044Q00692Q0100086Q000200066Q00033Q000100302Q0003000F000D6Q000300012Q00E33Q00044Q00E3000100094Q00E3000200064Q003201033Q00012Q00E3000400023Q00209900040004001600062E0004008600010001000493012Q008600012Q00E3000400023Q00209900040004001700062E0004008600010001000493012Q008600012Q00E3000400073Q0020990004000400180010630103001200042Q00A3012Q000300016Q00046Q0001000A6Q000200066Q00033Q00014Q000400073Q00202Q00040004001900102Q0003001200046Q000300016Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q33E33F000B4Q00E37Q0020995Q000100062E3Q000A00010001000493012Q000A00012Q00E33Q00014Q00692Q0100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q005F012Q00017Q00033Q0003073Q0056697369626C6503163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F000B4Q00E37Q0020995Q000100062E3Q000A00010001000493012Q000A00012Q00E33Q00014Q00692Q0100026Q000200036Q00033Q000100302Q0003000200036Q000300012Q005F012Q00017Q000F3Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7403043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003C40030B3Q004C61796F75744F72646572030F3Q00426F7264657253697A65506978656C03063Q00506172656E74026Q00144003063Q005374726F6B6501284Q00AB00018Q00025Q00202Q00020002000100202Q00020002000200102Q0001000100024Q000100013Q00122Q000200036Q00033Q00054Q000400023Q00202Q00040004000500102Q00030004000400122Q000400073Q00202Q00040004000800122Q000500023Q00122Q000600093Q00122Q000700093Q00062Q0008001300013Q000493012Q001300010012830008000A4Q00020104000800020010120003000600044Q00045Q00202Q00040004000100102Q0003000B000400302Q0003000C00094Q000400033Q00102Q0003000D00044Q0001000300024Q000200046Q000300013Q00122Q0004000E6Q0002000400014Q000200056Q000300016Q000400023Q00202Q00040004000F4Q000500066Q0002000500014Q000100028Q00017Q00213Q0003063Q005F6F72646572026Q00F03F03053Q004672616D6503163Q004261636B67726F756E645472616E73706172656E637903043Q0053697A6503053Q005544696D322Q033Q006E6577028Q00026Q003640030B3Q004C61796F75744F7264657203063Q00506172656E7403043Q0049636F6E030D3Q00637562652D7665727465786573026Q002C4003093Q0049636F6E436F6C6F7203063Q00412Q63656E74030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E027Q004003093Q00546578744C6162656C03043Q005465787403043Q004E616D6503073Q0053656374696F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002A40030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q0036C002643Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100024Q00E300026Q00F600035Q00202Q00030003000100202Q00030003000200102Q0002000100034Q000200013Q00122Q000300036Q00043Q000400302Q00040004000200122Q000500063Q00202Q00050005000700122Q000600023Q00122Q000700083Q00122Q000800083Q00122Q000900096Q00050009000200102Q0004000500054Q00055Q00202Q00050005000100102Q0004000A00054Q000500023Q00102Q0004000B00054Q0002000400024Q000300033Q00202Q00040001000C00062Q0004002000010001000493012Q002000010012830004000D3Q0012830005000E4Q000601065Q00209900070001000F00062E0007002700010001000493012Q002700012Q00E3000700043Q0020990007000700102Q000201030007000200125F000400123Q00202Q00040004000700122Q000500083Q00122Q000600136Q00040006000200102Q00030011000400122Q000400063Q00202Q00040004000700122Q000500083Q00122Q000600153Q00122Q000700133Q00122Q000800086Q00040008000200102Q00030014000400102Q0003000B00024Q000400013Q00122Q000500166Q00063Q000A00302Q00060004000200202Q00070001001800062Q0007003F00010001000493012Q003F0001001283000700193Q0010630106001700072Q00FE000700053Q00102Q0006001A00074Q000700043Q00202Q00070007001700102Q0006001B000700302Q0006001C001D00122Q0007001F3Q00202Q00070007001E00202Q00070007002000102Q0006001E000700122Q000700123Q00202Q00070007000700122Q000800083Q00122Q000900136Q00070009000200102Q00060011000700122Q000700063Q00202Q00070007000700122Q000800083Q00122Q000900093Q00122Q000A00133Q00122Q000B00086Q0007000B000200102Q00060014000700122Q000700063Q00202Q00070007000700122Q000800023Q00122Q000900213Q00122Q000A00023Q00122Q000B00086Q0007000B000200102Q00060005000700102Q0006000B00024Q0004000600014Q000200028Q00017Q003B3Q0003073Q0044656661756C74026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q0003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00546578744C6162656C03043Q004E616D6503063Q00546F2Q676C6503083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674025Q0080474003053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q66026Q0020C0030A3Q0066726F6D4F2Q66736574026Q002Q40026Q003040030F3Q00426F7264657253697A65506978656C026Q002CC0027Q0040026Q0018402Q033Q005365742Q033Q0047657403053Q00537461746503043Q00466C616703043Q0053796E6303053Q007461626C6503063Q00696E7365727403083Q005F746F2Q676C657303093Q0041637469766174656403073Q00436F2Q6E65637403083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03083Q00496E7374616E636502F13Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100023Q00209900020001000100062E0002000800010001000493012Q000800012Q000601026Q00E300035Q001204000400026Q0003000200024Q000400013Q00122Q000500036Q00063Q000400302Q00060004000500302Q00060006000700122Q000700093Q00202Q00070007000A00122Q000800073Q00122Q0009000B3Q00122Q000A00073Q00122Q000B000B6Q0007000B000200102Q00060008000700102Q0006000C00034Q00040006000200122Q0005000D3Q00202Q00060001000E00062Q0006003800013Q000493012Q003800012Q00E3000600023Q00202Q01070001000E00122Q0008000F6Q00095Q00202Q000A0001001000062Q000A002700010001000493012Q002700012Q00E3000A00033Q002099000A000A00112Q00020106000A00020012BF000700133Q0020DF00070007000A00122Q0008000B3Q00122Q000900146Q00070009000200102Q00060012000700122Q000700093Q00202Q00070007000A00122Q0008000B3Q00122Q0009000D3Q00122Q000A00143Q00122Q000B000B6Q0007000B000200102Q0006001500070010630106000C0004001283000500164Q00E3000600013Q001295010700176Q00083Q000A00302Q00080006000700202Q00090001001800062Q0009004000010001000493012Q00400001001283000900193Q0010630108000400092Q008E000900043Q00102Q0008001A00094Q000900033Q00202Q00090009000400102Q0008001B000900302Q0008001C001D00122Q0009001F3Q00202Q00090009001E00202Q00090009002000102Q0008001E000900122Q000900133Q00202Q00090009000A00122Q000A000B3Q00122Q000B00146Q0009000B000200102Q00080012000900122Q000900093Q00202Q00090009000A00122Q000A000B6Q000B00053Q00122Q000C00143Q00122Q000D000B6Q0009000D000200102Q00080015000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00202Q000B000500214Q000B000B3Q00122Q000C00073Q00122Q000D000B6Q0009000D000200102Q00080008000900102Q0008000C00044Q0006000800014Q000600013Q00122Q000700226Q00083Q000600062Q0002006D00013Q000493012Q006D00012Q00E3000900033Q00209900090009002400062E0009006F00010001000493012Q006F00012Q00E3000900033Q0020990009000900250010630108002300090012BF000900133Q0020DF00090009000A00122Q000A00073Q00122Q000B00146Q0009000B000200102Q00080012000900122Q000900093Q00202Q00090009000A00122Q000A00073Q00122Q000B00263Q00122Q000C00143Q00122Q000D000B6Q0009000D000200102Q0008001500090012BE000900093Q00202Q00090009002700122Q000A00283Q00122Q000B00296Q0009000B000200102Q00080008000900302Q0008002A000B00102Q0008000C00044Q0006000800024Q000700054Q00C2000800063Q00124B0009000D6Q0007000900014Q000700013Q00122Q000800226Q00093Q00064Q000A00033Q00202Q000A000A000400102Q00090023000A00122Q000A00133Q00202Q000A000A000A001283000B000B3Q001283000C00144Q0002010A000C000200106301090012000A00065B000200A200013Q000493012Q00A200010012BF000A00093Q002013010A000A000A00122Q000B00073Q00122Q000C002B3Q00122Q000D00143Q00122Q000E000B6Q000A000E000200062Q000A00A900010001000493012Q00A900010012BF000A00093Q00202E010A000A000A00122Q000B000B3Q00122Q000C002C3Q00122Q000D00143Q00122Q000E000B6Q000A000E000200106301090015000A0012BE000A00093Q00202Q000A000A002700122Q000B001D3Q00122Q000C001D6Q000A000C000200102Q00090008000A00302Q0009002A000B00102Q0009000C00064Q0007000900024Q000800054Q00C2000900073Q001283000A002D4Q000F0108000A00012Q003201085Q00067E00093Q000100072Q00C23Q00024Q00E33Q00064Q00C23Q00064Q00E33Q00074Q00E33Q00034Q00C23Q00074Q00C23Q00013Q0010630108002E000900067E00090001000100012Q00C23Q00023Q0010630108002F000900209900090001003000062E000900CC00010001000493012Q00CC000100209900090001003100065B000900CC00013Q000493012Q00CC000100067E00090002000100012Q00C23Q00013Q00067E000A0003000100082Q00C23Q00094Q00C23Q00024Q00E33Q00064Q00C23Q00064Q00E33Q00074Q00E33Q00034Q00C23Q00074Q00C23Q00013Q00106301080032000A00065B000900DE00013Q000493012Q00DE00010012BF000A00333Q002074000A000A00344Q000B00083Q00202Q000B000B00354Q000C00086Q000A000C0001002099000A0004003600205C000A000A003700067E000C0004000100022Q00C23Q00084Q00C23Q00024Q000F010A000C000100065B000200EE00013Q000493012Q00EE0001002099000A0001003800065B000A00EE00013Q000493012Q00EE00010012BF000A00393Q002099000A000A003A002099000B000100382Q0006010C00014Q000F010A000C00010010630108003B00032Q0010010800024Q005F012Q00013Q00053Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02344Q00FC00018Q000200016Q000300026Q000400036Q00053Q00014Q00065Q00062Q0006000C00013Q000493012Q000C00012Q00E3000600043Q00209900060006000200062E0006000E00010001000493012Q000E00012Q00E3000600043Q0020990006000600030010630105000100062Q00580002000500014Q000200016Q000300056Q000400036Q00053Q00014Q00065Q00062Q0006002000013Q000493012Q002000010012BF000600053Q00201301060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A000200062Q0006002700010001000493012Q002700010012BF000600053Q00202E01060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A00020010630105000400062Q000F0102000500012Q00E3000200063Q00209900020002000C00065B0002003300013Q000493012Q003300010012BF0002000D3Q00209900020002000E2Q00E3000300063Q00209900030003000C2Q00E300046Q000F0102000400012Q005F012Q00019Q002Q0001034Q00E300016Q00102Q0100024Q005F012Q00017Q00033Q0003073Q0067657467656E7603043Q00466C61672Q01000B3Q0012BF3Q00014Q00D43Q000100022Q00E300015Q0020990001000100022Q00C35Q00010026CD3Q000800010003000493012Q000800012Q00CF8Q0006012Q00014Q0010012Q00024Q005F012Q00017Q000E3Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E742Q033Q004F2Q6603083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q00F03F026Q002CC0026Q00E03F028Q00027Q004003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E01464Q00E300015Q00062E0001000400010001000493012Q000400012Q005F012Q00014Q00E300016Q00D400010001000200065B0001000B00013Q000493012Q000B00012Q00062Q0100013Q00062E0001000C00010001000493012Q000C00012Q00062Q016Q00E3000200013Q0006772Q01001000010002000493012Q001000012Q005F012Q00014Q00842Q0100014Q00E3000200024Q00E3000300034Q00E3000400044Q003201053Q00012Q00E3000600013Q00065B0006001C00013Q000493012Q001C00012Q00E3000600053Q00209900060006000200062E0006001E00010001000493012Q001E00012Q00E3000600053Q0020990006000600030010630105000100062Q00580002000500014Q000200026Q000300066Q000400046Q00053Q00014Q000600013Q00062Q0006003000013Q000493012Q003000010012BF000600053Q00201301060006000600122Q000700073Q00122Q000800083Q00122Q000900093Q00122Q000A000A6Q0006000A000200062Q0006003700010001000493012Q003700010012BF000600053Q00202E01060006000600122Q0007000A3Q00122Q0008000B3Q00122Q000900093Q00122Q000A000A6Q0006000A00020010630105000400062Q000F01020005000100062E0001004500010001000493012Q004500012Q00E3000200073Q00209900020002000C00065B0002004500013Q000493012Q004500010012BF0002000D3Q00209900020002000E2Q00E3000300073Q00209900030003000C2Q000601046Q000F0102000400012Q005F012Q00017Q00013Q002Q033Q0053657400064Q0039016Q00206Q00014Q000200016Q000200028Q000200016Q00017Q003A3Q002Q033Q004D696E028Q002Q033Q004D6178026Q00594003093Q00496E6372656D656E74026Q00F03F03043Q006D61746803053Q00636C616D7003073Q0044656661756C74026Q00454003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E637903043Q005465787403043Q004E616D6503063Q00536C6964657203083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403083Q00506F736974696F6E03053Q005544696D322Q033Q006E6577026Q002040026Q00104003043Q0053697A65026Q004EC0026Q002C4003063Q00506172656E7403083Q00746F737472696E6703063Q00412Q63656E7403053Q005269676874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q0020C0026Q004940030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66026Q00E03F026Q0026C0026Q0030C0026Q002440030F3Q00426F7264657253697A65506978656C026Q00084003053Q004672616D65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578027Q0040026Q0018402Q033Q005365742Q033Q0047657403083Q00496E7374616E636502E53Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100023Q00209900020001000100062E0002000800010001000493012Q00080001001283000200023Q00209900030001000300062E0003000C00010001000493012Q000C0001001283000300043Q00209900040001000500062E0004001000010001000493012Q00100001001283000400063Q0012BF000500073Q00209900050005000800209900060001000900062E0006001600010001000493012Q001600012Q00C2000600024Q00C2000700024Q00C2000800034Q00020105000800022Q00E300065Q00124F0007000A6Q0006000200024Q000700013Q00122Q0008000B6Q00093Q000900302Q0009000C000600202Q000A0001000E00062Q000A002400010001000493012Q00240001001283000A000F3Q0010630109000D000A2Q00EA000A00023Q00102Q00090010000A4Q000A00033Q00202Q000A000A000D00102Q00090011000A00302Q00090012001300122Q000A00153Q00202Q000A000A001400202Q000A000A001600102Q00090014000A0012BC000A00183Q00202Q000A000A001900122Q000B00023Q00122Q000C001A3Q00122Q000D00023Q00122Q000E001B6Q000A000E000200102Q00090017000A00122Q000A00183Q00202Q000A000A0019001283000B00063Q001283000C001D3Q001283000D00023Q001283000E001E4Q0002010A000E00020010630109001C000A0010630109001F00062Q000F0107000900012Q00E3000700013Q0012830008000B4Q003201093Q000A0030330109000C00060012BF000A00204Q006C010B00056Q000A0002000200102Q0009000D000A4Q000A00043Q00102Q00090010000A4Q000A00033Q00202Q000A000A002100102Q00090011000A00302Q00090012001300122Q000A00153Q002099000A000A0014002099000A000A002200106301090014000A0012BF000A00243Q0020DF000A000A001900122Q000B00063Q00122Q000C00026Q000A000C000200102Q00090023000A00122Q000A00183Q00202Q000A000A001900122Q000B00063Q00122Q000C00253Q00122Q000D00023Q00122Q000E001B6Q000A000E000200102Q00090017000A001230000A00183Q00202Q000A000A001900122Q000B00023Q00122Q000C00263Q00122Q000D00023Q00122Q000E001E6Q000A000E000200102Q0009001C000A00102Q0009001F00064Q0007000900022Q00E3000800013Q0012DE000900276Q000A3Q000800302Q000A000D002800302Q000A0029002A4Q000B00033Q00202Q000B000B002C00102Q000A002B000B00122Q000B00243Q0020DF000B000B001900122Q000C00023Q00122Q000D002D6Q000B000D000200102Q000A0023000B00122Q000B00183Q00202Q000B000B001900122Q000C00023Q00122Q000D001A3Q00122Q000E00063Q00122Q000F002E6Q000B000F000200102Q000A0017000B0012BF000B00183Q00202E010B000B001900122Q000C00063Q00122Q000D002F3Q00122Q000E00023Q00122Q000F00306Q000B000F0002001063010A001C000B003033010A00310002001063010A001F00062Q00020108000A00022Q002C010900056Q000A00083Q00122Q000B00326Q0009000B00014Q000900013Q00122Q000A00336Q000B3Q00044Q000C00033Q00202Q000C000C002100102Q000B002B000C0012BF000C00183Q0020B3000C000C00194Q000D000500024Q000E000300024Q000D000D000E00122Q000E00023Q00122Q000F00063Q00122Q001000026Q000C0010000200102Q000B001C000C00302Q000B00310002001063010B001F00082Q00020109000B00022Q002C010A00056Q000B00093Q00122Q000C00326Q000A000C00014Q000A00013Q00122Q000B00336Q000C3Q00074Q000D00033Q00202Q000D000D000D00102Q000C002B000D0012BF000D00243Q002065010D000D001900122Q000E002D3Q00122Q000F002D6Q000D000F000200102Q000C0023000D0012BF000D00183Q002099000D000D00192Q0042000E000500022Q0042000F000300022Q0005010E000E000F001247000F00023Q00122Q0010002D3Q00122Q001100026Q000D0011000200102Q000C0017000D00122Q000D00183Q00202Q000D000D003400122Q000E00133Q00122Q000F00136Q000D000F0002001063010C001C000D003033010C00310002003033010C00350036001063010C001F00082Q0002010A000C00022Q00E3000B00054Q00C2000C000A3Q001283000D00374Q000F010B000D00012Q0032010B5Q00067E000C3Q000100082Q00C23Q00024Q00C23Q00034Q00C23Q00054Q00C23Q00044Q00C23Q00094Q00C23Q000A4Q00C23Q00074Q00C23Q00014Q00E3000D00064Q00C2000E00083Q00067E000F0001000100012Q00C23Q000C4Q000F010D000F000100067E000D0002000100032Q00C23Q000C4Q00C23Q00024Q00C23Q00033Q001063010B0038000D00067E000D0003000100012Q00C23Q00053Q001063010B0039000D001063010B003A00062Q0010010B00024Q005F012Q00013Q00043Q000F3Q0003043Q006D61746803053Q00636C616D7003053Q00666C2Q6F72026Q00E03F028Q0003043Q0053697A6503053Q005544696D322Q033Q006E6577026Q00F03F03083Q00506F736974696F6E03043Q005465787403083Q00746F737472696E6703083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E02474Q00AD00028Q000300016Q00048Q0003000300044Q000300036Q00020002000300122Q000300013Q00202Q00030003000200122Q000400013Q00202Q0004000400034Q000500036Q00050002000500202Q0005000500044Q0004000200024Q000500036Q0004000400054Q00058Q000600016Q0003000600024Q000300026Q000300016Q00048Q00030003000400262Q0003001C00010005000493012Q001C0001001283000300053Q00062E0003002300010001000493012Q002300012Q00E3000300024Q00E300046Q00420003000300042Q00E3000400014Q00E300056Q00420004000400052Q00050103000300042Q00E3000400043Q00128D010500073Q00202Q0005000500084Q000600033Q00122Q000700053Q00122Q000800093Q00122Q000900056Q00050009000200102Q0004000600054Q000400053Q00122Q000500073Q0020990005000500082Q00C2000600033Q001283000700053Q001283000800043Q001283000900054Q00020105000900020010630104000A00052Q00E3000400063Q0012BF0005000C4Q00E3000600024Q00B70005000200020010630104000B000500065B0001004600013Q000493012Q004600012Q00E3000400073Q00209900040004000D00065B0004004600013Q000493012Q004600010012BF0004000E3Q00209900040004000F2Q00E3000500073Q00209900050005000D2Q00E3000600024Q000F0104000600012Q005F012Q00019Q002Q0001054Q00DD00018Q00028Q000300016Q0001000300016Q00017Q00023Q0003043Q006D61746803053Q00636C616D7002104Q007600025Q00122Q000300013Q00202Q0003000300024Q000400016Q000500016Q000600026Q0003000600024Q000400016Q0003000300044Q000400026Q000500016Q0004000400054Q0003000300044Q000400016Q0002000400016Q00019Q002Q0001034Q00E300016Q00102Q0100024Q005F012Q00017Q00453Q00026Q003C4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503073Q0054657874626F7803083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65029A5Q99D93F026Q0020C003063Q00506172656E7403053Q004672616D6503103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E64617279026Q0018C0026Q003440030D3Q004175746F6D6174696353697A6503013Q005803103Q00436C69707344657363656E64616E74732Q01030F3Q00426F7264657253697A65506978656C03093Q00554950612Q64696E67030B3Q0050612Q64696E674C65667403043Q005544696D026Q001440030C3Q0050612Q64696E675269676874026Q00104003063Q005374726F6B6503073Q0054657874426F7803073Q0044656661756C74034Q00030F3Q00506C616365686F6C64657254657874030B3Q00506C616365686F6C6465722Q033Q003Q2E03113Q00506C616365686F6C646572436F6C6F723303073Q0053756254657874026Q00264003103Q00436C656172546578744F6E466F637573010003103Q00554953697A65436F6E73747261696E7403073Q004D696E53697A6503083Q004D696E5769647468025Q0080464003073Q004D617853697A6503083Q004D61785769647468025Q0080614003043Q006D61746803043Q006875676503073Q00466F637573656403073Q00436F2Q6E65637403093Q00466F6375734C6F73742Q033Q005365742Q033Q0047657403083Q00496E7374616E636502D23Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100024Q00E300025Q00124F000300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000A00302Q00050003000400202Q00060001000600062Q0006000F00010001000493012Q000F0001001283000600073Q0010630105000500062Q00EA000600023Q00102Q0005000800064Q000600033Q00202Q00060006000500102Q00050009000600302Q0005000A000B00122Q0006000D3Q00202Q00060006000C00202Q00060006000E00102Q0005000C00060012BF000600103Q0020DF00060006001100122Q000700123Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00202Q00060006001100122Q000700123Q00122Q000800163Q00122Q000900133Q00122Q000A00126Q0006000A000200102Q00050014000600123E000600153Q00202Q00060006001100122Q000700183Q00122Q000800193Q00122Q000900043Q00122Q000A00126Q0006000A000200102Q00050017000600102Q0005001A00024Q0003000500012Q00E3000300013Q0012830004001B4Q003201053Q00082Q00E3000600033Q00209900060006001D0010630105001C00060012BF000600103Q0020DF00060006001100122Q000700043Q00122Q000800136Q00060008000200102Q0005000F000600122Q000600153Q00202Q00060006001100122Q000700043Q00122Q0008001E3Q00122Q000900133Q00122Q000A00126Q0006000A000200102Q0005001400060012BC000600153Q00202Q00060006001100122Q000700123Q00122Q000800123Q00122Q000900123Q00122Q000A001F6Q0006000A000200102Q00050017000600122Q0006000D3Q00202Q0006000600200020990006000600210010630105002000060030330105002200230030330105002400120010630105001A00022Q003201066Q00E3000700013Q001283000800254Q003201093Q00020012BF000A00273Q002099000A000A0011001283000B00123Q001283000C00284Q009B010A000C000200102Q00090026000A00122Q000A00273Q00202Q000A000A001100122Q000B00123Q00122Q000C00286Q000A000C000200102Q00090029000A4Q000700096Q00063Q00012Q00020103000600022Q0008000400046Q000500033Q00122Q0006002A6Q0004000600014Q000400056Q000500036Q000600033Q00202Q00060006002B4Q000700066Q0004000700024Q000500013Q00122Q0006002C6Q00073Q000C00302Q00070003000400202Q00080001002D00062Q0008007A00010001000493012Q007A00010012830008002E3Q00106301070005000800209900080001003000062E0008007F00010001000493012Q007F0001001283000800313Q0010630107002F00082Q00A5000800033Q00202Q00080008003300102Q0007003200084Q000800023Q00102Q0007000800084Q000800033Q00202Q00080008000500102Q00070009000800302Q0007000A003400302Q00070035003600122Q0008000D3Q00202Q00080008000C00202Q00080008000E00102Q0007000C000800122Q0008000D3Q00202Q00080008002000202Q00080008002100102Q00070020000800122Q000800153Q00202Q00080008001100122Q000900123Q00122Q000A00123Q00122Q000B00043Q00122Q000C00126Q0008000C000200102Q00070017000800102Q0007001A00034Q00088Q000900013Q00122Q000A00376Q000B3Q000200122Q000C00103Q00202Q000C000C001100202Q000D0001003900062Q000D00A500010001000493012Q00A50001001283000D003A3Q001283000E00124Q0002010C000E0002001063010B0038000C0012BF000C00103Q002099000C000C0011002099000D0001003C00062E000D00AE00010001000493012Q00AE0001001283000D003D3Q0012BF000E003E3Q002099000E000E003F2Q0002010C000E0002001063010B003B000C2Q004B0109000B4Q00F000083Q00012Q000201050008000200209900060005004000205C00060006004100067E00083Q000100042Q00E33Q00074Q00C23Q00044Q00E33Q00084Q00E33Q00034Q000F01060008000100209900060005004200205C00060006004100067E00080001000100072Q00E33Q00074Q00C23Q00044Q00E33Q00084Q00E33Q00034Q00E33Q00064Q00C23Q00014Q00C23Q00054Q000F0106000800012Q003201063Q000300067E00070002000100012Q00C23Q00053Q00106301060043000700067E00070003000100012Q00C23Q00053Q0010630106004400070010630106004500022Q0010010600024Q005F012Q00013Q00043Q00043Q0003053Q00436F6C6F7203063Q00412Q63656E74030C3Q005472616E73706172656E6379029A5Q99C93F000A4Q005E9Q00000100016Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030001000400302Q0003000300046Q000300016Q00017Q00073Q0003053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E637903083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E03043Q005465787400164Q00E38Q00E3000100014Q00E3000200024Q003201033Q00022Q00E3000400033Q0020990004000400020010630103000100042Q00E3000400043Q0010630103000300042Q000F012Q000300012Q00E33Q00053Q0020995Q000400065B3Q001500013Q000493012Q001500010012BF3Q00053Q00201A014Q00064Q000100053Q00202Q0001000100044Q000200063Q00202Q0002000200076Q000200012Q005F012Q00017Q00013Q0003043Q005465787402034Q00E300025Q0010630102000100012Q005F012Q00017Q00013Q0003043Q005465787400044Q00E37Q0020995Q00012Q0010012Q00024Q005F012Q00017Q00253Q00026Q003C40026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D3203063Q00506172656E74026Q003A4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403043Q004E616D6503053Q004C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C65667403043Q0053697A6503053Q0056616C7565034Q0003063Q00412Q63656E7403053Q005269676874026Q0020C02Q033Q005365742Q033Q0047657403083Q00496E7374616E636502853Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100024Q00E300025Q001204010300016Q00020002000200122Q000300023Q00202Q00040001000300062Q0004002500013Q000493012Q002500012Q00E3000400013Q00202Q01050001000300122Q000600046Q00075Q00202Q00080001000500062Q0008001400010001000493012Q001400012Q00E3000800023Q0020990008000800062Q00020104000800020012BF000500083Q0020DF00050005000900122Q0006000A3Q00122Q0007000B6Q00050007000200102Q00040007000500122Q0005000D3Q00202Q00050005000900122Q0006000A3Q00122Q000700023Q00122Q0008000B3Q00122Q0009000A6Q00050009000200102Q0004000C00050010630104000E00020012830003000F4Q00E3000400033Q001295010500106Q00063Q000A00302Q00060011001200202Q00070001001400062Q0007002D00010001000493012Q002D0001001283000700153Q0010630106001300072Q00EA000700043Q00102Q0006001600074Q000700023Q00202Q00070007001300102Q00060017000700302Q00060018001900122Q0007001B3Q00202Q00070007001A00202Q00070007001C00102Q0006001A00070012BF000700083Q00206501070007000900122Q0008000A3Q00122Q0009000B6Q00070009000200102Q0006000700070012BF0007000D3Q0020990007000700090012830008000A4Q00C2000900033Q001283000A000B3Q001283000B000A4Q00020107000B00020010630106000C00070012BF0007000D3Q0020990007000700090012830008000B4Q0064000900033Q001283000A00123Q001283000B000A4Q00020107000B00020010630106001D00070010630106000E00022Q00020104000600022Q00E3000500033Q001295010600106Q00073Q000A00302Q00070011001200202Q00080001001E00062Q0008005800010001000493012Q005800010012830008001F3Q0010630107001300082Q00EA000800053Q00102Q0007001600084Q000800023Q00202Q00080008002000102Q00070017000800302Q00070018001900122Q0008001B3Q00202Q00080008001A00202Q00080008002100102Q0007001A00080012BF000800083Q0020DF00080008000900122Q000900123Q00122Q000A000B6Q0008000A000200102Q00070007000800122Q0008000D3Q00202Q00080008000900122Q000900123Q00122Q000A00223Q00122Q000B000B3Q00122Q000C000A6Q0008000C000200102Q0007000C00080012300008000D3Q00202Q00080008000900122Q0009000B3Q00122Q000A00223Q00122Q000B00123Q00122Q000C000A6Q0008000C000200102Q0007001D000800102Q0007000E00024Q0005000700022Q003201063Q000300067E00073Q000100012Q00C23Q00053Q00106301060023000700067E00070001000100012Q00C23Q00053Q0010630106002400070010630106002500022Q0010010600024Q005F012Q00013Q00023Q00023Q0003043Q005465787403083Q00746F737472696E6702064Q00E300025Q0012BF000300024Q00C2000400014Q00B70003000200020010630102000100032Q005F012Q00017Q00013Q0003043Q005465787400044Q00E37Q0020995Q00012Q0010012Q00024Q005F012Q00017Q002C3Q00026Q003C40030A3Q005465787442752Q746F6E03043Q0054657874034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q0053697A6503053Q005544696D322Q033Q006E6577028Q0003063Q00506172656E74026Q00204003043Q0049636F6E026Q002C4003093Q0049636F6E436F6C6F7203073Q0053756254657874030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00E03F03083Q00506F736974696F6E026Q003A4003093Q00526967687449636F6E030E3Q00526967687449636F6E436F6C6F72026Q0020C003093Q00546578744C6162656C03043Q004E616D6503063Q0042752Q746F6E03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E6365030C3Q00536574526967687449636F6E02A53Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100024Q00E300025Q001271000300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000500302Q00050003000400302Q00050005000600302Q00050007000800122Q0006000A3Q00202Q00060006000B00122Q000700083Q00122Q0008000C3Q00122Q000900083Q00122Q000A000C6Q0006000A000200102Q00050009000600102Q0005000D00024Q00030005000200122Q0004000E3Q00202Q00050001000F00062Q0005003500013Q000493012Q003500012Q00E3000500023Q00202Q01060001000F00122Q000700106Q00085Q00202Q00090001001100062Q0009002400010001000493012Q002400012Q00E3000900033Q0020990009000900122Q00020105000900020012BF000600143Q0020DF00060006000B00122Q0007000C3Q00122Q000800156Q00060008000200102Q00050013000600122Q0006000A3Q00202Q00060006000B00122Q0007000C3Q00122Q0008000E3Q00122Q000900153Q00122Q000A000C6Q0006000A000200102Q0005001600060010630105000D0003001283000400174Q0027000500053Q0012830006000C3Q00209900070001001800065B0007005500013Q000493012Q005500012Q00E3000700023Q00202Q01080001001800122Q000900106Q000A5Q00202Q000B0001001900062Q000B004300010001000493012Q004300012Q00E3000B00033Q002099000B000B00122Q00020107000B00022Q0040000500073Q00122Q000700143Q00202Q00070007000B00122Q000800083Q00122Q000900156Q00070009000200102Q00050013000700122Q0007000A3Q00202Q00070007000B00122Q000800083Q00122Q0009001A3Q00122Q000A00153Q00122Q000B000C6Q0007000B000200102Q00050016000700102Q0005000D000300122Q000600174Q00E3000700013Q0012950108001B6Q00093Q000B00302Q00090007000800202Q000A0001001C00062Q000A005D00010001000493012Q005D0001001283000A001D3Q00106301090003000A2Q00B0000A00043Q00102Q0009001E000A4Q000A00033Q00202Q000A000A000300102Q0009001F000A00302Q00090020002100122Q000A00233Q00202Q000A000A002200202Q000A000A002400102Q00090022000A00122Q000A00233Q00202Q000A000A002500202Q000A000A002600102Q00090025000A00122Q000A00143Q00202Q000A000A000B00122Q000B000C3Q00122Q000C00156Q000A000C000200102Q00090013000A00122Q000A000A3Q00202Q000A000A000B00122Q000B000C6Q000C00043Q00122Q000D00153Q00122Q000E000C6Q000A000E000200102Q00090016000A00122Q000A000A3Q00202Q000A000A000B00122Q000B00086Q000C000400064Q000C000C3Q00122Q000D00083Q00122Q000E000C6Q000A000E000200102Q00090009000A00102Q0009000D00034Q00070009000200202Q00080003002700202Q00080008002800067E000A3Q000100042Q00E33Q00054Q00C23Q00024Q00E33Q00064Q00E33Q00034Q000F0108000A000100209900080003002900205C00080008002800067E000A0001000100042Q00E33Q00054Q00C23Q00024Q00E33Q00064Q00E33Q00034Q000F0108000A000100209900080003002A00205C00080008002800067E000A0002000100052Q00E33Q00054Q00C23Q00024Q00E33Q00064Q00E33Q00034Q00C23Q00014Q000F0108000A00012Q003201083Q00020010630108002B000200067E00090003000100012Q00C23Q00053Q0010630108002C00092Q0010010800024Q005F012Q00013Q00043Q00023Q0003103Q004261636B67726F756E64436F6C6F7233030C3Q00456C656D656E74486F76657200094Q00169Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q00169Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00073Q0003103Q004261636B67726F756E64436F6C6F723303063Q00412Q63656E7403043Q007461736B03053Q0064656C6179029A5Q99B93F03083Q0043612Q6C6261636B03053Q00737061776E001B4Q00E38Q00A0000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300010012BF3Q00033Q0020995Q0004001283000100053Q00067E00023Q000100042Q00E38Q00E33Q00014Q00E33Q00024Q00E33Q00034Q000F012Q000200012Q00E33Q00043Q0020995Q000600065B3Q001A00013Q000493012Q001A00010012BF3Q00033Q0020995Q00072Q00E3000100043Q0020990001000100062Q003E012Q000200012Q005F012Q00013Q00013Q00023Q0003103Q004261636B67726F756E64436F6C6F723303073Q00456C656D656E7400094Q00169Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300016Q00017Q00013Q0003043Q005465787402064Q00E300025Q00065B0002000500013Q000493012Q000500012Q00E300025Q0010630102000100012Q005F012Q00017Q00353Q0003043Q0054657874025Q00805240026Q003A4003153Q0046696E6446697273744368696C644F66436C612Q7303093Q00554950612Q64696E67030D3Q0050612Q64696E67426F2Q746F6D03043Q005544696D2Q033Q006E6577028Q00026Q004640030A3Q005465787442752Q746F6E034Q00030F3Q004175746F42752Q746F6E436F6C6F72010003103Q004261636B67726F756E64436F6C6F723303093Q005365636F6E6461727903163Q004261636B67726F756E645472616E73706172656E6379029A5Q99A93F030B3Q00416E63686F72506F696E7403073Q00566563746F7232026Q00F03F03083Q00506F736974696F6E03053Q005544696D32026Q0028C0026Q0024C003043Q0053697A65030A3Q0066726F6D4F2Q6673657403063Q005A496E646578026Q00244003063Q00506172656E74026Q002A4003063Q005374726F6B65026Q33E33F03043Q0049636F6E03063Q00636972636C6503073Q0053756254657874026Q002640026Q00E03F026Q0020C003093Q00546578744C6162656C03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674026Q003EC0030A3Q004D6F757365456E74657203073Q00436F2Q6E656374030A3Q004D6F7573654C6561766503093Q0041637469766174656403083Q00496E7374616E636502B43Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100023Q00209900020001000100065B0002000A00013Q000493012Q000A0001001283000300023Q00062E0003000B00010001000493012Q000B0001001283000300034Q00E300045Q00205C000400040004001283000600054Q000201040006000200065B0004001700013Q000493012Q001700010012BF000500073Q00206501050005000800122Q000600093Q00122Q0007000A6Q00050007000200102Q0004000600052Q00E3000500013Q0012290106000B6Q00073Q000900302Q00070001000C00302Q0007000D000E4Q000800023Q00202Q00080008001000102Q0007000F000800302Q00070011001200122Q000800143Q00202Q00080008000800122Q000900153Q00122Q000A00156Q0008000A000200102Q00070013000800122Q000800173Q00202Q00080008000800122Q000900153Q00122Q000A00183Q00122Q000B00153Q00122Q000C00196Q0008000C000200102Q00070016000800122Q000800173Q00202Q00080008001B4Q000900033Q00122Q000A00036Q0008000A000200102Q0007001A000800302Q0007001C001D4Q000800033Q00102Q0007001E00084Q0005000700024Q000600046Q000700053Q00122Q0008001F6Q0006000800014Q000600056Q000700056Q000800023Q00202Q00080008002000122Q000900216Q0006000900024Q000700063Q00202Q00080001002200062Q0008004700010001000493012Q00470001001283000800233Q0012830009001F4Q003B000A8Q000B00023Q00202Q000B000B00244Q0007000B000200302Q0007001C002500102Q0007001E00054Q000800083Q00062Q0002008900013Q000493012Q008900010012BF000900143Q0020DF00090009000800122Q000A00153Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00153Q00122Q000B00273Q00122Q000C00263Q00122Q000D00096Q0009000D000200102Q0007001600092Q00E3000900013Q001283000A00284Q0032010B3Q000B003033010B00110015001063010B000100022Q00EA000C00073Q00102Q000B0029000C4Q000C00023Q00202Q000C000C002400102Q000B002A000C00302Q000B002B002C00122Q000C002E3Q00202Q000C000C002D00202Q000C000C002F00102Q000B002D000C0012BF000C00143Q0020DF000C000C000800122Q000D00093Q00122Q000E00266Q000C000E000200102Q000B0013000C00122Q000C00173Q00202Q000C000C000800122Q000D00093Q00122Q000E00253Q00122Q000F00263Q00122Q001000096Q000C0010000200102Q000B0016000C0012BF000C00173Q00202E010C000C000800122Q000D00153Q00122Q000E00303Q00122Q000F00153Q00122Q001000096Q000C00100002001063010B001A000C003033010B001C0025001063010B001E00052Q00020109000B00022Q00C2000800093Q000493012Q009700010012BF000900143Q0020DF00090009000800122Q000A00263Q00122Q000B00266Q0009000B000200102Q00070013000900122Q000900173Q00202Q00090009000800122Q000A00263Q00122Q000B00093Q00122Q000C00263Q00122Q000D00096Q0009000D000200102Q00070016000900209900090005003100205C00090009003200067E000B3Q000100062Q00E33Q00084Q00C23Q00074Q00E33Q00094Q00E33Q00024Q00C23Q00064Q00C23Q00084Q000F0109000B000100209900090005003300205C00090009003200067E000B0001000100062Q00E33Q00084Q00C23Q00074Q00E33Q00094Q00E33Q00024Q00C23Q00064Q00C23Q00084Q000F0109000B000100209900090005003400205C00090009003200067E000B0002000100012Q00C23Q00014Q000F0109000B00012Q003201093Q00010010630109003500052Q0010010900024Q005F012Q00013Q00033Q00063Q00030A3Q0054657874436F6C6F723303063Q00412Q63656E7403053Q00436F6C6F72030C3Q005472616E73706172656E6379029A5Q99C93F03043Q0054657874001D4Q00079Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000200102Q00030003000400302Q0003000400056Q000300016Q00053Q00064Q001C00013Q000493012Q001C00012Q00E38Q00A0000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000600102Q0003000100046Q000300012Q005F012Q00017Q00063Q00030A3Q0054657874436F6C6F723303073Q005375625465787403053Q00436F6C6F7203063Q005374726F6B65030C3Q005472616E73706172656E6379026Q33E33F001D4Q00079Q00000100016Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300019Q004Q000100046Q000200026Q00033Q00024Q000400033Q00202Q00040004000400102Q00030003000400302Q0003000500066Q000300016Q00053Q00064Q001C00013Q000493012Q001C00012Q00E38Q00A0000100056Q000200026Q00033Q00014Q000400033Q00202Q00040004000200102Q0003000100046Q000300012Q005F012Q00017Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q00E37Q0020995Q000100065B3Q000900013Q000493012Q000900010012BF3Q00023Q0020995Q00032Q00E300015Q0020990001000100012Q003E012Q000200012Q005F012Q00017Q002D3Q00026Q003E4003093Q00546578744C6162656C03163Q004261636B67726F756E645472616E73706172656E6379026Q00F03F03043Q005465787403013Q002D03083Q00466F6E7446616365030A3Q0054657874436F6C6F723303083Q005465787453697A65026Q002840030E3Q005465787458416C69676E6D656E7403043Q00456E756D03043Q004C656674030C3Q00546578745472756E6361746503053Q004174456E64030B3Q00416E63686F72506F696E7403073Q00566563746F72322Q033Q006E6577028Q00026Q00E03F03083Q00506F736974696F6E03053Q005544696D32026Q00204003043Q0053697A65025Q004060C003063Q00506172656E74034Q0003063Q00412Q63656E7403053Q005269676874026Q004CC0026Q005040030A3Q005465787442752Q746F6E2Q033Q0042757903103Q004261636B67726F756E64436F6C6F7233030F3Q004175746F42752Q746F6E436F6C6F720100026Q0018C0030A3Q0066726F6D4F2Q66736574026Q004640026Q003440026Q00104003093Q0041637469766174656403073Q00436F2Q6E65637403083Q00496E7374616E63652Q033Q0053657402913Q00062E0001000400010001000493012Q000400012Q003201026Q00C2000100024Q00E300025Q0012DB000300016Q0002000200024Q000300013Q00122Q000400026Q00053Q000B00302Q00050003000400302Q0005000500064Q000600023Q00102Q0005000700064Q000600033Q00209900060006000500107200050008000600302Q00050009000A00122Q0006000C3Q00202Q00060006000B00202Q00060006000D00102Q0005000B000600122Q0006000C3Q00202Q00060006000E00202Q00060006000F00102Q0005000E00060012BF000600113Q0020DF00060006001200122Q000700133Q00122Q000800146Q00060008000200102Q00050010000600122Q000600163Q00202Q00060006001200122Q000700133Q00122Q000800173Q00122Q000900143Q00122Q000A00136Q0006000A000200102Q000500150006001230000600163Q00202Q00060006001200122Q000700043Q00122Q000800193Q00122Q000900043Q00122Q000A00136Q0006000A000200102Q00050018000600102Q0005001A00024Q0003000500022Q00E3000400013Q001283000500024Q003201063Q000A00303301060003000400303301060005001B2Q00EA000700043Q00102Q0006000700074Q000700033Q00202Q00070007001C00102Q00060008000700302Q00060009000A00122Q0007000C3Q00202Q00070007000B00202Q00070007001D00102Q0006000B00070012BF000700113Q0020DF00070007001200122Q000800043Q00122Q000900146Q00070009000200102Q00060010000700122Q000700163Q00202Q00070007001200122Q000800043Q00122Q0009001E3Q00122Q000A00143Q00122Q000B00136Q0007000B000200102Q000600150007001230000700163Q00202Q00070007001200122Q000800133Q00122Q0009001F3Q00122Q000A00043Q00122Q000B00136Q0007000B000200102Q00060018000700102Q0006001A00024Q0004000600022Q00E3000500013Q00129E000600206Q00073Q000A00302Q0007000500214Q000800043Q00102Q0007000700084Q000800033Q00202Q00080008000500102Q00070008000800302Q00070009000A4Q000800033Q00209900080008001C0010630107002200080030330107002300240012BF000800113Q0020DF00080008001200122Q000900043Q00122Q000A00146Q0008000A000200102Q00070010000800122Q000800163Q00202Q00080008001200122Q000900043Q00122Q000A00253Q00122Q000B00143Q00122Q000C00136Q0008000C000200102Q0007001500080012BF000800163Q00206501080008002600122Q000900273Q00122Q000A00286Q0008000A000200102Q0007001800080010630107001A00022Q00020105000700022Q00E3000600054Q00C2000700053Q001283000800294Q000F01060008000100209900060005002A00205C00060006002B00067E00083Q000100012Q00C23Q00014Q000F0106000800012Q003201063Q00020010630106002C000200067E00070001000100052Q00C23Q00034Q00C23Q00044Q00E33Q00064Q00C23Q00054Q00E33Q00033Q0010630106002D00072Q0010010600024Q005F012Q00013Q00023Q00033Q0003083Q0043612Q6C6261636B03043Q007461736B03053Q00737061776E000A4Q00E37Q0020995Q000100065B3Q000900013Q000493012Q000900010012BF3Q00023Q0020995Q00032Q00E300015Q0020990001000100012Q003E012Q000200012Q005F012Q00017Q00113Q0003043Q0054657874034Q002Q033Q00737562027Q00C003023Q002D5303103Q004261636B67726F756E64436F6C6F72332Q033Q004F2Q66030A3Q0054657874436F6C6F723303043Q00536F6C6403053Q005368696E7903063Q00436F6C6F723303073Q0066726F6D524742026Q004440026Q003E40028Q002Q033Q0042757903063Q00412Q63656E74043A4Q00E300045Q0010630104000100012Q00E3000400013Q00067F0105000600010002000493012Q00060001001283000500023Q0010630104000100052Q0080010400026Q000500016Q00040002000200202Q00040004000300122Q000600046Q00040006000200262Q0004001000010005000493012Q001000012Q00CF00046Q0006010400013Q00062E0003001E00010001000493012Q001E00012Q00E3000500034Q00EF000600043Q00202Q00060006000700102Q0005000600064Q000500036Q000600043Q00202Q00060006000100102Q0005000800064Q000500033Q00302Q00050001000900044Q0039000100065B0004002F00013Q000493012Q002F00012Q00E3000500034Q00E3000600043Q00209900060006000A0010630105000600062Q00E3000500033Q0012BF0006000B3Q00209900060006000C0012830007000D3Q0012830008000E3Q0012830009000F4Q00020106000900020010630105000800062Q00E3000500033Q003033010500010010000493012Q003900012Q00E3000500034Q00E3000600043Q0020990006000600110010630105000600062Q00E3000500034Q00E3000600043Q0020990006000600010010630105000800062Q00E3000500033Q0030330105000100102Q005F012Q00017Q00F73Q00030C3Q0043726561746557696E646F7703053Q005469746C65030A3Q0065796564726F2Q70657203093Q0043726561746554616203043Q004E616D65030D3Q005468616E6B732E204576656E7403043Q0049636F6E03113Q0053656C656374656449636F6E436F6C6F7203053Q0042726F776E030C3Q00437265617465546F2Q676C65030E3Q0057616C6B20746F205475726B6579030E3Q00706572736F6E2D77616C6B696E6703073Q0044656661756C74010003043Q00466C6167030C3Q0057616C6B546F5475726B657903083Q0043612Q6C6261636B030F3Q0053657457616C6B546F5475726B6579030C3Q00437265617465536C6964657203113Q005475726B65792077616C6B2073702Q65642Q033Q004D696E026Q0034402Q033Q004D6178025Q00408E4003093Q00496E6372656D656E74026Q00F03F03123Q005365745475726B657957616C6B53702Q656403113Q004175746F204461696C792052657761726403053Q00636C6F636B030F3Q004175746F4461696C7952657761726403123Q005365744175746F4461696C7952657761726403043Q004D61696E03123Q0033642D637562652D612Q726F772D6C65667403093Q004175746F204C69667403063Q00637572736F7203083Q004175746F4C696674030B3Q005365744175746F4C696674030A3Q004175746F2050756E636803113Q0068616E642D68616C662D652Q6C6970736503093Q004175746F50756E6368030C3Q005365744175746F50756E6368030A3Q004175746F2053746F6D702Q033Q006C656703093Q004175746F53746F6D70030C3Q005365744175746F53746F6D7003103Q004175746F2054652Q7269746F72696573030C3Q006C6F636174696F6E2D70696E030F3Q004175746F54652Q7269746F7269657303123Q005365744175746F54652Q7269746F72696573030C3Q00436F2Q6C65637461626C6573030D3Q0062752Q6C65742D666C79696E67030F3Q0057616C6B20746F2041697264726F70030E3Q0070617065722D616972706C616E65030D3Q0057616C6B546F41697264726F7003103Q0053657457616C6B546F41697264726F7003103Q0057616C6B20746F207461726765742Q20030B3Q004175746F47656D57616C6B030E3Q005365744175746F47656D57616C6B030E3Q0047656D2077616C6B2073702Q6564030F3Q0053657447656D57616C6B53702Q6564030F3Q004175746F2047656D732054772Q656E030F3Q0062752Q746572666C792D77696E6773030C3Q004175746F47656D54772Q656E030F3Q005365744175746F47656D54772Q656E03103Q0047656D732074772Q656E2073702Q6564026Q004940025Q00208C4003103Q0053657447656D54772Q656E53702Q656403043Q00426F2Q7303063Q0073776F72647303103Q004272696E6720412Q6C20626F2Q73657303163Q0074776F2D612Q726F77732D646F776E2D616E642D757003093Q00426F2Q734272696E67030C3Q00536574426F2Q734272696E67030C3Q0057616C6B20746F20426F2Q73030A3Q0057616C6B546F426F2Q73030D3Q0053657457616C6B546F426F2Q73030A3Q00547020746F20626F2Q73030C3Q005470546F426F2Q734B692Q6C030F3Q005365745470546F426F2Q734B692Q6C030D3Q0043726561746553656374696F6E030C3Q00426F2Q7320747261636B657203053Q0073776F726403093Q0049636F6E436F6C6F7203063Q00507572706C6503043Q007461736B03053Q00737061776E03043Q00452Q67732Q033Q00652Q6703173Q004175746F2068617463682073656C656374656420652Q6703143Q004175746F486174636853656C6563746564452Q6703173Q005365744175746F486174636853656C6563746564452Q6703103Q004175746F206861746368207065722031030F3Q004175746F48617463685065724F6E6503123Q005365744175746F48617463685065724F6E6503103Q0053656C65637420452Q672028312D3529026Q00144003133Q0053657453656C6563746564452Q67496E646578030A3Q004375626520776F726C64030D3Q00637562652D7665727465786573030E3Q004175746F20486174636820452Q6703103Q004175746F486174636843756265452Q6703133Q005365744175746F486174636843756265452Q6703043Q0053686F70030D3Q0073686F2Q70696E672D6361727403093Q004175746F2053652Q6C03123Q00636972636C652D646F2Q6C61722D7369676E03083Q004175746F53652Q6C030B3Q005365744175746F53652Q6C03103Q004175746F20427579205765696768747303153Q0073686F2Q70696E672D6261736B65742D636865636B030E3Q004175746F4275795765696768747303113Q005365744175746F42757957656967687473030C3Q004175746F2042757920444E412Q033Q0070696E030A3Q004175746F427579444E41030D3Q005365744175746F427579444E41030F3Q004175746F2042757920426F6469657303103Q00706572736F6E2D63726F756368696E67030D3Q004175746F427579426F6469657303103Q005365744175746F427579426F6469657303063Q004D61726B6574030D3Q006275696C64696E672D62616E6B030C3Q004175746F2042757920412Q6C03193Q004175746F42757953757065726D61726B657457656967687473031C3Q005365744175746F42757953757065726D61726B65745765696768747303133Q004175746F53652Q6C53757065726D61726B657403163Q005365744175746F53652Q6C53757065726D61726B6574031A3Q004175746F53757065726D61726B657454652Q7269746F72696573031D3Q005365744175746F53757065726D61726B657454652Q7269746F7269657303083Q004D65726368616E74030F3Q004D65726368616E74206F2Q6665727303063Q0048692Q64656E2Q0103133Q004D65726368616E742050657420536E69706572030E3Q004D65726368616E74536E6970657203113Q005365744D65726368616E74536E69706572030D3Q0043726561746554657874626F7803093Q004D6178207072696365030B3Q00506C616365686F6C64657203153Q00652E672E20354D2028626C616E6B203D20616E792903133Q005365744D65726368616E744D61785072696365030B3Q004372656174654C6162656C03063Q0054617267657403053Q0056616C756503043Q004E6F6E6503063Q00537461747573034Q00030A3Q005065747320666F756E64030E3Q004F70656E2050657420496E646578025Q00408F40030A3Q0053656172636820706574030B3Q00706574206E616D653Q2E030C3Q0043726561746542752Q746F6E03103Q005265667265736820706574206C69737403183Q00726F746174652D636F756E7465722D636C6F636B7769736503113Q00436C65617220612Q6C207461726765747303013Q007803273Q005069636B20796F75722070657473202874617020746F2073656C6563742F756E73656C6563742903093Q00526967687449636F6E03103Q00612Q726F772D6C617267652D646F776E03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203123Q00437265617465436F726E657242752Q746F6E03113Q00612Q726F772D6C617267652D726967687403043Q005465787403063Q004F2Q6665727303103Q004261636B20746F204D65726368616E7403103Q00612Q726F772D6C617267652D6C65667403063Q0069706169727303053Q00546965723103053Q00546965723203053Q005469657233030C3Q00437265617465427579526F7703043Q004375626503043Q006375626503153Q0055736520617420796F7572206F776E207269736B2103123Q00637562652D7175657374696F6E2D6D61726B030A3Q004175746F20437562657303053Q005374617465030C3Q005365744175746F4375626573030B3Q0054772Q656E2053702Q6564026Q004440030C3Q005365744375626553702Q6564030D3Q0057616C6B20746F204375626573030B3Q0057616C6B546F4375626573030E3Q0053657457616C6B546F437562657303063Q0051756573747303043Q007374617203063Q0059652Q6C6F77030C3Q005072656D69756D204F6E6C7903143Q00612Q726F772D746869636B2D746F2D7269676874030A3Q004175746F20517565737403093Q004175746F5175657374030C3Q005365744175746F517565737403023Q00756903063Q007374617475732Q033Q004F2Q6603073Q0063752Q72656E7403053Q00446F696E6703013Q002D03043Q004D69736303103Q0063726F2Q7368616972732D736C61736803143Q0044697361626C652033442052656E646572696E6703103Q00636F6D70757465722D6D6F6E69746F7203093Q0044697361626C653344030C3Q0053657444697361626C653344030B3Q004175746F2052656A6F696E03133Q00612Q726F772D6375726C2D746F2D7269676874030A3Q004175746F52656A6F696E030D3Q005365744175746F52656A6F696E030E3Q004175746F2052656A6F696E205632030C3Q004175746F52656A6F696E5632030F3Q005365744175746F52656A6F696E563203103Q00437573746F6D2057616C6B53702Q656403063Q00706572736F6E030F3Q0057616C6B53702Q6564546F2Q676C6503123Q0053657457616C6B53702Q6564546F2Q676C65030F3Q0057616C6B53702Q65642056616C7565025Q0070974003113Q0053657457616C6B53702Q656456616C756503103Q00437573746F6D204A756D70506F776572030E3Q00706572736F6E2D66612Q6C696E67030F3Q004A756D70506F776572546F2Q676C6503123Q005365744A756D70506F776572546F2Q676C65030F3Q004A756D70506F7765722056616C7565025Q00407F4003113Q005365744A756D70506F77657256616C7565030D3Q00496E66696E697465204A756D70030C3Q00496E66696E6974654A756D70030F3Q00536574496E66696E6974654A756D7003053Q005374617473030A3Q00636861696E2D6C696E6B2Q033Q00465053031A3Q0063686576726F6E2D6C617267652D636F6E64656E7365642D7570030A3Q004C6967687447722Q656E03013Q0030030C3Q004E6574776F726B2050696E6703103Q00776966692D7369676E616C2D6661697203043Q0030206D73030C3Q00456C61707365642054696D6503083Q002Q303A2Q303A2Q30030A3Q0047656D73202F204D696E03193Q00636C6F636B2D7370696E2D726576657273652D646173686564030B3Q0047656D73204561726E656403073Q006469616D6F6E64030B3Q00526573657420537461747300FB023Q00E37Q00205C5Q00012Q003201023Q00012Q00E3000300013Q0010630102000200032Q0002012Q00020002001283000100033Q00205C00023Q00042Q003201043Q00030030330104000500060010630104000700012Q00E3000500023Q0020990005000500090010630104000800052Q000201020004000200205C00030002000A2Q003201053Q000500303600050005000B00302Q00050007000C00302Q0005000D000E00302Q0005000F00104Q000600033Q00202Q00060006001200102Q0005001100064Q00030005000100202Q0003000200134Q00053Q000600303301050005001400303600050015001600302Q00050017001800302Q0005000D001600302Q00050019001A4Q000600033Q00202Q00060006001B00102Q0005001100064Q00030005000100202Q00030002000A4Q00053Q000500303600050005001C00302Q00050007001D00302Q0005000D000E00302Q0005000F001E4Q000600033Q00202Q00060006001F00102Q0005001100064Q00030005000100202Q00033Q00044Q00053Q00020030330105000500200030330105000700212Q000201030005000200205C00040003000A2Q003201063Q000500303600060005002200302Q00060007002300302Q0006000D000E00302Q0006000F00244Q000700033Q00202Q00070007002500102Q0006001100074Q00040006000100202Q00040003000A4Q00063Q000500303600060005002600302Q00060007002700302Q0006000D000E00302Q0006000F00284Q000700033Q00202Q00070007002900102Q0006001100074Q00040006000100202Q00040003000A4Q00063Q000500303600060005002A00302Q00060007002B00302Q0006000D000E00302Q0006000F002C4Q000700033Q00202Q00070007002D00102Q0006001100074Q00040006000100202Q00040003000A4Q00063Q000500303600060005002E00302Q00060007002F00302Q0006000D000E00302Q0006000F00304Q000700033Q00202Q00070007003100102Q0006001100074Q00040006000100202Q00043Q00044Q00063Q00020030330106000500320030330106000700332Q000201040006000200205C00050004000A2Q003201073Q000500303600070005003400302Q00070007003500302Q0007000D000E00302Q0007000F00364Q000800033Q00202Q00080008003700102Q0007001100084Q00050007000100202Q00050004000A4Q00073Q000500303600070005003800302Q00070007000C00302Q0007000D000E00302Q0007000F00394Q000800033Q00202Q00080008003A00102Q0007001100084Q00050007000100202Q0005000400134Q00073Q000600303301070005003B00303600070015001600302Q00070017001800302Q0007000D001600302Q00070019001A4Q000800033Q00202Q00080008003C00102Q0007001100084Q00050007000100202Q00050004000A4Q00073Q000500303600070005003D00302Q00070007003E00302Q0007000D000E00302Q0007000F003F4Q000800033Q00202Q00080008004000102Q0007001100084Q00050007000100202Q0005000400134Q00073Q000600303301070005004100303600070015004200302Q00070017004300302Q0007000D004200302Q00070019001A4Q000800033Q00202Q00080008004400102Q0007001100084Q00050007000100202Q00053Q00044Q00073Q00020030330107000500450030330107000700462Q000201050007000200205C00060005000A2Q003201083Q000500303600080005004700302Q00080007004800302Q0008000D000E00302Q0008000F00494Q000900033Q00202Q00090009004A00102Q0008001100094Q00060008000100202Q00060005000A4Q00083Q000500303600080005004B00302Q00080007004800302Q0008000D000E00302Q0008000F004C4Q000900033Q00202Q00090009004D00102Q0008001100094Q00060008000100202Q00060005000A4Q00083Q000500303600080005004E00302Q00080007004800302Q0008000D000E00302Q0008000F004F4Q000900033Q00202Q00090009005000102Q0008001100094Q00060008000100202Q0006000500514Q00083Q00030030330108000500520030330108000700532Q00E3000900023Q0020990009000900550010630108005400092Q000F0106000800012Q003201065Q0012BF000700563Q00209900070007005700067E00083Q000100032Q00E33Q00044Q00C23Q00064Q00C23Q00054Q002000070002000100202Q00073Q00044Q00093Q000200302Q00090005005800302Q0009000700594Q00070009000200202Q00080007000A4Q000A3Q000500302Q000A0005005A00302Q000A0007005900302Q000A000D000E00302Q000A000F005B4Q000B00033Q00202Q000B000B005C00102Q000A0011000B4Q0008000A000100202Q00080007000A4Q000A3Q000500302Q000A0005005D00302Q000A0007005900302Q000A000D000E00302Q000A000F005E4Q000B00033Q00202Q000B000B005F00102Q000A0011000B4Q0008000A000100202Q0008000700134Q000A3Q000600302Q000A0005006000302Q000A0015001A00302Q000A0017006100302Q000A000D001A00302Q000A0019001A4Q000B00033Q00202Q000B000B006200102Q000A0011000B4Q0008000A000100202Q0008000700514Q000A3Q000200302Q000A0005006300302Q000A000700644Q0008000A000100202Q00080007000A4Q000A3Q000500302Q000A0005006500302Q000A0007005900302Q000A000D000E00302Q000A000F00664Q000B00033Q00202Q000B000B006700102Q000A0011000B4Q0008000A000100202Q00083Q00044Q000A3Q000200302Q000A0005006800302Q000A000700694Q0008000A000200202Q00090008000A4Q000B3Q000500302Q000B0005006A00302Q000B0007006B00302Q000B000D000E00302Q000B000F006C4Q000C00033Q00202Q000C000C006D00102Q000B0011000C4Q0009000B000100202Q00090008000A4Q000B3Q000500302Q000B0005006E00302Q000B0007006F00302Q000B000D000E00302Q000B000F00704Q000C00033Q00202Q000C000C007100102Q000B0011000C4Q0009000B000100202Q00090008000A4Q000B3Q000500302Q000B000500720030E5000B0007007300302Q000B000D000E00302Q000B000F00744Q000C00033Q00202Q000C000C007500102Q000B0011000C4Q0009000B000100202Q00090008000A4Q000B3Q000500302Q000B000500760030E5000B0007007700302Q000B000D000E00302Q000B000F00784Q000C00033Q00202Q000C000C007900102Q000B0011000C4Q0009000B000100202Q00093Q00044Q000B3Q000200302Q000B0005007A003033010B0007007B2Q00020109000B000200205C000A0009000A2Q0032010C3Q0005003036000C0005007C00302Q000C0007006F00302Q000C000D000E00302Q000C000F007D4Q000D00033Q00202Q000D000D007E00102Q000C0011000D4Q000A000C000100202Q000A0009000A4Q000C3Q0005003036000C0005006A00302Q000C0007006B00302Q000C000D000E00302Q000C000F007F4Q000D00033Q00202Q000D000D008000102Q000C0011000D4Q000A000C000100202Q000A0009000A4Q000C3Q0005003036000C0005002E00302Q000C0007002F00302Q000C000D000E00302Q000C000F00814Q000D00033Q00202Q000D000D008200102Q000C0011000D4Q000A000C000100202Q000A3Q00044Q000C3Q0002003033010C00050083003003010C0007006F4Q000A000C000200202Q000B3Q00044Q000D3Q000200302Q000D0005008400302Q000D008500864Q000B000D000200202Q000C000A000A4Q000E3Q000500302Q000E000500870030E5000E0007006F00302Q000E000D000E00302Q000E000F00884Q000F00033Q00202Q000F000F008900102Q000E0011000F4Q000C000E000100202Q000C000A008A4Q000E3Q000300302Q000E0005008B003033010E008C008D2Q00E3000F00033Q00205A000F000F008E00102Q000E0011000F4Q000C000E000100202Q000C000A008F4Q000E3Q000200302Q000E0005009000302Q000E009100924Q000C000E000200202Q000D000A008F4Q000F3Q0002003033010F00050093003033010F009100942Q0002010D000F000200205C000E000A008F2Q003201103Q00030030330110000500950030330110000700590030330110009100962Q0002010E001000020012BF000F00563Q002099000F000F005700067E00100001000100032Q00E33Q00044Q00E33Q00054Q00C23Q000E4Q003E010F000200012Q0032010F6Q000601105Q001283001100974Q0027001200133Q00067E00140002000100052Q00C23Q000C4Q00E33Q00054Q00C23Q000F4Q00E33Q00024Q00E33Q00063Q00067E00150003000100032Q00C23Q00124Q00C23Q000F4Q00C23Q00103Q00067E00160004000100032Q00C23Q00104Q00C23Q00134Q00C23Q00153Q00205C0017000A008A2Q003201193Q00030030330119000500980030330119008C009900067E001A0005000100022Q00C23Q00164Q00C23Q00153Q00106301190011001A2Q00020117001900022Q00C2001200173Q00067E00170006000100082Q00C23Q000F4Q00E33Q00054Q00C23Q000D4Q00C23Q000A4Q00E33Q00034Q00C23Q00144Q00C23Q00114Q00C23Q00153Q00205C0018000A009A2Q0032011A3Q0003003033011A0005009B003033011A0007009C001088011A001100174Q0018001A000100202Q0018000A009A4Q001A3Q000300302Q001A0005009D00302Q001A0007009E00067E001B0007000100042Q00E33Q00034Q00C23Q000D4Q00E33Q00054Q00C23Q00143Q001088011A0011001B4Q0018001A000100202Q0018000A009A4Q001A3Q000500302Q001A0005009F00302Q001A000700592Q00E3001B00023Q002099001B001B0055001063011A0054001B003033011A00A000A100067E001B0008000100022Q00C23Q00164Q00C23Q00103Q001003001A0011001B4Q0018001A00024Q001300183Q00202Q0018001300A200202Q00190011001A00102Q001800A300194Q001800166Q00198Q0018000200014Q001800176Q00180001000100202Q0018000A00A44Q001A3Q000300302Q001A000700A500302Q001A00A600A700067E001B0009000100012Q00C23Q000B3Q001088011A0011001B4Q0018001A000100202Q0018000B009A4Q001A3Q000300302Q001A000500A800302Q001A000700A900067E001B000A000100012Q00C23Q000A3Q00107B011A0011001B4Q0018001A000100202Q0018000B008F4Q001A3Q000200302Q001A0005009300302Q001A009100944Q0018001A000200202Q0019000B00514Q001B3Q000300302Q001B0005008400302Q001B0007006F4Q001C00023Q00202Q001C001C005500102Q001B0054001C4Q0019001B00014Q00198Q001A5Q00122Q001B00AA6Q001C00033Q00122Q001D00AB3Q00122Q001E00AC3Q00122Q001F00AD6Q001C000300012Q0052001B0002001D000493012Q00FB2Q0100205C0020000B00AE2Q003201223Q000100067E0023000B000100042Q00C23Q001A4Q00C23Q001F4Q00E33Q00054Q00C23Q00183Q0010630122001100232Q00020120002200022Q002D0019001F00202Q00F7001E5Q000619011B00F02Q010002000493012Q00F02Q010012BF001B00563Q002099001B001B005700067E001C000C000100042Q00E33Q00044Q00E33Q00054Q00C23Q00194Q00C23Q001A4Q0023001B0002000100202Q001B3Q00044Q001D3Q000200302Q001D000500AF00302Q001D000700B04Q001B001D000200202Q001C001B00514Q001E3Q000300302Q001E000500B100302Q001E000700B24Q001F00023Q00202Q001F001F005500102Q001E0054001F4Q001C001E000100202Q001C001B000A4Q001E3Q000500302Q001E000500B300302Q001E0007002100302Q001E000D000E00067E001F000D000100012Q00E33Q00073Q00108D001E00B4001F4Q001F00033Q00202Q001F001F00B500102Q001E0011001F4Q001C001E000100202Q001C001B00134Q001E3Q000600302Q001E000500B600302Q001E0015001600302Q001E001700B70030ED001E000D00B700302Q001E0019001A4Q001F00033Q00202Q001F001F00B800102Q001E0011001F4Q001C001E000100202Q001C001B000A4Q001E3Q000500302Q001E000500B900302Q001E0007000C0030ED001E000D000E00302Q001E000F00BA4Q001F00033Q00202Q001F001F00BB00102Q001E0011001F4Q001C001E000100202Q001C3Q00044Q001E3Q000300302Q001E000500BC00302Q001E000700BD2Q00E3001F00023Q002099001F001F00BE001063011E0008001F2Q0026011C001E000200202Q001D001C00514Q001F3Q000300302Q001F000500BF00302Q001F000700C04Q002000023Q00202Q0020002000BE00102Q001F005400204Q001D001F000100202Q001D001C000A2Q0032011F3Q0006003033011F000500C1003033011F000700BD2Q00E3002000023Q0020990020002000BE001063011F00540020003033011F000D000E003033011F000F00C22Q00E3002000033Q0020990020002000C3001063011F001100202Q000F011D001F00012Q006F001D00083Q00202Q001D001D00C400202Q001E001C008F4Q00203Q000200302Q00200005009300302Q0020009100C64Q001E0020000200102Q001D00C5001E4Q001D00083Q00202Q001D001D00C400205C001E001C008F2Q003201203Q00020030330120000500C80030330120009100C92Q0002011E00200002001063011D00C7001E00205C001D3Q00042Q0032011F3Q0002003033011F000500CA003033011F000700CB2Q0002011D001F000200205C001E001D000A2Q003201203Q00050030330120000500CC0030330120000700CD0030330120000D000E00303C0120000F00CE4Q002100033Q00202Q0021002100CF00102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500D000302Q0020000700D100302Q0020000D000E00302Q0020000F00D24Q002100033Q00202Q0021002100D300102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500D400302Q0020000700D100302Q0020000D000E00302Q0020000F00D54Q002100033Q00202Q0021002100D600102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500D700302Q0020000700D800302Q0020000D000E00302Q0020000F00D94Q002100033Q00202Q0021002100DA00102Q0020001100214Q001E0020000100202Q001E001D00134Q00203Q000600302Q0020000500DB00302Q00200015001600302Q0020001700DC00302Q0020000D001600302Q00200019001A4Q002100033Q00202Q0021002100DD00102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000500302Q0020000500DE00302Q0020000700DF00302Q0020000D000E00302Q0020000F00E04Q002100033Q00202Q0021002100E100102Q0020001100214Q001E0020000100202Q001E001D00134Q00203Q000600302Q0020000500E200302Q00200015004200302Q0020001700E300302Q0020000D004200302Q00200019001A4Q002100033Q00202Q0021002100E400102Q0020001100214Q001E0020000100202Q001E001D000A4Q00203Q000400302Q0020000500E500302Q0020000D000E00302Q0020000F00E64Q002100033Q00202Q0021002100E700102Q0020001100214Q001E0020000100202Q001E3Q00044Q00203Q000200302Q0020000500E800302Q0020000700E92Q00B4001E0020000200202Q001F001E008F4Q00213Q000400302Q0021000500EA00302Q0021000700EB4Q002200023Q00202Q0022002200EC00102Q00210054002200302Q0021009100ED4Q001F0021000200205C0020001E008F2Q008701223Q000300302Q0022000500EE00302Q0022000700EF00302Q0022009100F04Q00200022000200202Q0021001E008F4Q00233Q000300302Q0023000500F100302Q00230007001D00302Q0023009100F22Q000201210023000200205C0022001E008F2Q008701243Q000300302Q0024000500F300302Q0024000700F400302Q0024009100ED4Q00220024000200202Q0023001E008F4Q00253Q000300302Q0025000500F500302Q0025000700F600302Q0025009100ED2Q000201230025000200205C0024001E009A2Q003201263Q00030030330126000500F700303301260007009C00067E0027000E000100062Q00E33Q00094Q00E33Q000A4Q00E33Q000B4Q00C23Q00214Q00C23Q00234Q00C23Q00223Q0010630126001100272Q000F0124002600010012BF002400563Q00209900240024005700067E0025000F0001000D2Q00E33Q00044Q00C23Q001F4Q00E33Q000C4Q00E33Q000D4Q00C23Q00204Q00E33Q00094Q00C23Q00214Q00E33Q000E4Q00E33Q000B4Q00E33Q000A4Q00C23Q00234Q00E33Q000F4Q00C23Q00224Q003E0124000200012Q005F012Q00013Q00103Q00183Q0003053Q00416C69766503043Q007461736B03043Q0077616974029A5Q99B93F03093Q00776F726B7370616365030E3Q0046696E6446697273744368696C64030A3Q00426F2Q734D6F64656C7303063Q00697061697273030B3Q004765744368696C6472656E2Q033Q0049734103053Q004D6F64656C03083Q00486974734C65667403073Q004D61784869747303043Q004E616D652Q0103053Q0056616C7565030B3Q004372656174654C6162656C03083Q00746F737472696E672Q033Q00202F202Q033Q0053657403053Q00706169727303083Q00496E7374616E636503073Q0044657374726F792Q00614Q00E37Q0020995Q000100065B3Q006000013Q000493012Q006000010012BF3Q00023Q0020B25Q000300122Q000100048Q0002000100124Q00053Q00206Q000600122Q000200078Q000200024Q00015Q00064Q004E00013Q000493012Q004E00010012BF000200083Q00205C00033Q00092Q0094010300044Q007201023Q0004000493012Q004C000100205C00070006000A0012830009000B4Q000201070009000200065B0007004C00013Q000493012Q004C000100205C00070006000600125D0109000C6Q00070009000200202Q00080006000600122Q000A000D6Q0008000A000200062Q0007004C00013Q000493012Q004C000100065B0008004C00013Q000493012Q004C000100209900090006000E00208100010009000F002099000900070010002099000A000800102Q00E3000B00013Q002099000C0006000E2Q00C3000B000B000C00062E000B003F00010001000493012Q003F00012Q00E3000B00013Q002010000C0006000E4Q000D00023Q00202Q000D000D00114Q000F3Q000200202Q00100006000E00102Q000F000E001000122Q001000126Q001100096Q00100002000200122Q001100133Q00122Q001200126Q0013000A6Q0012000200024Q00100010001200102Q000F001000104Q000D000F00024Q000B000C000D00044Q004C00012Q00E3000B00013Q002085000C0006000E4Q000B000B000C00202Q000B000B001400122Q000D00126Q000E00096Q000D0002000200122Q000E00133Q00122Q000F00126Q0010000A6Q000F000200022Q0063000D000D000F2Q000F010B000D00010006190102001400010002000493012Q001400010012BF000200154Q00E3000300014Q0052000200020004000493012Q005D00012Q00C300070001000500062E0007005D00010001000493012Q005D000100209900070006001600065B0007005B00013Q000493012Q005B000100209900070006001600205C0007000700172Q003E0107000200012Q00E3000700013Q0020810007000500180006190102005200010002000493012Q00520001000493014Q00012Q005F012Q00017Q00083Q0003053Q00416C697665030D3Q00676574496E646578436F756E742Q033Q005365742Q033Q00202F2003143Q004F70656E2050657420496E64657820666972737403043Q007461736B03043Q0077616974027Q004000194Q00E37Q0020995Q000100065B3Q001800013Q000493012Q001800012Q00E33Q00013Q0020995Q00022Q00B93Q000100012Q00E3000200023Q00205C00020002000300065B3Q001100013Q000493012Q001100012Q00C200045Q001283000500044Q00C2000600014Q006300040004000600062E0004001200010001000493012Q00120001001283000400054Q000F010200040001001292010200063Q00202Q00020002000700122Q000300086Q00020002000100046Q00012Q005F012Q00017Q00123Q002Q033Q00536574030A3Q007461726765745465787403063Q00697061697273030A3Q006973546172676574656403043Q006E616D652Q033Q00726F7703153Q0046696E6446697273744368696C644F66436C612Q7303083Q0055495374726F6B6503053Q00436F6C6F7203063Q00412Q63656E7403063Q005374726F6B65030C3Q005472616E73706172656E6379029A5Q99B93F03163Q0046696E6446697273744368696C64576869636849734103093Q00546578744C6162656C030A3Q0054657874436F6C6F7233030A3Q004C6967687447722Q656E03043Q005465787400374Q00E37Q00205C5Q00012Q00E3000200013Q0020990002000200022Q0066010200014Q000E014Q00010012BF3Q00034Q00E3000100024Q00523Q00020002000493012Q003400012Q00E3000500013Q0020990005000500040020990006000400052Q00B70005000200020020B600060004000600202Q00060006000700122Q000800086Q00060008000200062Q0006002400013Q000493012Q0024000100065B0005001A00013Q000493012Q001A00012Q00E3000700033Q00209900070007000A00062E0007001C00010001000493012Q001C00012Q00E3000700033Q00209900070007000B00106301060009000700065B0005002200013Q000493012Q002200010012830007000D3Q00062E0007002300010001000493012Q002300012Q00E3000700043Q0010630106000C00070020990007000400060020D100070007000E00122Q0009000F6Q000A00016Q0007000A000200062Q0007003400013Q000493012Q0034000100065B0005003100013Q000493012Q003100012Q00E3000800033Q00209900080008001100062E0008003300010001000493012Q003300012Q00E3000800033Q002099000800080012001063010700100008000619012Q000A00010002000493012Q000A00012Q005F012Q00017Q000B3Q0003063Q00737472696E6703053Q006C6F7765722Q033Q00476574034Q0003063Q006970616972732Q033Q00726F7703073Q0056697369626C6503043Q0066696E6403043Q006E616D65026Q00F03F2Q00283Q0012BF3Q00013Q0020995Q00022Q00E300015Q00065B0001000A00013Q000493012Q000A00012Q00E300015Q0020990001000100032Q00D400010001000200062E0001000B00010001000493012Q000B0001001283000100044Q00B73Q000200020012BF000100054Q00E3000200014Q0052000100020003000493012Q002500010020990006000500062Q00E3000700023Q00065B0007002400013Q000493012Q002400010026CD3Q002300010004000493012Q002300010012BF000700013Q00201E00070007000800122Q000800013Q00202Q00080008000200202Q0009000500094Q0008000200024Q00095Q00122Q000A000A6Q000B00016Q0007000B000200262Q000700230001000B000493012Q002300012Q00CF00076Q0006010700013Q0010630106000700070006192Q01001000010002000493012Q001000012Q005F012Q00017Q00033Q00030C3Q00536574526967687449636F6E030E3Q00612Q726F772D6C617267652D757003103Q00612Q726F772D6C617267652D646F776E01104Q0084017Q00E3000100013Q00065B0001000D00013Q000493012Q000D00012Q00E3000100013Q00205C00010001000100065B3Q000B00013Q000493012Q000B0001001283000300023Q00062E0003000C00010001000493012Q000C0001001283000300034Q000F2Q01000300012Q00E3000100024Q000A0001000100012Q005F012Q00017Q00013Q00034Q0001093Q0026CD3Q000600010001000493012Q000600012Q00E300016Q0006010200014Q003E2Q0100020001000493012Q000800012Q00E3000100014Q000A0001000100012Q005F012Q00017Q00133Q0003063Q006970616972732Q033Q00726F7703073Q0044657374726F79030B3Q006765745065744E616D65732Q033Q0053657403153Q004F70656E2050657420496E6465782066697273742103073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564030C3Q0043726561746542752Q746F6E03043Q004E616D6503083Q0043612Q6C6261636B03083Q00496E7374616E6365030B3Q004C61796F75744F7264657203053Q007461626C6503063Q00696E7365727403043Q006E616D6500483Q0012BF3Q00014Q00E300016Q00523Q00020002000493012Q000A000100209900050004000200065B0005000A00013Q000493012Q000A000100209900050004000200205C0005000500032Q003E010500020001000619012Q000400010002000493012Q000400012Q0032017Q00189Q003Q00013Q00206Q00046Q0001000200064Q001800010001000493012Q001800012Q00E3000100023Q00205C000100010005001283000300064Q000F2Q01000300012Q005F012Q00014Q00E3000100023Q00203100010001000500122Q000300076Q00030001000200202Q0003000300084Q000300033Q00122Q000400096Q000500013Q00202Q00050005000A00122Q0006000B6Q0003000300062Q000F2Q01000300010012BF000100014Q00C200026Q0052000100020003000493012Q004100012Q00E3000600033Q00205C00060006000C2Q003201083Q00020010630108000D000500067E00093Q000100052Q00E33Q00044Q00C23Q00054Q00E33Q00024Q00E33Q00014Q00E33Q00053Q0010630108000E00092Q000201060008000200209900070006000F2Q00E3000800064Q00DA0008000800040010630107001000080012BF000700113Q0020990007000700122Q00E300086Q003201093Q0002001063010900130005002099000A0006000F00106301090002000A2Q000F0107000900012Q00F700045Q0006192Q01002800010002000493012Q002800012Q00E3000100074Q000A0001000100012Q00E3000100054Q000A0001000100012Q005F012Q00013Q00013Q00073Q0003143Q00546F2Q676C654D65726368616E745461726765742Q033Q0053657403073Q0067657467656E7603123Q0053656C65637465645461726765745065747303013Q002F2Q033Q006D617803093Q00207461726765746564001A4Q0034016Q00206Q00014Q000100018Q0002000100064Q001300013Q000493012Q001300012Q00E3000200023Q00203100020002000200122Q000400036Q00040001000200202Q0004000400044Q000400043Q00122Q000500056Q000600033Q00202Q00060006000600122Q000700076Q0004000400072Q000F010200040001000493012Q001700012Q00E3000200023Q00205C0002000200022Q00C2000400014Q000F0102000400012Q00E3000200044Q000A0002000100012Q005F012Q00017Q00053Q0003143Q00436C6561724D65726368616E74546172676574732Q033Q0053657403023Q00302F2Q033Q006D617803093Q00207461726765746564000E4Q008A016Q00206Q00016Q000100016Q00013Q00206Q000200122Q000200036Q000300023Q00202Q00030003000400122Q000400056Q0002000200042Q000F012Q000200012Q00E33Q00034Q000A3Q000100012Q005F012Q00019Q003Q00054Q007F9Q00000100016Q000100018Q000200016Q00017Q00013Q0003073Q005F73656C65637400044Q00E37Q0020995Q00012Q000A3Q000100012Q005F012Q00017Q00013Q0003073Q005F73656C65637400044Q00E37Q0020995Q00012Q000A3Q000100012Q005F012Q00017Q00093Q0003093Q0067657452656D6F746503093Q00617661696C61626C652Q033Q00536574030D3Q004E6F7420617661696C61626C6503103Q0052656D6F7465206E6F7420666F756E6403053Q007063612Q6C03073Q00426F756768742003043Q006E616D65030A3Q00427579206661696C656400284Q00E38Q00E3000100014Q00C35Q00012Q00E3000100023Q0020990001000100012Q00D400010001000200065B3Q000B00013Q000493012Q000B000100209900023Q000200062E0002001000010001000493012Q001000012Q00E3000200033Q00205C000200020003001283000400044Q000F010200040001000493012Q0027000100062E0001001700010001000493012Q001700012Q00E3000200033Q00205C000200020003001283000400054Q000F010200040001000493012Q002700010012BF000200063Q00067E00033Q000100022Q00C23Q00014Q00E33Q00014Q00B70002000200022Q00E3000300033Q00205C00030003000300065B0002002500013Q000493012Q00250001001283000500073Q00209900063Q00082Q006300050005000600062E0005002600010001000493012Q00260001001283000500094Q000F0103000500012Q005F012Q00013Q00013Q00013Q00030C3Q00496E766F6B6553657276657200054Q00CB7Q00206Q00014Q000200018Q000200016Q00017Q000C3Q0003053Q00416C69766503093Q006765744F2Q6665727303053Q0070616972732Q033Q0053657403043Q006E616D6503053Q00707269636503093Q00617661696C61626C6503203Q004F70656E20746865204D65726368616E7420746F206C6F6164206F2Q66657273034Q0003043Q007461736B03043Q0077616974026Q00E03F00254Q00E37Q0020995Q000100065B3Q002400013Q000493012Q002400012Q00E33Q00013Q0020F55Q00026Q0001000200122Q000100036Q000200026Q00010002000300044Q001D00010006510106000E00013Q000493012Q000E00012Q00C300063Q00042Q00E3000700034Q002D00070004000600065B0006001800013Q000493012Q0018000100205C000700050004002099000900060005002099000A00060006002099000B000600072Q000F0107000B0001000493012Q001D000100205C000700050004001283000900083Q001283000A00094Q0006010B6Q000F0107000B00010006192Q01000B00010002000493012Q000B00010012BF0001000A3Q00209900010001000B0012830002000C4Q003E2Q0100020001000493014Q00012Q005F012Q00017Q00013Q0003073Q00456E61626C656400044Q00E37Q0020995Q00012Q0010012Q00024Q005F012Q00017Q00063Q0003023Q006F7303043Q0074696D65028Q002Q033Q0053657403083Q002Q303A2Q303A2Q3003013Q003000153Q001252012Q00013Q00206Q00026Q000100029Q0000124Q00038Q00019Q008Q00028Q00033Q00206Q0004001283000200054Q0036012Q000200016Q00043Q00206Q000400122Q000200068Q000200016Q00053Q00206Q000400122Q000200068Q000200016Q00017Q00153Q0003053Q00416C69766503043Q007461736B03043Q0077616974026Q00F03F2Q033Q0053657403083Q00746F737472696E67028Q0003053Q007063612Q6C2Q033Q00206D7303023Q006F7303043Q0074696D6503043Q006D61746803053Q00666C2Q6F72025Q0020AC40026Q004E4003063Q00737472696E6703063Q00666F726D6174030E3Q00253032643A253032643A2530326403053Q0056616C75650003013Q0030005E4Q00E37Q0020995Q000100065B3Q005D00013Q000493012Q005D00010012BF3Q00023Q0020995Q0003001283000100044Q003E012Q000200012Q00E33Q00013Q00205C5Q00050012BF000200064Q00E3000300024Q0094010200034Q000E014Q00010012833Q00073Q0012BF000100083Q00067E00023Q000100022Q00C28Q00E33Q00034Q00222Q01000200014Q000100043Q00202Q00010001000500122Q000300066Q00048Q00030002000200122Q000400096Q0003000300044Q00010003000100122Q0001000A3Q00209900010001000B2Q002F2Q01000100024Q000200056Q00010001000200122Q0002000C3Q00202Q00020002000D00202Q00030001000E4Q00020002000200122Q0003000C3Q00202Q00030003000D00202Q00040001000E00201601040004000F2Q00B700030002000200207001040001000F2Q00E3000500063Q00205C0005000500050012BF000700103Q002099000700070011001283000800124Q00C2000900024Q00C2000A00034Q00C2000B00044Q004B0107000B4Q000E01053Q00012Q00E3000500074Q00D400050001000200065B0005005B00013Q000493012Q005B00010020990006000500132Q00E3000700083Q0026CD0007004400010014000493012Q004400012Q00E3000700084Q0042000700060007000EA00107004400010007000493012Q004400012Q00E3000800094Q00DA0008000800072Q0084010800094Q0084010600084Q00970107000A3Q00202Q0007000700054Q0009000B6Q000A00096Q0009000A6Q00073Q0001000E2Q0007005700010001000493012Q005700012Q00E3000700094Q000B01070007000100202Q00070007000F4Q0008000C3Q00202Q0008000800054Q000A000B6Q000B00076Q000A000B6Q00083Q000100044Q005B00012Q00E30007000C3Q00205C000700070005001283000900154Q000F0107000900012Q00F77Q000493014Q00012Q005F012Q00013Q00013Q00043Q0003043Q006D61746803053Q00666C2Q6F72030E3Q004765744E6574776F726B50696E67025Q00408F4000093Q00124D012Q00013Q00206Q00024Q000100013Q00202Q0001000100034Q00010002000200202Q0001000100046Q000200029Q006Q00017Q00", GetFEnv(), ...);
