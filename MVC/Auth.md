Alright 🔥 — here’s a **crisp and complete cheat sheet** for **Authentication + Authorization** in **ASP.NET Core MVC (.NET 6)**:

---

# 🔒 Authentication & Authorization Cheat Sheet (ASP.NET Core MVC)

---

# 1. Add Authentication to Your Project

✅ Install Identity (if needed):

```bash
dotnet add package Microsoft.AspNetCore.Identity.EntityFrameworkCore
dotnet add package Microsoft.EntityFrameworkCore.SqlServer
```

✅ In `Program.cs`, configure services:

```csharp
builder.Services.AddDbContext<ApplicationDbContext>(options =>
    options.UseSqlServer(builder.Configuration.GetConnectionString("DefaultConnection")));

builder.Services.AddDefaultIdentity<IdentityUser>(options => 
    options.SignIn.RequireConfirmedAccount = true)
    .AddEntityFrameworkStores<ApplicationDbContext>();

builder.Services.AddControllersWithViews();
```

✅ Add Authentication Middleware:

```csharp
var app = builder.Build();

app.UseAuthentication(); // 👈 Must come **before** UseAuthorization
app.UseAuthorization();
```

---

# 2. Scaffold Identity UI (Optional)

✅ If you want ready-made login/register pages:

```bash
dotnet aspnet-codegenerator identity -dc ApplicationDbContext
```

✅ This gives you:
- `/Identity/Account/Login`
- `/Identity/Account/Register`
- `/Identity/Account/Logout`
- Email confirmation logic, etc.

---

# 3. Protect Pages (Authorization)

✅ Protect a **Controller** or **Action** with `[Authorize]`:

```csharp
[Authorize]
public class ProductsController : Controller
{
    // Only logged-in users can access
}
```

✅ Protect specific **Actions**:

```csharp
public class ProductsController : Controller
{
    [Authorize]
    public IActionResult Create() => View();

    [AllowAnonymous]
    public IActionResult Index() => View();
}
```

✅ [AllowAnonymous] overrides [Authorize] (anyone can see `Index()`).

---

# 4. Login / Logout

✅ Redirect users if they are not logged in:

```html
@if (User.Identity.IsAuthenticated)
{
    <p>Welcome @User.Identity.Name!</p>
    <form asp-area="Identity" asp-page="/Account/Logout" method="post">
        <button type="submit" class="btn btn-link">Logout</button>
    </form>
}
else
{
    <a asp-area="Identity" asp-page="/Account/Login">Login</a> |
    <a asp-area="Identity" asp-page="/Account/Register">Register</a>
}
```

---

# 5. Roles (Admin, User, etc.)

✅ Add Roles Support:

```csharp
builder.Services.AddDefaultIdentity<IdentityUser>(options => options.SignIn.RequireConfirmedAccount = true)
    .AddRoles<IdentityRole>() // 👈 Add this
    .AddEntityFrameworkStores<ApplicationDbContext>();
```

✅ Create Roles (seeder or manual):

```csharp
public static async Task SeedRoles(RoleManager<IdentityRole> roleManager)
{
    if (!await roleManager.RoleExistsAsync("Admin"))
        await roleManager.CreateAsync(new IdentityRole("Admin"));
    
    if (!await roleManager.RoleExistsAsync("User"))
        await roleManager.CreateAsync(new IdentityRole("User"));
}
```

✅ Assign Role to a User:

```csharp
await userManager.AddToRoleAsync(user, "Admin");
```

✅ Authorize by Role:

```csharp
[Authorize(Roles = "Admin")]
public IActionResult AdminOnly()
{
    return View();
}
```

✅ Multiple roles:

```csharp
[Authorize(Roles = "Admin,Moderator")]
```

---

# 6. Policy-Based Authorization (advanced)

✅ Define a policy:

```csharp
builder.Services.AddAuthorization(options =>
{
    options.AddPolicy("MustBeAdmin", policy =>
        policy.RequireRole("Admin"));
});
```

✅ Apply it:

```csharp
[Authorize(Policy = "MustBeAdmin")]
public IActionResult AdminDashboard()
{
    return View();
}
```

---

# 7. Custom Claims (Super advanced 🔥)

✅ Add a Claim during registration:

```csharp
await userManager.AddClaimAsync(user, new Claim("Department", "IT"));
```

✅ Authorize by claim:

```csharp
[Authorize(Policy = "ITOnly")]
public IActionResult ITDashboard() => View();
```

✅ Define policy:

```csharp
builder.Services.AddAuthorization(options =>
{
    options.AddPolicy("ITOnly", policy =>
        policy.RequireClaim("Department", "IT"));
});
```

---

# 8. Redirect to Login Automatically

✅ Configure where unauthenticated users are redirected:

```csharp
builder.Services.ConfigureApplicationCookie(options =>
{
    options.LoginPath = "/Identity/Account/Login";
    options.AccessDeniedPath = "/Identity/Account/AccessDenied";
});
```

---

# 🎯 Key Attributes Quick Table

| Attribute | Meaning |
|:---|:---|
| `[Authorize]` | Require login |
| `[Authorize(Roles = "Admin")]` | Require login AND "Admin" role |
| `[AllowAnonymous]` | Allow access without login |
| `[Authorize(Policy = "PolicyName")]` | Require custom policy (role, claim, etc.) |

---

# 🔥 Super Quick Summary

| Step | Action |
|:---|:---|
| Add Identity | `AddDefaultIdentity` in `Program.cs` |
| Use Authentication | `app.UseAuthentication()` |
| Protect Routes | `[Authorize]` |
| Setup Roles | `AddRoles<IdentityRole>()`, `SeedRoles()` |
| Create Policies | `options.AddPolicy()` |

---

# Bonus Tip 🚀
Always use `[Authorize]` on **Controllers** if you don't want random users accessing private stuff!  
And combine `[Authorize]` + Bootstrap + Toastr for showing **"Access Denied"** in a pretty way.

---

Would you also want me to create an "**Admin Panel Starter Template**" — with Role-based Authorization, nice navbar showing Login/Logout dynamically, etc.?  
(You could just paste it and start building ✨)  

**Should I prepare it?** 🎯