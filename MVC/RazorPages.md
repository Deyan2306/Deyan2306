Of course!  
Here's a **very clean, compact, and complete Razor Pages cheatsheet** for .NET 6:  

---

# 📄 Razor Pages Cheat Sheet (.NET 6)

---

## 1. What is Razor Pages?
- Razor Pages = **MVC without Controllers**.
- Each `.cshtml` page **has its own model (`PageModel`)** for handling requests.

Good for: **Simple websites, admin panels, CRUD apps.**

---

## 2. Project Structure

```
/Pages
    /Index.cshtml
    /Index.cshtml.cs
    /Products
        /Create.cshtml
        /Create.cshtml.cs
    /Shared
        /_Layout.cshtml
```

- `.cshtml` = HTML + C# (the View).
- `.cshtml.cs` = C# PageModel class (the logic).

---

## 3. Creating a Razor Page

```bash
dotnet new razor --name MyRazorApp
```

✅ This creates a Razor Pages app.

---

## 4. Example Razor Page

### `/Pages/Index.cshtml`

```html
@page
@model IndexModel

<h1>Hello, @Model.Message!</h1>
```

---

### `/Pages/Index.cshtml.cs`

```csharp
using Microsoft.AspNetCore.Mvc.RazorPages;

public class IndexModel : PageModel
{
    public string Message { get; set; }

    public void OnGet()
    {
        Message = "World";
    }
}
```

- `OnGet()` → called when page is visited.
- `OnPost()` → called when form is submitted (POST request).

---

## 5. Handling Forms

### Form in Razor

```html
@page
@model CreateModel

<form method="post">
    <input type="text" asp-for="Name" />
    <button type="submit">Create</button>
</form>
```

---

### PageModel for the Form

```csharp
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;

public class CreateModel : PageModel
{
    [BindProperty]
    public string Name { get; set; }

    public void OnGet() { }

    public IActionResult OnPost()
    {
        if (!ModelState.IsValid)
            return Page();

        // Save Name to database
        return RedirectToPage("/Index");
    }
}
```

- `asp-for="Name"` binds input to C# property.
- `[BindProperty]` links form fields to model properties.
- `OnPost()` handles form submission.

---

## 6. Routing Razor Pages

- By default, URL matches the folder and filename.
- Customize route:

```html
@page "/custom-route"
```

Now `/custom-route` points to this page.

---

## 7. Redirecting

```csharp
return RedirectToPage("/Products/Index");
return RedirectToPage("./Details", new { id = 5 });
return RedirectToAction("Index", "Home"); // if you mix with MVC
```

---

## 8. TempData & ViewData

- **TempData** survives **one redirect**.
- **ViewData** only exists for the current page.

```csharp
TempData["Success"] = "Product created!";
ViewData["Title"] = "Products";
```

```html
<h1>@TempData["Success"]</h1>
<h2>@ViewData["Title"]</h2>
```

---

## 9. Layout (_Layout.cshtml)

Shared layout for all pages:

```html
<!DOCTYPE html>
<html>
<head>
    <title>@ViewData["Title"]</title>
</head>
<body>
    <main>@RenderBody()</main>
</body>
</html>
```

Set layout in your page:

```csharp
@page
@model IndexModel
@{
    Layout = "_Layout";
}
```

---

## 10. Dependency Injection (inject services)

```csharp
@inject ILogger<IndexModel> Logger
```

or in PageModel:

```csharp
private readonly ILogger<IndexModel> _logger;

public IndexModel(ILogger<IndexModel> logger)
{
    _logger = logger;
}
```

---

# 🚀 Quick Razor Syntax Reminders:

| Syntax | Meaning |
|:---|:---|
| `@{ var x = 5; }` | Inline C# code block |
| `@Model.Property` | Access C# model |
| `@foreach (var item in Model.Items) {}` | C# loops |
| `@Html.DisplayFor(m => m.Name)` | Display model field |
| `asp-for="Property"` | Bind input field to model |
| `asp-page="/Create"` | Generate link to another Razor Page |

---

# 📌 Life Cycle of a Razor Page

| Event | Method |
|:---|:---|
| GET request | `OnGet()` |
| POST request | `OnPost()` |
| GET (Async) | `OnGetAsync()` |
| POST (Async) | `OnPostAsync()` |

---

Would you also like me to create a **Mini CRUD example** (like a basic "Products" create/read/update/delete) using Razor Pages? 🚀  
It'll be super helpful to tie everything together! 🎯