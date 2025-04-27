Alright!  
Here’s a **super clean, powerful MVC cheat sheet** for .NET 6 — **including form creation, validation, and error handling** 🔥:

---

# 📄 ASP.NET Core MVC Cheat Sheet (.NET 6)

---

## 1. What is MVC?

- **Model** — your data (C# classes).
- **View** — your UI (HTML + Razor).
- **Controller** — your logic (handle requests).

✅ Best for **bigger web apps** needing **separation of concerns**.

---

## 2. Create a New MVC Project

```bash
dotnet new mvc --name MyMvcApp
```

✅ This gives you Controllers, Models, Views folders out of the box.

---

## 3. Basic Structure Example

```
/Controllers
    ProductController.cs
/Models
    Product.cs
/Views
    /Product
        Index.cshtml
        Create.cshtml
/Views/Shared
    _Layout.cshtml
```

---

# ⚙️ Working Example (Creating Products)

---

## 4. Model (with Validation)

```csharp
using System.ComponentModel.DataAnnotations;

public class Product
{
    public int Id { get; set; }

    [Required(ErrorMessage = "Name is required.")]
    [StringLength(100, ErrorMessage = "Max 100 characters.")]
    public string Name { get; set; }

    [Range(0.01, 10000.00, ErrorMessage = "Price must be between 0.01 and 10000.")]
    public decimal Price { get; set; }
}
```

✅ **Validation** uses `[Required]`, `[StringLength]`, `[Range]`, etc.

---

## 5. Controller

```csharp
using Microsoft.AspNetCore.Mvc;

public class ProductController : Controller
{
    // GET: /Product
    public IActionResult Index()
    {
        return View();
    }

    // GET: /Product/Create
    public IActionResult Create()
    {
        return View();
    }

    // POST: /Product/Create
    [HttpPost]
    [ValidateAntiForgeryToken]
    public IActionResult Create(Product product)
    {
        if (ModelState.IsValid)
        {
            // Save to database (not shown here)
            return RedirectToAction("Index");
        }

        // If validation fails, redisplay the form
        return View(product);
    }
}
```

- `ModelState.IsValid` checks if model passes validation.

- `[ValidateAntiForgeryToken]` = protect from CSRF attacks.

---

## 6. View (Form)

### `/Views/Product/Create.cshtml`

```html
@model Product

<h2>Create Product</h2>

<form asp-action="Create" method="post">
    @Html.AntiForgeryToken()

    <div>
        <label asp-for="Name"></label>
        <input asp-for="Name" />
        <span asp-validation-for="Name" class="text-danger"></span>
    </div>

    <div>
        <label asp-for="Price"></label>
        <input asp-for="Price" />
        <span asp-validation-for="Price" class="text-danger"></span>
    </div>

    <button type="submit">Save</button>
</form>

@section Scripts {
    <partial name="_ValidationScriptsPartial" />
}
```

✅ Notes:
- `asp-for` binds form inputs to model properties.
- `asp-validation-for` shows validation errors.
- `_ValidationScriptsPartial` enables **client-side validation** using JavaScript.

---

## 7. Enable Validation Scripts

Make sure you have this line inside your `_Layout.cshtml` before `</body>`:

```html
<partial name="_ValidationScriptsPartial" />
```

✅ This adds client-side (browser) validation automatically.

---

## 8. Common Validation Attributes

| Attribute | Purpose |
|:---|:---|
| `[Required]` | Must not be empty |
| `[StringLength(max)]` | Limit string length |
| `[Range(min, max)]` | Numeric range |
| `[EmailAddress]` | Must be a valid email |
| `[Compare("OtherProperty")]` | Compare two fields (e.g., Password and ConfirmPassword) |
| `[RegularExpression("pattern")]` | Match regex |

---

# 📢 Important

| Concept | Code |
|:---|:---|
| Check for validation errors | `ModelState.IsValid` |
| Display errors | `asp-validation-for` |
| Protect against CSRF | `@Html.AntiForgeryToken()` + `[ValidateAntiForgeryToken]` |
| Redirect after success | `RedirectToAction("ActionName")` |
| Stay on form if error | `return View(model);` |

---

# 🚀 Full Life Cycle of a POST Form in MVC:

1. User visits `/Product/Create`.
2. Form shows up (GET).
3. User fills the form and clicks Submit.
4. Form sends data to `Create(Product product)` (POST).
5. **If ModelState is valid**:
   - Save data.
   - Redirect to Index page.
6. **If ModelState is invalid**:
   - Re-render form with validation messages.

---

# 💬 Bonus Tip: Show all errors

If you want to list **all** errors together:

```html
@foreach (var error in ViewData.ModelState.Values.SelectMany(v => v.Errors))
{
    <div class="text-danger">@error.ErrorMessage</div>
}
```

---

# 📜 Summary Table

| Area | Key Command/Idea |
|:---|:---|
| Create new MVC project | `dotnet new mvc` |
| Form input binding | `asp-for` |
| Field validation | `[Required]`, `[Range]`, etc. |
| Validate on server | `ModelState.IsValid` |
| Display field error | `asp-validation-for` |
| CSRF protection | `@Html.AntiForgeryToken()` |
| Client-side validation | Include `_ValidationScriptsPartial` |
| Redirect after success | `RedirectToAction("Index")` |

---

Would you also want a **Mini CRUD MVC example** (Create + Read + Update + Delete Products) to practice everything at once? 🔥  
I can prepare that quickly if you want! 🚀