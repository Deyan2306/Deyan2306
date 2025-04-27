Perfect, let's go directly to the point:

---

# 🎯 How to Use Model Validation in ASP.NET Core Controllers (MVC / Razor Pages)

---

## 1. ✅ Add Validation Attributes to Your Model

Use built-in attributes like `[Required]`, `[StringLength]`, `[Range]`, etc.

Example:

```csharp
using System.ComponentModel.DataAnnotations;

public class UserViewModel
{
    [Required]
    [StringLength(50)]
    public string Name { get; set; }

    [Required]
    [EmailAddress]
    public string Email { get; set; }

    [Range(18, 99)]
    public int Age { get; set; }
}
```

---

## 2. 🎯 Validate the Model Inside Controller

When the form is submitted, check `ModelState.IsValid`:

```csharp
[HttpPost]
public IActionResult Create(UserViewModel model)
{
    if (!ModelState.IsValid)
    {
        // Model is invalid → return back to the form with error messages
        return View(model);
    }

    // Model is valid → continue with saving to the database
    // _context.Users.Add(...);
    // _context.SaveChanges();

    return RedirectToAction("Index");
}
```

✅ **If invalid**, you return the same view and errors are shown.  
✅ **If valid**, you continue with your business logic (DB save, redirect, etc.).

---

## 3. 🖌️ Show Validation Errors in Your View

Inside your `.cshtml` Razor View:

```html
<form asp-action="Create" method="post">
    <div asp-validation-summary="All" class="text-danger"></div>

    <div class="form-group">
        <label asp-for="Name"></label>
        <input asp-for="Name" class="form-control" />
        <span asp-validation-for="Name" class="text-danger"></span>
    </div>

    <div class="form-group">
        <label asp-for="Email"></label>
        <input asp-for="Email" class="form-control" />
        <span asp-validation-for="Email" class="text-danger"></span>
    </div>

    <div class="form-group">
        <label asp-for="Age"></label>
        <input asp-for="Age" class="form-control" />
        <span asp-validation-for="Age" class="text-danger"></span>
    </div>

    <button type="submit" class="btn btn-primary">Submit</button>
</form>
```

✅ This automatically displays validation errors if `ModelState` is invalid.

---

## 4. 🚀 Add Client-Side Validation

In your `_Layout.cshtml`, **include** these scripts for automatic browser-side validation:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-validate/1.19.3/jquery.validate.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-validation-unobtrusive/4.0.0/jquery.validate.unobtrusive.min.js"></script>
```

✅ Now your forms will also validate **on the client side** (browser) before even hitting the server!

---

# 🧠 Quick Summary Table

| Step | What to Do |
|:---|:---|
| 1. Add validation attributes | `[Required]`, `[StringLength]`, `[Range]`, etc. on your model |
| 2. Check `ModelState.IsValid` | In your POST action inside the controller |
| 3. Show errors in view | Use `asp-validation-for` and `asp-validation-summary` |
| 4. Enable client-side validation | Add jQuery validation scripts |

---

# ⚡ Bonus Tip
You can **add custom validation attributes** by extending `ValidationAttribute` class if you need advanced rules.

Example:

```csharp
public class MustBeTrueAttribute : ValidationAttribute
{
    public override bool IsValid(object value)
    {
        return value is bool b && b;
    }
}
```

---

Would you also want me to make a super short "**Custom Server Validation**" cheatsheet too? (like `ModelState.AddModelError`) ✅  
It’s useful for things like checking if a username is already taken dynamically. 🚀  
Would you like it?