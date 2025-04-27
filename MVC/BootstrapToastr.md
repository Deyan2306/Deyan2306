Got you!  
Here’s a **clean cheat sheet** for **Bootstrap + Toastr** inside **ASP.NET Core MVC** apps 🔥:

---

# 🎨 Bootstrap + Toastr Cheat Sheet for ASP.NET Core MVC (.NET 6)

---

# 1. Add Bootstrap and Toastr

✅ Inside your `_Layout.cshtml` (`Views/Shared/_Layout.cshtml`):

👉 In `<head>`, add:

```html
<!-- Bootstrap CSS -->
<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet" />

<!-- Toastr CSS -->
<link href="https://cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.css" rel="stylesheet" />
```

👉 Before `</body>`, add:

```html
<!-- jQuery (required for Toastr) -->
<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>

<!-- Bootstrap JS -->
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"></script>

<!-- Toastr JS -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/toastr.js/latest/toastr.min.js"></script>
```

✅ Now you can use both Bootstrap **and** Toastr in your project.

---

# 2. Show Toast Notifications with Toastr

### In a Razor View (`.cshtml`):

```html
@section Scripts {
    <script>
        toastr.success('Operation successful!');
        toastr.error('Something went wrong.');
        toastr.info('Here is some info.');
        toastr.warning('This is a warning.');
    </script>
}
```

---

# 3. Customizing Toastr Options

```html
<script>
    toastr.options = {
      "closeButton": true,
      "progressBar": true,
      "positionClass": "toast-top-right", // Change to "toast-bottom-left", etc.
      "timeOut": "3000"
    };

    toastr.success('Custom Toast!');
</script>
```

---

# 4. Trigger Toastr After Server Actions

✅ In your **Controller**, use `TempData` to send a message:

```csharp
public IActionResult Create()
{
    TempData["SuccessMessage"] = "Product created successfully!";
    return RedirectToAction("Index");
}
```

✅ In your View (`Views/Product/Index.cshtml`):

```html
@section Scripts {
    <script>
        @if (TempData["SuccessMessage"] != null)
        {
            <text>
                toastr.success('@TempData["SuccessMessage"]');
            </text>
        }
    </script>
}
```

✅ This way, **you show toasts automatically after POST-redirect-GET**.

---

# 5. Using Bootstrap for Forms, Buttons, Layouts

### Example: Create Form with Bootstrap styling

```html
@model Product

<h2>Create Product</h2>

<form asp-action="Create" method="post" class="needs-validation" novalidate>
    @Html.AntiForgeryToken()

    <div class="mb-3">
        <label asp-for="Name" class="form-label"></label>
        <input asp-for="Name" class="form-control" />
        <span asp-validation-for="Name" class="text-danger"></span>
    </div>

    <div class="mb-3">
        <label asp-for="Price" class="form-label"></label>
        <input asp-for="Price" class="form-control" />
        <span asp-validation-for="Price" class="text-danger"></span>
    </div>

    <button type="submit" class="btn btn-primary">Save</button>
</form>

@section Scripts {
    <partial name="_ValidationScriptsPartial" />
}
```

✅ Use:
- `form-control` for inputs
- `btn btn-primary` for buttons
- `text-danger` for errors

---

# 6. Quick Toastr Usage Summary

| Toastr Type | Code |
|:---|:---|
| Success | `toastr.success('Done!');` |
| Error | `toastr.error('Oops!');` |
| Info | `toastr.info('FYI!');` |
| Warning | `toastr.warning('Careful!');` |

---

# 7. Bonus: Toast for ModelState Errors

✅ If you want to toast **server-side validation errors**:

```csharp
if (!ModelState.IsValid)
{
    TempData["ErrorMessage"] = "Please fix the form errors.";
    return View(model);
}
```

And in your View:

```html
@section Scripts {
    <script>
        @if (TempData["ErrorMessage"] != null)
        {
            <text>
                toastr.error('@TempData["ErrorMessage"]');
            </text>
        }
    </script>
}
```

---

# 🔥 Summary Table

| Feature | How |
|:---|:---|
| Add Bootstrap | CDN in `<head>` and JS before `</body>` |
| Add Toastr | CDN in `<head>` and JS before `</body>` |
| Trigger Toastr | `toastr.success('msg')` in JS |
| Toast after Action | `TempData` + Razor script |
| Bootstrap form styles | `form-control`, `btn`, `form-label` |

---

# 🎯 How They Work Together

- Use **Bootstrap** to style your **pages and forms**.
- Use **Toastr** to show **pop-up notifications** after **server actions**.

---

Would you also want me to make a **ready-to-paste Starter Template** that already has Bootstrap, Toastr, a nice form, and ready scripts?  
(You could just copy-paste it and start immediately 🚀)  
**Should I prepare it?** 🔥