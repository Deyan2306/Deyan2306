Of course! Here's a **compact but complete Entity Framework Core (.NET 6) cheatsheet**, focused on **creating and working with databases**:

---

# 📚 Entity Framework Core Cheat Sheet (.NET 6)

---

## 1. Install EF Core packages

```bash
dotnet add package Microsoft.EntityFrameworkCore
dotnet add package Microsoft.EntityFrameworkCore.Design
dotnet add package Microsoft.EntityFrameworkCore.SqlServer
```

(Replace `SqlServer` with `Npgsql` for PostgreSQL, `Sqlite`, etc.)

---

## 2. Define Your **DbContext**

```csharp
using Microsoft.EntityFrameworkCore;

public class AppDbContext : DbContext
{
    public DbSet<User> Users { get; set; }  // Table

    protected override void OnConfiguring(DbContextOptionsBuilder options)
        => options.UseSqlServer("Your_Connection_String");
}
```

---

## 3. Define Your **Entities (Models)**

```csharp
public class User
{
    public int Id { get; set; }
    public string Name { get; set; }
}
```

---

## 4. Migrations — **Create/Update Database Schema**

> 📦 Migrations are version control for your database schema!

- **Add initial migration**:

```bash
dotnet ef migrations add InitialCreate
```

- **Apply migrations to database**:

```bash
dotnet ef database update
```

- **Remove a migration**:

```bash
dotnet ef migrations remove
```

- **Drop the database**:

```bash
dotnet ef database drop
```

---

## 5. Basic **CRUD** Operations

```csharp
var context = new AppDbContext();

// 🔵 CREATE
var user = new User { Name = "Alice" };
context.Users.Add(user);
context.SaveChanges();

// 🟡 READ
var users = context.Users.ToList();
var userById = context.Users.Find(1);  // Find by Primary Key

// 🟠 UPDATE
var updateUser = context.Users.Find(1);
updateUser.Name = "Bob";
context.SaveChanges();

// 🔴 DELETE
var deleteUser = context.Users.Find(1);
context.Users.Remove(deleteUser);
context.SaveChanges();
```

---

## 6. **Querying** Data (LINQ + EF)

```csharp
var users = context.Users
    .Where(u => u.Name.StartsWith("A"))
    .OrderBy(u => u.Name)
    .ToList();
```

- **Select Specific Fields**:

```csharp
var names = context.Users
    .Select(u => u.Name)
    .ToList();
```

- **Include Related Data** (Eager Loading):

```csharp
var usersWithPosts = context.Users
    .Include(u => u.Posts)
    .ToList();
```

---

## 7. **Relationships**

### One-to-Many

```csharp
public class User
{
    public int Id { get; set; }
    public string Name { get; set; }
    public List<Post> Posts { get; set; } = new();
}

public class Post
{
    public int Id { get; set; }
    public string Title { get; set; }
    public int UserId { get; set; }
    public User User { get; set; }
}
```

---

## 8. Fluent API (Advanced Configurations)

```csharp
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<User>()
        .Property(u => u.Name)
        .IsRequired()
        .HasMaxLength(50);

    modelBuilder.Entity<Post>()
        .HasOne(p => p.User)
        .WithMany(u => u.Posts)
        .HasForeignKey(p => p.UserId);
}
```

---

## 9. Connection String Example (SQL Server)

```plaintext
Server=(localdb)\mssqllocaldb;Database=MyAppDb;Trusted_Connection=True;
```

---

## 10. Tips:

- **`SaveChanges()`** commits ALL pending changes.
- **Use `async/await`** for production:

```csharp
await context.SaveChangesAsync();
var users = await context.Users.ToListAsync();
```

- **Migration commands** must be run from the project directory containing the `DbContext`.

---

# 🚀 Bonus Commands:

| Command                               | Description                       |
|---------------------------------------|-----------------------------------|
| `dotnet ef migrations list`           | List all migrations              |
| `dotnet ef dbcontext info`            | Show DbContext info              |
| `dotnet ef dbcontext scaffold`        | Generate models from existing DB |

---

Would you also like a **visual diagram** of the EF Core workflow (DbContext ➔ Migrations ➔ Database)? 🎨  
It can make it even easier to memorize! 🚀