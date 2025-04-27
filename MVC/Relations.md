Perfect question!  
Here’s a **clear and simple guide** for setting up **1:1**, **1:N**, and **M:N** relationships in **Entity Framework Core**:

---

# ⚡ Relationships in EF Core (.NET 6)

---

## 1️⃣ One-to-One (1:1)

Example: Each `User` has **one** `Profile`.

### Entities:

```csharp
public class User
{
    public int Id { get; set; }
    public string Name { get; set; }
    
    public Profile Profile { get; set; }  // Navigation property
}

public class Profile
{
    public int Id { get; set; }
    public string Bio { get; set; }
    
    public int UserId { get; set; }
    public User User { get; set; }
}
```

---

### Fluent API Setup (Optional, but better):

```csharp
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<User>()
        .HasOne(u => u.Profile)
        .WithOne(p => p.User)
        .HasForeignKey<Profile>(p => p.UserId);
}
```

---

## 2️⃣ One-to-Many (1:N)

Example: One `User` has **many** `Posts`.

### Entities:

```csharp
public class User
{
    public int Id { get; set; }
    public string Name { get; set; }
    
    public List<Post> Posts { get; set; } = new();  // Navigation property
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

### Fluent API Setup (Optional):

```csharp
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<Post>()
        .HasOne(p => p.User)
        .WithMany(u => u.Posts)
        .HasForeignKey(p => p.UserId);
}
```

---

## 3️⃣ Many-to-Many (M:N)

Example: A `Student` can enroll in many `Courses`, and a `Course` can have many `Students`.

👉 In EF Core 5+, you don't need to manually create a join table — **EF Core creates it automatically!**

### Entities:

```csharp
public class Student
{
    public int Id { get; set; }
    public string Name { get; set; }
    
    public List<Course> Courses { get; set; } = new();
}

public class Course
{
    public int Id { get; set; }
    public string Title { get; set; }
    
    public List<Student> Students { get; set; } = new();
}
```

---

### Fluent API Setup (Optional):

```csharp
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<Student>()
        .HasMany(s => s.Courses)
        .WithMany(c => c.Students);
}
```

✅ EF will automatically create a join table like `StudentCourse` behind the scenes.

---

# 📌 Summary Table:

| Relation | Entities Definition | Fluent API | Notes |
|:---|:---|:---|:---|
| 1:1 | Nav prop in both classes | `.HasOne().WithOne()` | One owns another |
| 1:N | List in "one" side, ref in "many" side | `.HasOne().WithMany()` | Classic foreign key |
| M:N | Lists on both sides | `.HasMany().WithMany()` | Auto-generated join table |

---

Would you also want **quick code snippets** for **seeding data** with these relationships?  
(Example: adding a `User` + `Profile` + `Posts` in one shot) 🚀✨