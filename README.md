# BU CAS CS320 Concepts of Programming Languages.

This is public repository used for publishing assignments and lecture materials.

## Mirroring this repository

You will first create a private repository that mirrors this one. The following instructions describe how to do so.

### Step 1:

Please clone the public class repository:
```
git clone https://github.com/qcfu-bu/cs320-spring-2024
```

### Step 2:

Please create a *private* repository of your own on GitHub. For instance, the following one is created for my own use:

https://github.com/qcfu-bu/cs320-spring-2024-private

Then mirror-push the class repo into your own private repo:
```
git -C ./cs320-spring-2024 push --mirror https://github.com/qcfu-bu/cs320-spring-2024-private
```

### Step 3:

Clone your private repository. All the work you do for this course should be done in your private repository.
```
git clone https://github.com/qcfu-bu/cs320-spring-2024-private
```

### Step 4:

Add the *public repo* as a remote for your private repo.
```
git -C ./cs320-spring-2024-private remote add upstream https://github.com/qcfu-bu/cs320-spring-2024
```

### Step 5 (Optional):
At this point, you may delete the cloned public repo as it is no longer needed.
```
rm -rf cs320-spring-2024
```


## Syncing your private mirror with this repository

Remember to sync with the class repo frequently. To sync with the public repo, run the following commands inside your `cs320-spring-2024-private` directory.
```
git fetch upstream
git merge upstream/main main
```

Updates made to the public repository will now be available on your computer. However, these changes will not appear on your GitHub yet. Run the following command to upload these changes to your GitHub.
```
git push
```
